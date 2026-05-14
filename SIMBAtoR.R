# to import movement data from sleap/simba into R
library(data.table)
library(tidyverse)

file_path_mid <- "C:/Users/bdy2530/Desktop/simba-activeavoidance-proj/activeavoidance/project_folder/logs/Time_bins_0.1s_movement_results_20260512153226.csv"
file_path_nose <- "C:/Users/bdy2530/Desktop/simba-activeavoidance-proj/activeavoidance/project_folder/logs/Time_bins_0.1s_movement_results_20260513123031.csv"

process_simba_csv <- function(path, value_col_name) {
  dt <- fread(path)
  dt <- dt[MEASUREMENT == "Velocity (cm/s)"]
  
  threshold <- quantile(dt$VALUE, 0.99, na.rm = TRUE)
  dt[VALUE > threshold, VALUE := NA]
  
  # Keep only essential columns to save memory, and rename VALUE
  dt <- dt[, .(VIDEO, `TIME BIN #`, VALUE)]
  setnames(dt, "VALUE", value_col_name)
  return(dt)
}

mid_df <- process_simba_csv(file_path_mid, "Velocity_mid")
nose_df <- process_simba_csv(file_path_nose, "Velocity_nose")

velocity_df <- merge(mid_df, nose_df, by = c("VIDEO", "TIME BIN #"), all = TRUE)
velocity_df[, Time_sec := `TIME BIN #` * 0.1]

velocity_df <- as_tibble(velocity_df)

# read Rds files
meta_data <- readRDS("Active.Avoidance_meta_Data.Rds")
operant_data <- readRDS("Active.Avoidance_Operant_Data.Rds")

# match video names to metadata ----
unique_videos <- velocity_df %>%
  distinct(VIDEO) %>%
  mutate(
    Box = as.numeric(str_extract(VIDEO, "(?<=Box)\\d+")),
    Vid_DateTime_str = str_extract(VIDEO, "(?<=Box\\d_).*"),
    Vid_DateTime = as.POSIXct(Vid_DateTime_str, format = "%d%b%Y_%H-%M-%S") 
  )
meta_data <- meta_data %>%
  mutate(
    Box = as.numeric(Box),
    DateTime = as.POSIXct(DateTime) 
  )
mapped_videos <- unique_videos %>%
  left_join(
    meta_data %>% select(Box, DateTime, Subject, Day, Paradigm), 
    by = "Box", 
    relationship = "many-to-many"
  ) %>%
  mutate(
    TimeDiff_Mins = abs(as.numeric(difftime(Vid_DateTime, DateTime, units = "mins")))
  ) %>%
  # SAFEGUARD 1: Max allowed offset. 
  # If the closest session is >30 mins away, the true session was probably deleted.
  filter(TimeDiff_Mins <= 10) %>%
  # SAFEGUARD 2: For each Video, keep only its best Med-PC session match
  group_by(VIDEO) %>%
  slice_min(TimeDiff_Mins, n = 1) %>%
  # SAFEGUARD 3: For each Med-PC session (Subject+Day), keep only its best Video match
  group_by(Subject, Day) %>%
  slice_min(TimeDiff_Mins, n = 1) %>%
  ungroup() %>%
  select(VIDEO, Subject, Day, Paradigm)

velocity_df <- velocity_df %>%
  left_join(mapped_videos, by = "VIDEO") %>%
  # Drop videos that couldn't be matched (e.g. subjects you excluded)
  filter(!is.na(Subject))

fwrite(velocity_df, "cleaned_velocity_data.csv")


# freezing calculations/criteria ------
THRESH_MID <- 3 # cm/s
THRESH_NOSE <- 6.6 # cm/s (adjust if it feels too loose, gaby did ~6.6)

velocity_df <- velocity_df %>%
  distinct(VIDEO, Time_sec, .keep_all = TRUE) %>%
  arrange(VIDEO, Time_sec)


velocity_df <- velocity_df %>%
  group_by(VIDEO) %>%
  mutate(
    # MUST meet both criteria to be considered "under threshold" for freezing
    is_under_thresh = coalesce((Velocity_mid < THRESH_MID) & (Velocity_nose < THRESH_NOSE), FALSE),
    streak_id = consecutive_id(is_under_thresh)
  ) %>%
  group_by(VIDEO, streak_id) %>%
  mutate(
    streak_len = n(),
    is_freezing = is_under_thresh & (streak_len >= 15) # 1.5 seconds
  ) %>%
  ungroup() %>%
  select(-is_under_thresh, -streak_id, -streak_len)

# ITI freezing ----
iti_data <- operant_data %>%
  filter(name %in% c("Cue", "Cue.End")) %>%
  group_by(Subject, Day, Trial, name) %>%
  summarize(value = first(value), .groups = "drop") %>%
  pivot_wider(names_from = name, values_from = value) %>%
  arrange(Subject, Day, Trial) %>%
  group_by(Subject, Day) %>%
  mutate(
    ITI_Start = Cue.End,
    ITI_End = lead(Cue) 
  ) %>%
  ungroup() %>%
  filter(!is.na(ITI_End)) %>%
  select(Subject, Day, Trial, ITI_Start, ITI_End)

iti_velocity <- velocity_df %>%
  inner_join(
    iti_data, 
    join_by(Subject, Day, Time_sec >= ITI_Start, Time_sec < ITI_End)
  ) %>%
  distinct(Subject, Day, Trial, Time_sec, .keep_all = TRUE)

iti_freezing_summary <- iti_velocity %>%
  group_by(Subject, Day, Trial) %>%
  summarize(
    Total_ITI_Duration_sec = n() * 0.1,
    Freezing_Duration_sec = sum(is_freezing, na.rm = TRUE) * 0.1,
    Pct_Freezing = (Freezing_Duration_sec / Total_ITI_Duration_sec) * 100,
    .groups = "drop"
  )


# cue freezing ----
cue_data <- operant_data %>%
  filter(name %in% c("Cue", "Cue.End")) %>%
  group_by(Subject, Day, Trial, name) %>%
  summarize(value = first(value), .groups = "drop") %>%
  pivot_wider(names_from = name, values_from = value) %>%
  arrange(Subject, Day, Trial) %>%
  # We just use Cue and Cue.End from the same trial!
  filter(!is.na(Cue) & !is.na(Cue.End)) %>%
  select(Subject, Day, Trial, Cue_Start = Cue, Cue_End = Cue.End)

cue_velocity <- velocity_df %>%
  inner_join(
    cue_data, 
    join_by(Subject, Day, Time_sec >= Cue_Start, Time_sec < Cue_End)
  ) %>%
  distinct(Subject, Day, Trial, Time_sec, .keep_all = TRUE)

cue_freezing_summary <- cue_velocity %>%
  group_by(Subject, Day, Trial) %>%
  summarize(
    Total_Cue_Duration_sec = n() * 0.1,
    Freezing_During_Cue_sec = sum(is_freezing, na.rm = TRUE) * 0.1,
    Pct_Freezing_Cue = (Freezing_During_Cue_sec / Total_Cue_Duration_sec) * 100,
    .groups = "drop"
  )


# plotting ----
freezing_combined <- cue_freezing_summary %>%
  left_join(iti_freezing_summary, by = c("Subject", "Day", "Trial"))

subject_daily_freezing <- freezing_combined %>%
  group_by(Subject, Day) %>%
  summarize(
    Cue = mean(Pct_Freezing_Cue, na.rm = TRUE),
    ITI = mean(Pct_Freezing, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Pivot to long format for easier plotting with ggplot2
  pivot_longer(cols = c(Cue, ITI), names_to = "Period", values_to = "Pct_Freezing")

group_daily_freezing <- subject_daily_freezing %>%
  group_by(Day, Period) %>%
  summarize(
    Mean_Freezing = mean(Pct_Freezing, na.rm = TRUE),
    SEM = sd(Pct_Freezing, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

freezing_plot <- ggplot(group_daily_freezing, aes(x = Day, y = Mean_Freezing, color = Period, group = Period)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Mean_Freezing - SEM, ymax = Mean_Freezing + SEM), width = 0.2, linewidth = 0.8) +
  scale_color_manual(values = c("Cue" = "#D55E00", "ITI" = "#0072B2")) + # Colorblind-friendly colors
  scale_x_continuous(breaks = min(group_daily_freezing$Day):max(group_daily_freezing$Day)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  labs(
    title = "Day-by-Day Freezing: Cue vs ITI",
    x = "Training Day",
    y = "Freezing (%)",
    color = "Period"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "top",
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black")
  )
print(freezing_plot)

# save to prism ----
prism_cue <- subject_daily_freezing %>%
  filter(Period == "Cue") %>%
  select(-Period) %>%
  # Make Subjects the columns and populate with Pct_Freezing
  pivot_wider(names_from = Subject, names_prefix = "Sub_", values_from = Pct_Freezing) %>%
  arrange(Day) # Order rows by Day (1 to 7)

fwrite(prism_cue, "Prism_Cue_Freezing.csv")

prism_iti <- subject_daily_freezing %>%
  filter(Period == "ITI") %>%
  select(-Period) %>%
  pivot_wider(names_from = Subject, names_prefix = "Sub_", values_from = Pct_Freezing) %>%
  arrange(Day)

fwrite(prism_iti, "Prism_ITI_Freezing.csv")
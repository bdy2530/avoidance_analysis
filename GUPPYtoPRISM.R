library(rhdf5)
library(tidyverse)
library(progress)
library(googlesheets4)
library(ggrepel)
library(glue)
library(lubridate)
library(broom)
library(purrr)
library(ggprism)
gs4_deauth()

# Setup stuff -----------------------------------------------------------
Experiment <- "Active.Avoidance"

meta.data.link <- "https://docs.google.com/spreadsheets/d/1VPx4tm8clOViS7U8v_KSBg8S7oL2NFnLAkSvROfv-6o/edit?usp=sharing"
meta.data <- read_sheet(meta.data.link) %>%
  mutate(Subject = as.numeric(Subject)) %>% 
  rowwise() %>% 
  dplyr::select(Subject, Group, Sex)

Operant_Data <- readRDS(file = glue("{Experiment}_Operant_Data.Rds"))
Operant_Data <- Operant_Data %>% 
  mutate(Subject = as.numeric(as.character(Subject)),  # Convert factor/character to numeric properly
         Date = date(DateTime))
Operant_metadata <- Operant_Data %>% 
  mutate(Date = date(DateTime)) %>% 
  dplyr::select(Subject, Date, Paradigm.Day) %>% 
  distinct() %>% 
  arrange(Subject, Paradigm.Day) %>% 
  left_join(meta.data) 

timeaxis_h5 <- readRDS(file = glue("{Experiment}_timeaxis_h5.Rds"))
Zmeans <- readRDS(file = glue("{Experiment}_Operant_Data.Zmeans.Rds"))

#subset time axis by index timeaxis_subsample_index - downsample by 20 for plotting
timeaxis_subsample_index <- seq(10,length(timeaxis_h5),50) %>% as.integer()
timeaxis <- timeaxis_h5[timeaxis_subsample_index]
#formatting with rownumbers/samples:
timeaxisTable <- tibble(Time = timeaxis) %>%
  dplyr::mutate(TimeKey = row_number()) %>%
  relocate(TimeKey)

forGroups <- meta.data %>%
  dplyr::select(Subject, Group, Sex) %>%
  distinct()

Zmeans <- Zmeans %>% 
  mutate(Subject = as.numeric(as.character(Subject)))

Zmeans <-  Zmeans %>% 
  filter(!str_detect(Event, "timer")) %>% 
  left_join(Operant_metadata, by = c("Subject", "Date")) %>% select(!Zerr) %>% select(!Group)


Zmeans_unpack <- Zmeans %>%
  rename(zScore = Zmean) %>%
  rowwise() %>%
  mutate(plotParadigm = Paradigm.Day, #plotParaName(Paradigm),
         zScore = unlist(zScore) %>% .[timeaxis_subsample_index] %>% list(),
         zpack = list(tibble(zScore = unlist(zScore),
                             Time = timeaxisTable$Time,
                             Timekey = timeaxisTable$TimeKey))) %>%
  select(!zScore) %>%
  ungroup() %>% 
  unnest(zpack)

non_finite_rows <- Zmeans_unpack %>% 
  filter(!is.finite(Time) | !is.finite(zScore))

non_finite_metadata <- non_finite_rows %>%
  dplyr::select(Subject, Event, Structure, `Paradigm.Day`, Sex) %>% 
  distinct()

Zmeans_unpack <- Zmeans_unpack %>%
  filter(is.finite(Time) & is.finite(zScore))

# all days, structures, subjects - cue avoid escape ------------------
#recursive script to generate csv files for all event/structure combinations of all subjects for each day (rows are time entries -10 to 10s, columns are SUBJECTS)
# Get master lists
all_subjects <- sort(unique(meta.data$Subject[!meta.data$Subject %in% c(9497, 9498, 9499, 9503, 9504, 9505)]))
all_subjects_str <- as.character(all_subjects)
days <- sort(unique(na.omit(Zmeans_unpack$Paradigm.Day)))

# Create an ordered list of EXACTLY the columns we want in the final CSV:
# e.g., "Day1_1", "Day1_2" ... "Day2_1", "Day2_2" ...
# expand.grid varies the first argument fastest, meaning it cycles subjects inside each day
expected_cols_df <- expand.grid(Subject = all_subjects_str, Day = days, stringsAsFactors = FALSE)
expected_cols <- paste0("Day", expected_cols_df$Day, "_", expected_cols_df$Subject)

# Define your combinations
event_structure_combinations <- expand.grid(
  #Event = c("cue_on", "avoid", "escape", "cue_on_avoid", "cue_on_escape"), 
  Event = c("shock"),
  Structure = c("achDLS", "achDMS", "daDLS", "daDMS"), 
  stringsAsFactors = FALSE
)

# Loop through events and structures
for(i in 1:nrow(event_structure_combinations)) {
  event <- event_structure_combinations$Event[i]
  structure <- event_structure_combinations$Structure[i]
  
  # Filter for the specific Event and Structure
  data <- Zmeans_unpack %>%
    filter(Event == event, Structure == structure) %>%
    filter(!is.na(`Paradigm.Day`))
  
  # Skip if there's no data
  if(nrow(data) == 0) next 
  
  # 1. Combine Day and Subject into a single column identifier
  data_wide <- data %>%
    mutate(Day_Subj = paste0("Day", `Paradigm.Day`, "_", Subject)) %>%
    arrange(Timekey) %>%
    dplyr::select(Time, Day_Subj, zScore) %>%  
    # 2. Pivot so Time is the only row, and Day_Subj becomes the horizontal columns
    pivot_wider(names_from = Day_Subj, values_from = zScore)
  
  # 3. Identify which Day-Subject combinations are completely missing
  missing_cols <- setdiff(expected_cols, colnames(data_wide))
  
  # 4. Add the missing ones as empty (NA) columns
  if(length(missing_cols) > 0) {
    for(mc in missing_cols) {
      data_wide[[mc]] <- NA
    }
  }
  
  # 5. Order the columns exactly: Time first, then Day 1 subjects, Day 2 subjects, etc.
  data_wide <- data_wide %>%
    dplyr::select(Time, all_of(expected_cols)) %>%
    arrange(Time) # Ensure rows are sorted sequentially by time (-10s to 10s)
  
  # Write the CSV
  write_csv(data_wide, glue("{event}_{structure}_allperf.csv"))
}



# averaged across days; all structures, subjects - cue avoid escape ------------------
# Master subject list (keeps consistent columns/order across all output files)
all_subjects <- sort(unique(meta.data$Subject[!meta.data$Subject %in% c(9497, 9498, 9499, 9503, 9504, 9505)]))
all_subjects_str <- as.character(all_subjects)

# Define event/structure combos
event_structure_combinations <- expand.grid(
  Event = c("cue_on", "avoid", "escape", "cue_on_avoid", "cue_on_escape", "shock"), 
  Structure = c("achDLS", "achDMS", "daDLS", "daDMS"), 
  stringsAsFactors = FALSE
)

for(i in 1:nrow(event_structure_combinations)) {
  event <- event_structure_combinations$Event[i]
  structure <- event_structure_combinations$Structure[i]
  
  # Filter relevant data
  data <- Zmeans_unpack %>%
    filter(Event == event, Structure == structure) %>%
    filter(!is.na(`Paradigm.Day`))
  
  # Skip empty combinations
  if(nrow(data) == 0) next
  
  # Average across days (and any repeated rows) per Subject x Time
  data_avg <- data %>%
    group_by(Time, Timekey, Subject) %>%
    summarise(zScore = mean(zScore, na.rm = TRUE), .groups = "drop")
  
  # Pivot: rows = timepoints, columns = subjects
  data_wide <- data_avg %>%
    arrange(Timekey) %>%
    dplyr::select(Time, Subject, zScore) %>%
    pivot_wider(names_from = Subject, values_from = zScore)
  
  # Add missing subject columns as NA so every file has same subject columns
  missing_subjects <- setdiff(all_subjects_str, colnames(data_wide))
  if(length(missing_subjects) > 0) {
    for(ms in missing_subjects) data_wide[[ms]] <- NA
  }
  
  # Enforce exact column order
  data_wide <- data_wide %>%
    dplyr::select(Time, all_of(all_subjects_str)) %>%
    arrange(Time)
  
  # Save
  write_csv(data_wide, glue("{event}_{structure}_avgAcrossDays_allperf.csv"))
}


# cross correlation between DA and ACh traces, all events, separated by region (DMS vs DLS) ------------------
# Revised cross-correlation code with improvements:
regions <- c("DMS", "DLS")
events <- c("cue_on", "avoid", "escape", "cue_on_avoid", "cue_on_escape", "shock")
all_subjects_str <- as.character(sort(unique(meta.data$Subject[!meta.data$Subject %in% c(9497, 9498, 9499, 9503, 9504, 9505)])))

# Calculate the time step (dt) to convert lag indices to actual seconds
dt <- diff(timeaxisTable$Time)[1] 

# Define the biological window for cross-correlation
t_min <- -6.0
t_max <- 6.0
max_lag_seconds <- 2.5 

for(region in regions) {
  for(event in events) {
    
    ach_struct <- paste0("ach", region)
    da_struct <- paste0("da", region)
    
    # 1. Prepare data & Average per subject
    data <- Zmeans_unpack %>%
      filter(Event == event, Structure %in% c(ach_struct, da_struct)) %>%
      filter(!is.na(`Paradigm.Day`)) %>%
      group_by(Time, Timekey, Subject, Structure) %>%
      summarise(zScore = mean(zScore, na.rm = TRUE), .groups = "drop")
    
    if(nrow(data) == 0) next
    
    # Pivot so DA and ACh are side-by-side
    data_paired <- data %>%
      pivot_wider(names_from = Structure, values_from = zScore)
    
    if(!all(c(ach_struct, da_struct) %in% names(data_paired))) next
    
    # Initialize output containers
    ccf_traces <- list()
    peak_lags <- data.frame(Subject = character(), Peak_R = numeric(), Optimal_Lag_sec = numeric(), stringsAsFactors = FALSE)
    
    for(subj in all_subjects_str) {
      # Grab subject data AND apply the event-specific time window
      subj_data <- data_paired %>% 
        filter(Subject == subj, Time >= t_min, Time <= t_max) %>% 
        arrange(Timekey)
      
      if(nrow(subj_data) == 0) next
      
      # Extract vectors and explicitly center them (demean)
      x_raw <- scale(subj_data[[da_struct]], center = TRUE, scale = FALSE)
      y_raw <- scale(subj_data[[ach_struct]], center = TRUE, scale = FALSE)
      
      # Handle missing timepoints without dropping the whole subject
      valid_idx <- complete.cases(x_raw, y_raw)
      x <- x_raw[valid_idx]
      y <- y_raw[valid_idx]
      
      # Need enough data points to run a meaningful CCF
      if(length(x) < 10) next
      
      # 2. Calculate Cross-Correlation
      lag_limit <- round(max_lag_seconds / dt)
      ccf_res <- ccf(x, y, lag.max = lag_limit, plot = FALSE)
      
      # Extract lags and correlations
      lags_sec <- ccf_res$lag[,1,1] * dt
      corrs <- ccf_res$acf[,1,1]
      
      # Save trace 
      ccf_traces[[subj]] <- tibble(Lag_sec = lags_sec, !!subj := corrs)
      
      # 3. Find the peak based on ABSOLUTE maximum
      max_idx <- which.max(abs(corrs))
      
      peak_lags <- rbind(peak_lags, data.frame(
        Subject = subj, 
        Peak_R = corrs[max_idx],           
        Optimal_Lag_sec = lags_sec[max_idx] 
      ))
    }
    
    # Skip writing if NO subjects processed successfully
    if(length(ccf_traces) == 0) next
    
    # ---------------------------------------------------------
    # FORMATTING FOR PRISM
    # ---------------------------------------------------------
    
    # A) Traces: Ensure all 27 subjects are columns in exact order
    ccf_traces_wide <- reduce(ccf_traces, full_join, by = "Lag_sec") %>% arrange(Lag_sec)
    missing_subj <- setdiff(all_subjects_str, names(ccf_traces_wide)[-1])
    if(length(missing_subj) > 0) {
      for(ms in missing_subj) ccf_traces_wide[[ms]] <- NA
    }
    ccf_traces_wide <- ccf_traces_wide %>% dplyr::select(Lag_sec, all_of(all_subjects_str))
    
    # B) Peak Lags: Ensure all 27 subjects are ROWS in exact order
    master_subjects_df <- tibble(Subject = all_subjects_str)
    peak_lags_complete <- left_join(master_subjects_df, peak_lags, by = "Subject")
    
    # Write outputs
    write_csv(ccf_traces_wide, glue("CCF_Trace_{event}_{region}.csv"))
    write_csv(peak_lags_complete, glue("CCF_PeakLags_{event}_{region}.csv"))
  }
}




# all days, structures, separated subjects by performance - cue avoid escape ------------------
# Define performance groups each day based on %avoided per day
performance_groups <- c("Lower", "Higher")
# Step 1: Calculate mouse median avoidance across Days 4-7
late_phase_avoid <- Operant_Data %>%
  filter(name %in% c("total.avoids", "total.trials.run")) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  filter(Day %in% 4:7) %>%
  group_by(Subject) %>%
  summarise(median_avoid = median(100 * total.avoids / total.trials.run, na.rm=TRUE), .groups = "drop") 

# Step 2: Calculate median cutoff for group assignment
threshold <- median(late_phase_avoid$median_avoid, na.rm=TRUE)

# Step 3: Assign “Low” or “High” based on this threshold
subject_group <- late_phase_avoid %>%
  mutate(performance_group = if_else(median_avoid < threshold, "Lower", "Higher"))

# Step 4: Join back to all records for plotting/statistics
perc_avoid <- Operant_Data %>%
  filter(name %in% c("total.avoids", "total.trials.run")) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  left_join(subject_group %>% select(Subject, performance_group), by = "Subject")


# iterative code for all event/structure combinations - low vs high performance groups
event_structure_combinations <- expand.grid(Event = c("cue_on", "avoid", "escape", "cue_on_avoid", "cue_on_escape", "shock"), Structure = c("achDLS", "achDMS", "daDLS", "daDMS"), stringsAsFactors = FALSE) 

for(i in 1:nrow(event_structure_combinations)) {
  event <- event_structure_combinations$Event[i]
  structure <- event_structure_combinations$Structure[i]
  
  # Low performance group
  low_perf_data <- Zmeans_unpack %>%
    left_join(subject_group %>% select(Subject, performance_group), by = "Subject") %>%
    filter(Event == event & Structure == structure & performance_group == "Lower") %>%
    group_by(Time, Timekey, `Paradigm.Day`) %>%
    summarise(mean_zScore = mean(zScore, na.rm = TRUE)) %>%
    pivot_wider(names_from = `Paradigm.Day`, values_from = mean_zScore) %>%
    arrange(Timekey) %>%
    select(-Timekey)
  
  write_csv(low_perf_data, glue("{event}_{structure}_lowperf.csv"))
  
  # High performance group
  high_perf_data <- Zmeans_unpack %>%
    left_join(subject_group %>% select(Subject, performance_group), by = "Subject") %>%
    filter(Event == event & Structure == structure & performance_group == "Higher") %>%
    group_by(Time, Timekey, `Paradigm.Day`) %>%
    summarise(mean_zScore = mean(zScore, na.rm = TRUE)) %>%
    pivot_wider(names_from = `Paradigm.Day`, values_from = mean_zScore) %>%
    arrange(Timekey) %>%
    select(-Timekey)
  
  write_csv(high_perf_data, glue("{event}_{structure}_highperf.csv"))
}














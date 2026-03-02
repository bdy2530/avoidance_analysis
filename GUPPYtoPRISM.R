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
  left_join(meta.data) %>% filter(Subject != 42) %>% filter(Subject != 44)

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
#extract day 1-7 cue_on data averaged across all subjects for achDLS (each column is day 1-7, rows are time entries -10 to 10s)
cue_on_achDLS <- Zmeans_unpack %>%
  filter(Event == "cue_on" & Structure == "achDLS") %>%
  group_by(Time, Timekey, `Paradigm.Day`) %>%
  summarise(mean_zScore = mean(zScore, na.rm = TRUE)) %>%
  pivot_wider(names_from = `Paradigm.Day`, values_from = mean_zScore) %>%
  arrange(Timekey) %>% 
  select(-Timekey)
write_csv(cue_on_achDLS, "cue_on_achDLS.csv")

#extract day 1-7 cue_on data averaged across all subjects for achDMS (each column is day 1-7, rows are time entries -10 to 10s)
cue_on_achDMS <- Zmeans_unpack %>%
  filter(Event == "cue_on" & Structure == "achDMS") %>%
  group_by(Time, Timekey, `Paradigm.Day`) %>%
  summarise(mean_zScore = mean(zScore, na.rm = TRUE)) %>%
  pivot_wider(names_from = `Paradigm.Day`, values_from = mean_zScore) %>%
  arrange(Timekey) %>% 
  select(-Timekey)
write_csv(cue_on_achDMS, "cue_on_achDMS.csv")

#extract day 1-7 cue_on data averaged across all subjects for daDLS (each column is day 1-7, rows are time entries -10 to 10s)
cue_on_daDLS <- Zmeans_unpack %>%
  filter(Event == "cue_on" & Structure == "daDLS") %>%
  group_by(Time, Timekey, `Paradigm.Day`) %>%
  summarise(mean_zScore = mean(zScore, na.rm = TRUE)) %>%
  pivot_wider(names_from = `Paradigm.Day`, values_from = mean_zScore) %>%
  arrange(Timekey) %>% 
  select(-Timekey)
write_csv(cue_on_daDLS, "cue_on_daDLS.csv")

#extract day 1-7 cue_on data averaged across all subjects for daDMS (each column is day 1-7, rows are time entries -10 to 10s)
cue_on_daDMS <- Zmeans_unpack %>% 
  filter(Event == "cue_on" & Structure == "daDMS") %>%
  group_by(Time, Timekey, `Paradigm.Day`) %>%
  summarise(mean_zScore = mean(zScore, na.rm = TRUE)) %>%
  pivot_wider(names_from = `Paradigm.Day`, values_from = mean_zScore) %>%
  arrange(Timekey) %>% 
  select(-Timekey)
write_csv(cue_on_daDMS, "cue_on_daDMS.csv")



#extract day 1-7 avoid data averaged across all subjects for achDLS (each column is day 1-7, rows are time entries -10 to 10s)
avoid_achDLS <- Zmeans_unpack %>%
  filter(Event == "avoid" & Structure == "achDLS") %>%
  group_by(Time, Timekey, `Paradigm.Day`) %>%
  summarise(mean_zScore = mean(zScore, na.rm = TRUE)) %>%
  pivot_wider(names_from = `Paradigm.Day`, values_from = mean_zScore) %>%
  arrange(Timekey) %>% 
  select(-Timekey)
write_csv(avoid_achDLS, "avoid_achDLS.csv")

#extract day 1-7 avoid data averaged across all subjects for achDMS (each column is day 1-7, rows are time entries -10 to 10s)
avoid_achDMS <- Zmeans_unpack %>%
  filter(Event == "avoid" & Structure == "achDMS") %>%
  group_by(Time, Timekey, `Paradigm.Day`) %>%
  summarise(mean_zScore = mean(zScore, na.rm = TRUE)) %>%
  pivot_wider(names_from = `Paradigm.Day`, values_from = mean_zScore) %>% 
  arrange(Timekey) %>%
  select(-Timekey)
write_csv(avoid_achDMS, "avoid_achDMS.csv")

#extract day 1-7 avoid data averaged across all subjects for daDLS (each column is day 1-7, rows are time entries -10 to 10s)
avoid_daDLS <- Zmeans_unpack %>%
  filter(Event == "avoid" & Structure == "daDLS") %>%
  group_by(Time, Timekey, `Paradigm.Day`) %>%
  summarise(mean_zScore = mean(zScore, na.rm = TRUE)) %>%
  pivot_wider(names_from = `Paradigm.Day`, values_from = mean_zScore) %>%
  arrange(Timekey) %>%
  select(-Timekey)
write_csv(avoid_daDLS, "avoid_daDLS.csv")

#extract day 1-7 avoid data averaged across all subjects for daDMS (each column is day 1-7, rows are time entries -10 to 10s)
avoid_daDMS <- Zmeans_unpack %>%
  filter(Event == "avoid" & Structure == "daDMS") %>%
  group_by(Time, Timekey, `Paradigm.Day`) %>%
  summarise(mean_zScore = mean(zScore, na.rm = TRUE)) %>%
  pivot_wider(names_from = `Paradigm.Day`, values_from = mean_zScore) %>% 
  arrange(Timekey) %>%
  select(-Timekey)
write_csv(avoid_daDMS, "avoid_daDMS.csv")



#extract day 1-7 escape data averaged across all subjects for achDLS (each column is day 1-7, rows are time entries -10 to 10s)
escape_achDLS <- Zmeans_unpack %>%
  filter(Event == "escape" & Structure == "achDLS") %>%
  group_by(Time, Timekey, `Paradigm.Day`) %>%
  summarise(mean_zScore = mean(zScore, na.rm = TRUE)) %>%
  pivot_wider(names_from = `Paradigm.Day`, values_from = mean_zScore) %>%
  arrange(Timekey) %>% 
  select(-Timekey)
write_csv(escape_achDLS, "escape_achDLS.csv")

#extract day 1-7 escape data averaged across all subjects for achDMS (each column is day 1-7, rows are time entries -10 to 10s)
escape_achDMS <- Zmeans_unpack %>%
  filter(Event == "escape" & Structure == "achDMS") %>%
  group_by(Time, Timekey, `Paradigm.Day`) %>%
  summarise(mean_zScore = mean(zScore, na.rm = TRUE)) %>%
  pivot_wider(names_from = `Paradigm.Day`, values_from = mean_zScore) %>% 
  arrange(Timekey) %>%
  select(-Timekey)
write_csv(escape_achDMS, "escape_achDMS.csv")

#extract day 1-7 escape data averaged across all subjects for daDLS (each column is day 1-7, rows are time entries -10 to 10s)
escape_daDLS <- Zmeans_unpack %>%
  filter(Event == "escape" & Structure == "daDLS") %>%
  group_by(Time, Timekey, `Paradigm.Day`) %>%
  summarise(mean_zScore = mean(zScore, na.rm = TRUE)) %>%
  pivot_wider(names_from = `Paradigm.Day`, values_from = mean_zScore) %>% 
  arrange(Timekey) %>%
  select(-Timekey)
write_csv(escape_daDLS, "escape_daDLS.csv")

#extract day 1-7 escape data averaged across all subjects for daDMS (each column is day 1-7, rows are time entries -10 to 10s)
escape_daDMS <- Zmeans_unpack %>%
  filter(Event == "escape" & Structure == "daDMS") %>%
  group_by(Time, Timekey, `Paradigm.Day`) %>%
  summarise(mean_zScore = mean(zScore, na.rm = TRUE)) %>%
  pivot_wider(names_from = `Paradigm.Day`, values_from = mean_zScore) %>% 
  arrange(Timekey) %>%
  select(-Timekey)
write_csv(escape_daDMS, "escape_daDMS.csv")









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

#extract day 1-7 cue_on data averaged across lower performance groups for achDLS (each column is day 1-7, rows are time entries -10 to 10s)
cue_on_achDLS_lowperf <- Zmeans_unpack %>%
  left_join(subject_group %>% select(Subject, performance_group), by = "Subject") %>%
  filter(Event == "cue_on" & Structure == "achDLS" & performance_group == "Lower") %>%
  group_by(Time, Timekey, `Paradigm.Day`) %>%
  summarise(mean_zScore = mean(zScore, na.rm = TRUE)) %>%
  pivot_wider(names_from = `Paradigm.Day`, values_from = mean_zScore) %>%
  arrange(Timekey) %>% 
  select(-Timekey)
write_csv(cue_on_achDLS_lowperf, "cue_on_achDLS_lowperf.csv")

# iterative code for all event/structure combinations - low vs high performance groups
event_structure_combinations <- expand.grid(Event = c("cue_on", "avoid", "escape"), Structure = c("achDLS", "achDMS", "daDLS", "daDMS"), stringsAsFactors = FALSE) 
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














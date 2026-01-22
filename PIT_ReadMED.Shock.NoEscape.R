#loads packages
library(readtext)
library(tidyverse)
library(lubridate)
library(readxl)
library(glue)
#library(zoo)
#options(warn = 2)
source("C:/Users/bdy2530/Desktop/Analysis/functionsPIT_New.R")

Experiments <- c("Shock.NoEscape")

for (i in Experiments){
  Experiment <- i
  
  #Gets list of files in path excluding folders
  DirWithMEDfiles <- r"(R:\Basic_Sciences\Phys\Lerner_Lab_tnl2633\Baran\MedPC\ShockNoEscape\)"
  MedFiles <- setdiff(list.files(path = DirWithMEDfiles, full.names = TRUE), 
                      list.dirs(path = DirWithMEDfiles, full.names = TRUE, recursive = FALSE))
  
  #scans folder for files without .txt and adds so they can be opened (due to laziness on rec days). KEEP OTHER FILE TYPES AWAY FROM THIS
  for (i in 1:length(MedFiles)){
    if (!str_detect(MedFiles[i],'.txt')){
      file.rename(MedFiles[i], str_c(MedFiles[i],'.txt'))
    }
  }
  
  #rerun this line to get updated list if .txt added (could add if statement, not necessary if no .txt added)
  # MedFiles <- setdiff(list.files(path = DirWithMEDfiles, full.names = TRUE), 
  #                     list.dirs(path = DirWithMEDfiles, full.names = TRUE, recursive = FALSE))
  
  #Loads preexisting data, if nothing makes an empty and saves it prior to loading
  
  MED.R.Name.Conversion <- read_xlsx("RK_Shock.NoEscape_MEDtoR_variableNames.xlsx") #MED to R variable name associations
  
  #Subject.Paradigm <- readRDS(file = glue("{Experiment}.Subject.Paradigm.Rds")) #Paradigm assignment data
  
  if (!file_test("-f", glue("{Experiment}_Operant_Data.Rds"))){ #behavioral data
    Operant_Data <- tibble()
    saveRDS(Operant_Data, file = glue("{Experiment}_Operant_Data.Rds"))
  }else {
    Operant_Data <- readRDS(file = glue("{Experiment}_Operant_Data.Rds"))
  }
  
  if (!file_test("-f", glue("{Experiment}_meta_Data.Rds"))){ #meta data (some of which is packages wtih behavioral data)
    meta_Data <- tibble()
    saveRDS(meta_Data, file = glue("{Experiment}_meta_Data.Rds"))
  }else {
    meta_Data <- readRDS(file = glue("{Experiment}_meta_Data.Rds"))
  }
  
  if (!file_test("-f", glue("{Experiment}_settings_num.Rds"))){ #MED settings harvested from output (all numeric)
    settings_num <- tibble()
    saveRDS(settings_num, file = glue("{Experiment}_settings_num.Rds"))
  }else {
    settings_num <- readRDS(file = glue("{Experiment}_settings_num.Rds"))
  }
  
  # Main Loop for extracting MED data into R --------------------------------
  for (i0 in 1:length(MedFiles)){
    
    # Split the full file path into components to extract the filename.
    fileNameData <- str_split(MedFiles[i0], "/|\\\\")[[1]]
    fileNameData <- str_replace(fileNameData[length(fileNameData)], ".txt", "")
    
    # Only process files whose names start with the current experiment name.
    if (str_detect(fileNameData, glue("^{Experiment}_"))){
      MED <- ReadNameMED_Shock.NoEscape(MedFiles, i0)
      
      #this can be moved into modulesLBN.R when you have more data to import
      #MEDOut is superfluous and should BE the output of m$ReaNamMED... the fct things can also be added, though piping will have to be removed
      meta.add <- as_tibble(MED$meta) %>%
        mutate(DateTime = mdy_hms(str_c(Date, StartTime, sep = "T"), tz = "US/Central"), 
               DateTimeEnd = mdy_hms(str_c(Date, EndTime, sep = "T"), tz = "US/Central"), 
        ) %>%
        select(!c(Date, StartTime, EndTime)) %>%
        relocate(DateTime, Subject, Paradigm, Paradigm.Day, Box, ProgramName)
      # meta.add <- meta.add %>% mutate(Paradigm = factor(Paradigm, levels = levels(Subject.Paradigm$Paradigm), ordered = TRUE))
      
      # Append the new metadata to the existing meta_Data tibble.
      # Then, for each subject, arrange by paradigm and day, assign a sequential Day number,
      # and ensure Paradigm.Day is numeric.
      meta_Data <- bind_rows(meta_Data, meta.add) %>% 
        group_by(Subject) %>%
        arrange(Paradigm, Paradigm.Day) %>%
        mutate(Day = row_number(), Paradigm.Day = as.numeric(Paradigm.Day)) %>%
        relocate(Day, .after = Paradigm.Day) %>%
        ungroup()
      
      # Process MED output data:
      # Convert the output list to a tibble and “unnest” it so each row corresponds to one data entry.
      # Group by the variable name and create an Instance index for duplicate entries.
      # Then join with metadata to associate DateTime, Subject, Paradigm, etc.
      MEDOut <-  tibble(enframe(MED$out)) %>% 
        unnest_longer(value) %>%
        group_by(name) %>%
        mutate(Instance = row_number()) %>%
        left_join(meta.add %>% select(DateTime, Subject, Paradigm, Paradigm.Day), by = character()) %>%
        left_join(meta_Data %>% select(Subject, Paradigm, Paradigm.Day, Day)) %>%
        relocate(DateTime, Subject, Paradigm, Paradigm.Day) %>%
        ungroup()
      #let's remove the data and leave settings and counters behind to label according to MED.R.Name.Conversion
      settings_num_add <- MEDOut %>% 
        filter(name %in% c("settings")) %>%
        select(c(Subject, Paradigm, Paradigm.Day, Day,  name, value, Instance)) %>%
        left_join(MED.R.Name.Conversion %>% rename(name = R) %>% select(!DIM), by = c("name", "Instance")) %>% 
        select(!c(name,Instance)) %>%
        rename(name = rename.R) %>%
        filter(!name == "unused") %>%
        pivot_wider(names_from = c(name), names_sep = "", values_from = value)
      
      #let's include in only data, not settings, move settings into MEDOut.meta above
      op.summary.data <- MEDOut %>% filter(name %in% c("summary")) %>% 
        left_join(MED.R.Name.Conversion %>% rename(name = R) %>% select(!DIM), by = c("name", "Instance")) %>% 
        select(!c(name,Instance)) %>%
        rename(name = rename.R) %>%
        filter(!name == "unused")
      
      op.trial.data <-  tibble(enframe(MED$out)) %>% 
        unnest_longer(value) %>%
        group_by(name) %>%
        mutate(Instance = row_number()) %>% 
        filter(name == "trial.data") %>% 
        .$value %>% 
        MED.array.detangle.inesc() %>% left_join(meta.add %>% select(DateTime, Subject, Paradigm, Paradigm.Day), by = character()) %>%
        left_join(meta_Data %>% select(Subject, Paradigm, Paradigm.Day, Day)) %>%
        relocate(DateTime, Subject, Paradigm, Paradigm.Day)
      
      op.MEDOut <- MEDOut %>% filter(name %in% c("elapsed.time", "crossings","crossings.shock"))
      
      
      
      #aggregates rows as they load in
      Operant_Data <- bind_rows(Operant_Data, op.trial.data, op.summary.data, op.MEDOut)
      settings_num <- bind_rows(settings_num, settings_num_add)
      
      
      
      #move file once read
      if(!dir.exists(str_c(DirWithMEDfiles, "/movedByRScript/"))){
        dir.create(str_c(DirWithMEDfiles, "/movedByRScript/"))
      }
      
      
      invisible(file.rename(MedFiles[i0], str_c(DirWithMEDfiles, "movedByRScript/", basename(MedFiles[i0]))))
      
      #save PIT_Operant_Data
      Operant_Data <- Operant_Data %>% distinct() %>% filter(Day == 1)
      meta_Data <- meta_Data %>% filter(Day == 1)
      settings_num <- settings_num %>% distinct()%>% filter(Day == 1)
      
      saveRDS(Operant_Data, file = glue("{Experiment}_Operant_Data.Rds"))
      saveRDS(meta_Data, file = glue("{Experiment}_meta_Data.Rds"))
      saveRDS(settings_num, file = glue("{Experiment}_settings_num.Rds"))
    }
  }
}

# New function to process crossings and shock crossings --------------------------------
library(dplyr)

Operant_Data <- Operant_Data %>% 
  # Remove rows with crossing entries (both types)
  filter(!name %in% c("total.crossings", "total.crossings.shock")) %>%
  # Bind in new rows: group the old crossing rows and sum their values
  bind_rows(
    Operant_Data %>% 
      filter(name %in% c("total.crossings", "total.crossings.shock")) %>%
      group_by(DateTime, Subject, Paradigm, Paradigm.Day, Day) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(name = "total.crossings",
             Trial = NA, 
             Instance = NA)
  ) %>%
  arrange(DateTime, Subject, Paradigm, Paradigm.Day, Day, Trial)

saveRDS(Operant_Data, file = glue("{Experiment}_Operant_Data.Rds"))


update_crossing_instances_reorder <- function(df) {
  # Ensure value is numeric and record the original row order
  df <- df %>%
    mutate(value = as.numeric(value),
           OrigRow = row_number())
  
  # Process each subject separately.
  # For crossing rows, compute the new instance number based on value.
  # Then, create an ordering key that for crossing rows is the new instance,
  # and for non-crossing rows is set so that they appear after the crossing rows.
  df_updated <- df %>%
    group_by(Subject) %>%
    mutate(newInstance = if_else(name %in% c("crossings", "crossings.shock"),
                                 as.integer(rank(value, ties.method = "first")),
                                 NA_integer_),
           # Compute the maximum instance number for this subject (if any crossings exist)
           maxInstance = if_else(any(name %in% c("crossings", "crossings.shock")),
                                 max(newInstance, na.rm = TRUE), 0L),
           # For ordering: if this is a crossing row, use newInstance;
           # otherwise, add the original row order (plus maxInstance) so that non-crossing rows come later.
           orderKey = if_else(name %in% c("crossings", "crossings.shock"),
                              newInstance,
                              maxInstance + OrigRow)
    ) %>%
    ungroup() %>%
    arrange(Subject, orderKey) %>%
    # Replace the Instance numbers in the crossing rows with the new instance numbers.
    mutate(Instance = if_else(name %in% c("crossings", "crossings.shock"),
                              newInstance,
                              Instance)) %>%
    # Optionally, remove helper columns
    select(-newInstance, -maxInstance, -orderKey, -OrigRow)
  
  return(df_updated)
}


test_data <- Operant_Data
test_data <- update_crossing_instances_reorder(test_data)

test_data_add <- test_data %>% filter(name %in% c("crossings", "crossings.shock")) %>% mutate(name = "crossing_all") %>%
  group_by(Subject, Paradigm.Day) %>%
  arrange(Subject, Paradigm.Day, value) %>%
  mutate(Instance = row_number())

test_data <- bind_rows(test_data, test_data_add) %>% arrange(Subject, Paradigm.Day) %>% filter(!name %in% c("crossings", "crossings.shock")) %>% mutate(name = if_else(name == "crossing_all", "crossings", name))
# you MIGHT have to fix the total.crossings for each animal

test_data$Day <- 8
test_data$Paradigm.Day <- 8

evenmore_test_data <- test_data %>%
  mutate(name = case_when(
    name == "Avoid" & !is.na(lead(value)) & (abs(lead(value) - value) > 5) ~ "Escape",
    name == "avoid.latency" & value > 5 ~ "escape.latency",
    TRUE ~ name
  ))

evenmore_test_data <- evenmore_test_data %>%
  group_by(Subject, Trial) %>%
  mutate(
    # Check if both Avoid and Escape exist in this trial
    has_both = any(name == "Avoid") & any(name == "Escape"),
    # Keep Avoid if both exist, otherwise keep whatever exists
    keep_event = case_when(
      !name %in% c("Avoid", "Escape") ~ TRUE,  # Keep all non-Avoid/Escape rows
      has_both & name == "Avoid" ~ TRUE,       # Keep Avoid when both present
      has_both & name == "Escape" ~ FALSE,     # Remove Escape when both present
      TRUE ~ TRUE                              # Keep single events
    )
  ) %>%
  filter(keep_event) %>%
  select(-has_both, -keep_event) %>%
  ungroup()

evenmore_test_data <- evenmore_test_data %>%
  group_by(Subject, name) %>%
  mutate(Instance = if_else(name %in% c("Avoid", "Escape"),
                            as.integer(row_number()),
                            Instance)) %>%
  ungroup()

recalculated_summaries <- evenmore_test_data %>%
  filter(!is.na(Trial)) %>%  # Only use trial data, not existing summary rows
  group_by(Subject, DateTime, Paradigm, Paradigm.Day, Day) %>%
  summarise(
    total.trials.run = n_distinct(Trial, na.rm = TRUE),
    total.avoids = sum(name == "Avoid", na.rm = TRUE),
    total.escapes = sum(name == "Escape", na.rm = TRUE),
    total.avoid.latency = ifelse(sum(name == "avoid.latency", na.rm = TRUE) > 0,
                                 mean(value[name == "avoid.latency"], na.rm = TRUE), 0),
    total.escape.latency = ifelse(sum(name == "escape.latency", na.rm = TRUE) > 0,
                                  mean(value[name == "escape.latency"], na.rm = TRUE), 0),
    total.ITI.shocks = sum(name == "Shock", na.rm = TRUE),
    # Remove this line that's causing the problem:
    # total.crossings = sum(value[name == "crossings"], na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Reshape to match your data format (excluding total.crossings)
  pivot_longer(cols = total.trials.run:total.ITI.shocks,  # Changed this line
               names_to = "name", 
               values_to = "value") %>%
  mutate(Trial = NA, Instance = NA) %>%
  select(DateTime, Subject, Paradigm, Paradigm.Day, Trial, name, value, Instance, Day)

# Then when creating final_data, include the existing total.crossings:
final_data <- evenmore_test_data %>%
  filter(!name %in% c("total.trials.run", "total.avoids", "total.escapes", 
                      "total.avoid.latency", "total.escape.latency", 
                      "total.ITI.shocks", "total.crossings")) %>%
  bind_rows(
    recalculated_summaries,  # This now excludes total.crossings
    existing_total_crossings  # Add back the correct total.crossings values
  ) %>%
  arrange(Subject, Paradigm.Day, Trial, name)

evenmore_test_data <- final_data


saveRDS(evenmore_test_data, file = glue("{Experiment}_Operant_Data.Rds"))

# Combine the active and shock data --------------------------------
active <- readRDS("Active.Avoidance_Operant_Data.Rds")
shock  <- readRDS("Shock.NoEscape_Operant_Data.Rds")
combined <- bind_rows(active, shock) %>%
  arrange(Subject, Day, DateTime)  # This orders by Subject, then Day, then DateTime

saveRDS(combined, file = "Combined_Operant_Data.Rds")

meta_Data$Paradigm.Day <- 8
meta_Data$Day <- 8
saveRDS(meta_Data, file = glue("{Experiment}_meta_Data.Rds"))

settings_num$Paradigm.Day <- 8
settings_num$Day <- 8
saveRDS(settings_num, file = glue("{Experiment}_settings_num.Rds"))

rm(active, shock, combined)
active <- readRDS("Active.Avoidance_meta_Data.Rds")
shock  <- readRDS("Shock.NoEscape_meta_Data.Rds")
combined <- bind_rows(active, shock) %>%
  arrange(Subject, Day, DateTime)  # This orders by Subject, then Day, then DateTime
saveRDS(combined, file = "Combined_meta_Data.Rds")

rm(active, shock, combined)
active <- readRDS("Active.Avoidance_settings_num.Rds")
shock  <- readRDS("Shock.NoEscape_settings_num.Rds")
combined <- bind_rows(active, shock) %>%
  arrange(Subject, Day)  # This orders by Subject, then Day, then DateTime
saveRDS(combined, file = "Combined_settings_num.Rds")



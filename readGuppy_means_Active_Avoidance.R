library(rhdf5)
library(tidyverse)
library(progress)
library(googlesheets4)
library(ggrepel)
library(glue)
library(lubridate)
gs4_deauth()


# Load Data and helper functions ------------------------------------------
Experiment <- "Active.Avoidance"
synapse_eval_trim <- readRDS(file = "Photometry_Eval.Active_Avoidance")

#may need to import operant data for something later... use the latter if needed
# LBN_Operant_Data <- readRDS(file = "LBN_Operant_Data_Mod.Rds")
#Operant_Data <- readRDS(file = glue("{Experiment}_Operant_Data.Rds"))




# File location -----------------------------------------------------------
guppylocation <- r"(C:\Users\bdy2530\Downloads\GuPPy_everything\SynapseTanks\AA-combined_1-2-3-4)"
guppyfolders.dir <- list.dirs(guppylocation, recursive = TRUE)
guppyfiles <- list.files(guppylocation, recursive = TRUE) %>% 
  as_tibble() %>% 
  rename(files = value) %>% 
  mutate(Filename = str_extract(files, "^[^/]+")) %>% 
  semi_join(synapse_eval_trim, by = join_by(Filename)) %>% 
  select(!Filename)


# Get file names ----------------------------------------------------------
#get h5 files for extraction
possibleEvents <- c("control_achDLS","control_achDMS","control_daDLS", "control_daDMS","signal_achDLS","signal_achDMS","signal_daDLS", "signal_daDMS",
                    "reward_delivered" ,"cue_on","port_entry" ,"lever_left" ,"lever_right" ,"timer","opto_on" ,"shock_on" ,"cue_off" ,"cue_on","cross"
                    ,"escape","avoid" ,"shock","timer")

event.H5.Files <- guppyfiles %>% 
  filter(str_detect(files, str_c("(",paste(possibleEvents, collapse = "|"), ").*z_score.*\\.h.*5")),
         !str_detect(files, "Uncorrected|peak")) %>% 
  mutate(Filename = str_extract(files, "^[^/]+"),  # Re-add session name
         Structure = str_extract(files, "(?<=_z_score_)[^\\.]+")) %>%  # Extract structure (e.g., achDLS)
  semi_join(synapse_eval_trim, by = c("Filename", "Structure"))  # Filter per session + structure

event.csv.Files <- guppyfiles %>% 
  filter(str_detect(files, str_c("(",paste(possibleEvents, collapse = "|"), ").*z_score.*\\.csv")),
         !str_detect(files, "Uncorrected")) %>% 
  mutate(Filename = str_extract(files, "^[^/]+"),  # Re-add session name
         Structure = str_extract(files, "(?<=_z_score_)[^\\.]+")) %>%  # Extract structure
  semi_join(synapse_eval_trim, by = c("Filename", "Structure"))  # Filter per session + structure



# Extract data from h5 files ----------------------------------------------
readZmeanH5 <- function(event.H5.dir, experiment_dir = guppylocation, min_ts = -10, max_ts = 10){
  dirname <- str_split(event.H5.dir, "/")[[1]][1]
  dirnameparts <- str_split(dirname,"_|-")[[1]]
  Subject.name <- dirnameparts[1]
  Paradigm.name <- dirnameparts[2]
  filename <- str_split(event.H5.dir, "/")[[1]] %>% tail(n = 1)
  fileparts <- strsplit(filename, "_z_score_")[[1]]
  structure.name <- str_remove(fileparts[2], "\\..*")
  event.name <- str_remove(fileparts[1], str_c("_", structure.name))
  abspath_data <- file.path(experiment_dir, event.H5.dir)
  block0_values <- h5read(abspath_data,"df/block0_values") 
  # axis0 <- h5read(test,"df/axis0") #axis0 is a string that describes rows of block0_values
  # axis1 <- h5read(test,"df/axis1") #axis1 just has a count of the number of columns of data and is fairly useless
  # block0_items <- h5read(test,"df/block0_items") #block0_items has basically the same information as axis0
  #block0_values, each row represents a trial of data:
  #critically the bottom 3 rows are timestamps, mean, and err. The rest are timestamps
  #I'll want to extract individual rows to check out sequences later
  #to replicat prior analysis for now, take only mean over specified range
  ts_row <-  dim(block0_values)[1]-2
  mean_row <- dim(block0_values)[1]-1
  filt_block <- block0_values[,which(block0_values[ts_row,] >= min_ts & block0_values[ts_row,] < max_ts)]
  zMean <- filt_block[mean_row,]
  if (!file.exists(glue("{Experiment}_timeaxis_h5.Rds"))){
    timeaxis_h5 <- filt_block[ts_row,]
    saveRDS(timeaxis_h5, file = glue("{Experiment}_timeaxis_h5.Rds")) #if this bugs pass Experiment to function
  }
  data_test <- tibble(Subject = Subject.name, Date = ymd(Paradigm.name), Event = event.name, Structure = structure.name) %>% 
    add_column(Zmean = list(zMean))

  data_test
}

#wrapper for progress bar
map_df_progress <- function(.x, .f, ..., .id = NULL) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(total = length(.x), force = TRUE)
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::map_df(.x, f, ..., .id = .id)
}


LBN_Operant_Data.Zmean <- tibble()
saveRDS(LBN_Operant_Data.Zmean, file = glue("{Experiment}_Operant_Data.Zmeans.Rds"))
# file.remove(file = glue("{Experiment}_timeaxis_h5.Rds"))
LBN_Operant_Data.Zmean <- map_df_progress(event.H5.Files$files, readZmeanH5)
# LBN_Operant_Data.Zmean <- LBN_Operant_Data.Zmean %>% filter(Subject != 9503 | !(Structure %in% c("achDLS", "daDLS")))
saveRDS(LBN_Operant_Data.Zmean, file = glue("{Experiment}_Operant_Data.Zmeans.Rds"))
timeaxis_h5 <- readRDS(file = glue("{Experiment}_timeaxis_h5.Rds"))

#run Active_Avoidance_Munge_CueIDs.R, then readGuppy_means_Active_Avoidance_CueOnTrialDefined.R next to extract escape/avoid trials

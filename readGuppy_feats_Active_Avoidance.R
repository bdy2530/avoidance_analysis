#this script relies on Photometry_Assessement_Active_Avoidance (or some other child version to come)
  #so that it generates the Photometry_Eval.Active_Avoidance.Rds file

library(rhdf5)
library(tidyverse)
library(progress)
library(googlesheets4)
library(glue)
library(lubridate)
source("C:/Users/bdy2530/Desktop/Analysis/functionsPIT_New.R")
gs4_deauth()

# Load Data and helper functions ------------------------------------------
#operant data not in use here...
Experiment <- "Active.Avoidance"
# setup_metadata_link <- "https://docs.google.com/spreadsheets/d/15h8O_2W2hGp28pBgIBfIG3BayWDFxUFyNmIU3BRs2Is/edit?usp=sharing"
# setup_metadata <- read_sheet(setup_metadata_link) %>% as_tibble() %>% 
#   mutate(Subject = as.character(Subject), 
#          Subject = case_when(nchar(Subject) == 2 ~ str_c("0", Subject),
#                              TRUE ~ Subject)) %>% 
#   filter(DOB >= ymd("2023-01-01")) %>% 
#   select(Subject, Group, Sex)
# Operant_Data <- readRDS(file = glue("{Experiment}_Operant_Data.Rds")) %>% left_join(setup_metadata)

synapse_eval_trim <- readRDS(file = "Photometry_Eval.Active_Avoidance")

# File location -----------------------------------------------------------
guppylocation <- r"(C:\Users\bdy2530\Downloads\GuPPy_everything\SynapseTanks\AA-01.2026\BD_2color-250110-104351)"
guppyfolders.dir <- list.dirs(guppylocation, recursive = TRUE)
guppyfiles <- list.files(guppylocation, recursive = TRUE) %>% 
  as_tibble() %>% 
  rename(files = value) %>% 
  mutate(Filename = str_extract(files, "^[^/]+")) %>% 
  semi_join(synapse_eval_trim, by = join_by(Filename)) %>% 
  select(!Filename)


         
# Get file names ----------------------------------------------------------
#get h5 files for extraction
# possibleEvents <- c("InactiveNP", "PE_NR_Ts", "ActiveNP_NR", "Rew_Ts", 
#                     "PE_Rew_Ts", "Shk_Ts", "L_NP_Ts", "R_NP_Ts")
possibleEvents <- c("control_achDLS","control_achDMS","control_daDLS", "control_daDMS","signal_achDLS","signal_achDMS","signal_daDLS", "signal_daDMS",
                    "reward_delivered" ,"cue_on","port_entry" ,"lever_left" ,"lever_right" ,"timer","opto_on" ,"shock_on" ,"cue_off" ,"cue_on","cross" ,"escape","avoid" ,"shock","timer", "SkCs")
event.H5.Files <- guppyfiles %>% 
  filter(str_detect(files, str_c("(",paste(possibleEvents, collapse = "|"), ").*z_score.*\\.h.*5")),
         !str_detect(files, "Uncorrected|peak"))
event.csv.Files <- guppyfiles %>% 
  filter(str_detect(files, str_c("(",paste(possibleEvents, collapse = "|"), ").*z_score.*\\.csv")),
         !str_detect(files, "Uncorrected"))



# Extract summary data (pos/neg peak, AUC) from csv's files ----------------------------------------------
readZmeanCSV <- function(event.csv.dir, experiment_dir = guppylocation){
  # event.csv.dir <- event.csv.Files$files[1]
  dirname <- str_split(event.csv.dir, "/")[[1]][1]
  dirnameparts <- str_split(dirname,"_|-")[[1]]
  Subject.name <- dirnameparts[1]
  Paradigm.name <- dirnameparts[2]
  filename <- str_split(event.csv.dir, "/")[[1]] %>% tail(n = 1)
  fileparts <- strsplit(filename, "_z_score_")[[1]]
  structure.name <- str_remove(fileparts[2], "\\..*")
  event.name <- str_remove(fileparts[1], str_c("_", structure.name)) %>% 
    str_remove("peak_AUC_")
  abspath_data <- file.path(experiment_dir, event.csv.dir)
  #reading the columns here, these are the time bins for extracted parameters
  #[-2.0, -2.0, 0.0, -5.0, 0.0, 5.0, -1.0, 0.0, -0.5, 0.0] #startPoint.value
  #[2.0,   0.0, 2.0,  0.0, 3.0, 10.0, 0.0, 1.0,  0.0, 0.5]     #endPoint.value
  block0_values <- read.csv(abspath_data, header = TRUE) %>% 
    mutate(timestamp = map(X, ~str_split(.x, "_")[[1]] %>% tail(n = 1)), .keep = "unused") %>% 
    relocate(timestamp) %>% 
    select(timestamp, peak_pos_1, peak_neg_1, area_1) #this will select 0 to 5s as Gaby did
    # select(1:4) #keeps only first set of features computed from -2 to 2s in the PSTH (see above for full list)
  filt_block <- block0_values %>% filter(timestamp == "mean") %>% select(!timestamp)
  data <- tibble(Subject = Subject.name, Date = ymd(Paradigm.name), Event = event.name, Structure = structure.name) %>% 
    add_column(filt_block)
  Operant_Data.Feats <- bind_rows(LBN_Operant_Data.Feats, data)
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

#call to make pull
# LBN_Operant_Data.Feats <- readRDS(glue("{Experiment}_Operant_Data.Feats.Rds"))
LBN_Operant_Data.Feats <- tibble()
saveRDS(LBN_Operant_Data.Feats, file = glue("{Experiment}_Operant_Data.Feats.Rds"))
LBN_Operant_Data.Feats <- map_df_progress(event.csv.Files$files, readZmeanCSV)
# LBN_Operant_Data.Feats <- LBN_Operant_Data.Feats %>% 
#   mutate(Paradigm = ymd(Paradigm)) %>% 
#   rename(Date = Paradigm)
saveRDS(LBN_Operant_Data.Feats, file = glue("{Experiment}_Operant_Data.Feats.Rds"))
# 
# if (!file.exists("LBN_Operant_Data.Feats.Rds")){
#   LBN_Operant_Data.Feats <- tibble()
#   saveRDS(LBN_Operant_Data.Feats, file = "LBN_Operant_Data.Feats.Rds")
#   LBN_Operant_Data.Feats <- map_df_progress(event.csv.Files$files, readZmeanCSV)
#   saveRDS(LBN_Operant_Data.Feats, file = "LBN_Operant_Data.Feats.Rds")
# }else {
#   LBN_Operant_Data.Feats <- readRDS(file = "LBN_Operant_Data.Feats.Rds")
# }
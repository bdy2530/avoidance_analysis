#run Active_Avoidance_Munge_CueIDs.R first to get native events

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

cue_ids <- readRDS(file = glue("cue_ids_{Experiment}"))
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
  semi_join(synapse_eval_trim, by = join_by(Filename)) #%>% 
  #select(!Filename)


# Get file names ----------------------------------------------------------
#get h5 files for extraction
possibleEvents <- c("cue_on")

event.H5.Files <- guppyfiles %>% 
  filter(str_detect(files, str_c("(",paste(possibleEvents, collapse = "|"), ").*z_score.*\\.h.*5")),
         !str_detect(files, "Uncorrected|peak"))
event.csv.Files <- guppyfiles %>% 
  filter(str_detect(files, str_c("(",paste(possibleEvents, collapse = "|"), ").*z_score.*\\.csv")),
         !str_detect(files, "Uncorrected"))



# Extract data from h5 files ----------------------------------------------

# Define the matching function to pair up Avoids/Escapes from MED to Synapse records which can be different
# This function takes a timestamp (ts_synapse) and a data frame of current cue IDs (current_cue_ids).
# match_cue_id <- function(ts_synapse, current_cue_ids) {
#   # Find the corresponding row in current_cue_ids where the difference between the cue value and ts_synapse is less than 1.5.
#   matched_row <- current_cue_ids %>%
#     # mutate(check = abs(value - as.numeric(ts_synapse)) < 1.5)
#     filter(abs(value - as.numeric(ts_synapse)) < 1.5) %>% # Allowing for some small numerical tolerance
#     select(name)
#   
#   if (nrow(matched_row) > 0) {
#     return(matched_row$name[1])  # Return the name if a match is found
#   } else {
#     return(NA)  # Return NA if no match is found
#   }
# }

# Testing a new match system
match_cue_id <- function(ts_synapse, current_cue_ids) {
  # 1. Safety Check: Return NA immediately if input timestamp is NA
  if (is.na(ts_synapse)) return(NA)
  
  # Remove any rows where the cue name is NA
  valid_ids <- current_cue_ids %>% filter(!is.na(name))
  if(nrow(valid_ids) == 0) return(NA)
  
  # Compute differences
  diffs <- abs(valid_ids$value - ts_synapse)
  
  # 2. Safety Check: Ensure we have valid numeric differences
  if (all(is.na(diffs))) return(NA)
  
  idx <- which.min(diffs)
  
  # 3. Safety Check: Ensure idx is not empty (length 0)
  if (length(idx) == 0) return(NA)
  
  if (diffs[idx] < 1.5) {
    return(valid_ids$name[idx])
  } else {
    return(NA)
  }
}


# This function takes a file path (event.H5.dir) and reads the HDF5 data, 
# extracts the cue-related information, and returns a list with two elements: CueAvoidZmeans and CueEscapeZmeans.
readZmeanH5_CueAvoidEscape <- function(event.H5.dir, experiment_dir = guppylocation, min_ts = -10, max_ts = 10) {
  dirname <- str_split(event.H5.dir, "/")[[1]][1]
  dirnameparts <- str_split(dirname, "_|-")[[1]]
  Subject.name <- dirnameparts[1]
  Paradigm.name <- dirnameparts[2]
  filename <- str_split(event.H5.dir, "/")[[1]] %>% tail(n = 1)
  fileparts <- strsplit(filename, "_z_score_")[[1]]
  structure.name <- str_remove(fileparts[2], "\\..*")
  event.name <- str_remove(fileparts[1], str_c("_", structure.name))
  abspath_data <- file.path(experiment_dir, event.H5.dir)
  
  #getting escape or avoid associated cues and making a list of cues with the 'name' column after the event that occurs afterwards
  current_cue_ids <- cue_ids %>% 
    filter(Subject == as.numeric(Subject.name), Date == ymd(Paradigm.name)) %>% 
    mutate(name = str_match(name, "(?<=_).*")[, 1])
  
  # DEBUG Print the MED timestamps
  # current_cue_ids %>% arrange(value) %>% print()
  
  #gets mean trial values
  block0_values <- h5read(abspath_data, "df/block0_values") #Reads the dataset "df/block0_values" from the HDF5 file.
  ts_row <- dim(block0_values)[1] - 2 #Determines which row in block0_values contains the timestamps (assuming it is the third-to-last row).
  mean_row <- dim(block0_values)[1] - 1  
  err_row <- dim(block0_values)[1]   
  
  axis0 <- h5read(abspath_data, "df/axis0") #list of ts events occur at
  numeric_axis0 <- axis0[1:(length(axis0) - 3)] #event related rows excluding timestamps, mean, and err.
  numeric_axis0_conv <- numeric_axis0 %>% as.numeric() 
  
  # Check if the first timestamp is valid before subtracting
  if (is.na(numeric_axis0_conv[1])) {
    warning(glue("First timestamp is NA in file {filename}. Skipping timestamp correction."))
    # Fallback: keep original or handle differently. 
    # If the first one is NA, likely the whole axis is text labels and not timestamps.
    numeric_axis0_conv_corrected <- rep(NA, length(numeric_axis0_conv))
  } else {
    numeric_axis0_conv_corrected <- numeric_axis0_conv - numeric_axis0_conv[1]
  }

  output <- tibble(
    numeric_axis0 = numeric_axis0_conv_corrected,
    name = map_chr(numeric_axis0_conv_corrected, ~ match_cue_id(as.numeric(.x), current_cue_ids))
  ) %>% 
    mutate(rn = row_number())
  
  if (dim(output)[1]==0) error(glue("no matching data: {abspath_data}"))
  data_avoid <- tibble()
  data_escape <- tibble()
  
  # Extract the Avoid and Escape blocks WITH ERROR DATA
  avoids_ind <- output %>% mutate(include = ifelse(name == "Avoid", rn, NA_real_)) %>% filter(!is.na(include)) %>% .$include
  if (!is_empty(avoids_ind)) {
    time_filter <- which(block0_values[ts_row, ] >= min_ts & block0_values[ts_row, ] < max_ts)
    avoid_block_mean <- block0_values[avoids_ind, time_filter, drop = FALSE]  # Mean data
    avoid_block_err <- block0_values[c(avoids_ind, err_row), time_filter, drop = FALSE]  # Include error row
    
    # Calculate mean and error
    if (length(avoids_ind) != 1) {
      avoid_mean_values <- colMeans(avoid_block_mean)
      # For error propagation: sqrt(sum(errors^2)) / n for averaging
      avoid_err_values <- sqrt(colSums(avoid_block_err[length(avoids_ind)+1, , drop = FALSE]^2)) / length(avoids_ind)
    } else {
      avoid_mean_values <- avoid_block_mean[1, ]
      avoid_err_values <- avoid_block_err[2, ]  # Error row
    }
    
    data_avoid <- tibble(Subject = Subject.name, Date = ymd(Paradigm.name), Event = event.name, Structure = structure.name) %>% 
      add_column(Zmean = list(avoid_mean_values), Zerr = list(avoid_err_values)) %>% 
      mutate(Event = str_c(Event, "_avoid"))
  }
  
  escapes_ind <- output %>% mutate(include = ifelse(name == "Escape", rn, NA_real_)) %>% filter(!is.na(include)) %>% .$include
  if (!is_empty(escapes_ind)) {
    time_filter <- which(block0_values[ts_row, ] >= min_ts & block0_values[ts_row, ] < max_ts)
    escape_block_mean <- block0_values[escapes_ind, time_filter, drop = FALSE]
    escape_block_err <- block0_values[c(escapes_ind, err_row), time_filter, drop = FALSE]
    
    if (length(escapes_ind) != 1) {
      escape_mean_values <- colMeans(escape_block_mean)
      escape_err_values <- sqrt(colSums(escape_block_err[length(escapes_ind)+1, , drop = FALSE]^2)) / length(escapes_ind)
    } else {
      escape_mean_values <- escape_block_mean[1, ]
      escape_err_values <- escape_block_err[2, ]
    }
    
    data_escape <- tibble(Subject = Subject.name, Date = ymd(Paradigm.name), Event = event.name, Structure = structure.name) %>% 
      add_column(Zmean = list(escape_mean_values), Zerr = list(escape_err_values)) %>% 
      mutate(Event = str_c(Event, "_escape"))
  }
  
  # Extract the filtered block for time axis
  filt_block <- block0_values[, which(block0_values[ts_row, ] >= min_ts & block0_values[ts_row, ] < max_ts)]
  if (!file.exists(glue("{Experiment}_timeaxis_h5.Rds"))) {
    timeaxis_h5 <- filt_block[ts_row, ]
    saveRDS(timeaxis_h5, file = glue("{Experiment}_timeaxis_h5.Rds"))
  }
  
  list(CueAvoidZmeans = data_avoid, CueEscapeZmeans = data_escape)
}

# Wrapper for progress bar
map_df_fast <- function(.x, .f, ...) {
  pb <- progress::progress_bar$new(total = length(.x), format = "[:bar] :percent :elapsed")
  results <- vector("list", length(.x))
  for (i in seq_along(.x)) {
    pb$tick()
    results[[i]] <- .f(.x[[i]], ...)
  }
  results
}

# Call the function with progress and combine results
results <- map_df_fast(event.H5.Files$files, readZmeanH5_CueAvoidEscape)
#Applies the readZmeanH5_CueAvoidEscape function to each file path in event.H5.Files$files.


# Combine the results into the main tables
LBN_Operant_Data.CueAvoidZmeans <- map_dfr(results, "CueAvoidZmeans")
LBN_Operant_Data.CueEscapeZmeans <- map_dfr(results, "CueEscapeZmeans")

Zmeans <- readRDS(file = glue("{Experiment}_Operant_Data.Zmeans.Rds")) %>% 
  bind_rows(LBN_Operant_Data.CueAvoidZmeans, LBN_Operant_Data.CueEscapeZmeans)
saveRDS(Zmeans, file = glue("{Experiment}_Operant_Data.Zmeans.Rds"))










library(rhdf5)
library(tidyverse)
library(progress)
library(glue)
library(lubridate)
library(fs)

# Load helper functions ---------------------------------------------------
readZscoresH5 <- function(event.H5.dir, experiment_dir = guppylocation, min_ts = -10, max_ts = 10, downsample_factor = 20){
  dirname <- str_split(event.H5.dir, "/")[[1]][1]
  dirnameparts <- str_split(dirname, "_|-")[[1]]
  Subject.name <- dirnameparts[1]
  Paradigm.name <- dirnameparts[2]
  filename <- str_split(event.H5.dir, "/")[[1]] %>% tail(n = 1)
  fileparts <- strsplit(filename, "_z_score_")[[1]]
  structure.name <- str_remove(fileparts[2], "\\..*")
  event.name <- str_remove(fileparts[1], str_c("_", structure.name))
  abspath_data <- file.path(experiment_dir, event.H5.dir)
  block0_values <- h5read(abspath_data, "df/block0_values") 
  ts_row <-  dim(block0_values)[1] - 2
  mean_row <- dim(block0_values)[1] - 1
  filt_block <- block0_values[, which(block0_values[ts_row, ] >= min_ts & block0_values[ts_row, ] < max_ts)]
  zMean <- filt_block[mean_row, ]
  
  # Downsample the data
  downsample_indices <- seq(1, ncol(filt_block), by = downsample_factor)
  filt_block <- filt_block[, downsample_indices]
  all_instances <- block0_values[1:(ts_row - 1), downsample_indices]
  
  # Check if all_instances is valid and contains data
  if (is.null(all_instances) || 
      !is.matrix(all_instances) || 
      nrow(all_instances) == 0 || 
      ncol(all_instances) == 0 || 
      all(is.na(all_instances))) {
    warning(glue("No valid instances found for file: {event.H5.dir}"))
    return(tibble())
  }
  
  # Extract date from Paradigm.name
  date_value <- ymd(Paradigm.name)
  
  # Create a dataframe with Subject, Trial, Event, Structure, Z-scores, and Date
  instance_data <- as_tibble(all_instances) %>%
    mutate(Subject = Subject.name, 
           Trial = 1:nrow(all_instances), 
           Event = event.name,
           Structure = structure.name,
           Date = date_value) %>%
    pivot_longer(cols = -c(Subject, Trial, Event, Structure, Date), 
                 names_to = "Time", 
                 values_to = "Zscore") %>%
    pivot_wider(names_from = Time, values_from = Zscore, names_prefix = "Y.")
  
  instance_data
}

# Wrapper for progress bar
map_df_progress <- function(.x, .f, ..., .id = NULL) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(total = length(.x), force = TRUE)
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::map_df(.x, f, ..., .id = .id)
}

# Process the files in batches and save the data
process_files_in_batches <- function(files, batch_size, downsample_factor) {
  total_batches <- ceiling(length(files) / batch_size)
  LBN_Operant_Data.Zscores <- tibble()
  
  for (batch_num in seq_len(total_batches)) {
    start_index <- (batch_num - 1) * batch_size + 1
    end_index <- min(batch_num * batch_size, length(files))
    batch_files <- files[start_index:end_index]
    
    batch_data <- map_df_progress(batch_files, readZscoresH5, downsample_factor = downsample_factor)
    LBN_Operant_Data.Zscores <- bind_rows(LBN_Operant_Data.Zscores, batch_data)
    
    # Save intermediate results
    saveRDS(LBN_Operant_Data.Zscores, file = glue("{Experiment}_Operant_Data.Zscores_batch{batch_num}.Rds"))
    
    # Clear memory
    rm(batch_data)
    gc()
  }
  
  # Save final results
  saveRDS(LBN_Operant_Data.Zscores, file = glue("{Experiment}_Operant_Data.Zscores.Rds"))
  
  # Remove intermediate batch RDS files
  for (batch_num in seq_len(total_batches)) {
    file_delete(glue("{Experiment}_Operant_Data.Zscores_batch{batch_num}.Rds"))
  }
  
  LBN_Operant_Data.Zscores
}

# Main script -------------------------------------------------------------
Experiment <- "Combined"
guppylocation <- r"(C:\Users\bdy2530\Desktop\GuPPy_everything\SynapseTanks\BD_2color-250110-104351)"
synapse_eval_trim <- readRDS(file = "Photometry_Eval.Active_Avoidance")
synapse_eval_trim <- synapse_eval_trim %>% filter(Subject != 9503 & Structure != "DMS")
# synapse_eval_trim <- synapse_eval_trim %>% filter(Subject == 9497) #FOR TESTING PURPOSES

# Get file names ----------------------------------------------------------
guppyfiles <- list.files(guppylocation, recursive = TRUE) %>% 
  as_tibble() %>% 
  rename(files = value) %>% 
  mutate(Filename = str_extract(files, "^[^/]+")) %>% 
  semi_join(synapse_eval_trim, by = join_by(Filename)) %>% dplyr::select(files)

possibleEvents <- c("control_achDLS","control_daDLS","signal_achDLS","signal_daDLS",
                    "cue_on","cue_off" ,"cue_on","cross" ,"escape","avoid" ,"shock", "SkCs")

event.H5.Files <- guppyfiles %>% 
  filter(str_detect(files, str_c("(", paste(possibleEvents, collapse = "|"), ").*z_score.*\\.h.*5")),
         !str_detect(files, "Uncorrected|peak"), !str_detect(files, "daDMS"), !str_detect(files, "achDMS"))

# Process files in batches
LBN_Operant_Data.Zscores <- process_files_in_batches(event.H5.Files$files, batch_size = 60, downsample_factor = 100)
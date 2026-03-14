library(rhdf5)
library(tidyverse)
library(progress)
library(glue)
library(lubridate)
library(fs)
library(googlesheets4)
gs4_deauth()

Experiment <- "Active.Avoidance"
guppylocation <- r"(C:\Users\bdy2530\Downloads\GuPPy_everything\SynapseTanks\AA-combined_1-2-3-4)"

raw_sampRate <- 1017.253
min_ts <- -10
max_ts <- 10
downsample_factor <- 20  # applied during H5 read

meta.data.link <- "https://docs.google.com/spreadsheets/d/1VPx4tm8clOViS7U8v_KSBg8S7oL2NFnLAkSvROfv-6o/edit?usp=sharing"
meta.data <- read_sheet(meta.data.link) %>%
  mutate(Subject = as.numeric(Subject)) %>%
  dplyr::select(Subject, Group, Sex) %>% 
  ungroup() %>%
  distinct()

Operant_Data <- readRDS(file = glue("{Experiment}_Operant_Data.Rds")) %>%
  mutate(Subject = as.numeric(as.character(Subject)),
         Date = date(DateTime)) %>%
  left_join(meta.data, by = "Subject")

meta_Data <- readRDS(file = glue("{Experiment}_meta_Data.Rds"))
meta_data_dateday <- meta_Data %>%
  mutate(Date = as_date(DateTime)) %>%
  group_by(Date) %>%
  summarise(Paradigm.Day = first(Paradigm.Day), .groups = "drop")

cue_ids <- readRDS(file = glue("cue_ids_{Experiment}"))
synapse_eval_trim <- readRDS(file = "Photometry_Eval.Active_Avoidance")
timeaxis_h5 <- readRDS(file = glue("{Experiment}_timeaxis_h5.Rds"))


perc_avoid_per_day <- Operant_Data %>%
  filter(name %in% c("total.avoids", "total.trials.run")) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  mutate(performance = 100 * total.avoids / total.trials.run) %>%
  dplyr::select(Subject, Date, Paradigm.Day, performance) %>%
  distinct()


trial_outcomes <- Operant_Data %>%
  filter(name %in% c("Avoid", "Escape")) %>%
  arrange(Subject, Date, value) %>%
  group_by(Subject, Date) %>%
  mutate(trial_num = cumsum(name %in% c("Avoid", "Escape"))) %>%
  ungroup() %>%
  dplyr::select(Subject, Date, trial_num, name, value) %>%
  rename(trial_outcome_raw = name, event_time = value)

# Latency from avoid.latency / escape.latency per trial
trial_latencies <- Operant_Data %>%
  filter(name %in% c("avoid.latency", "escape.latency")) %>%
  group_by(Subject, Date) %>%
  arrange(Subject, Date, Trial) %>%
  ungroup() %>%
  dplyr::select(Subject, Date, Trial, name, value) %>%
  rename(latency = value) %>%
  mutate(latency_type = case_when(
    name == "avoid.latency" ~ "avoid",
    name == "escape.latency" ~ "escape",
    TRUE ~ NA_character_
  )) %>%
  dplyr::select(-name)


cue_trial_map <- cue_ids %>%
  group_by(Subject, Date) %>%
  arrange(Subject, Date, value) %>%
  mutate(trial_in_session = row_number(),
         trial_outcome = case_when(
           str_detect(name, "Avoid") ~ "avoid",
           str_detect(name, "Escape") ~ "escape",
           TRUE ~ NA_character_
         )) %>%
  ungroup() %>%
  dplyr::select(Subject, Date, trial_in_session, trial_outcome, Paradigm.Day)


readH5_photoDF <- function(event.H5.dir, experiment_dir = guppylocation) {
  # Parse file path for metadata
  
  dirname <- str_split(event.H5.dir, "/")[[1]][1]
  dirnameparts <- str_split(dirname, "_|-")[[1]]
  Subject.name <- dirnameparts[1]
  Paradigm.name <- dirnameparts[2]
  filename <- str_split(event.H5.dir, "/")[[1]] %>% tail(n = 1)
  fileparts <- strsplit(filename, "_z_score_")[[1]]
  structure.name <- str_remove(fileparts[2], "\\..*")
  event.name <- str_remove(fileparts[1], str_c("_", structure.name))
  abspath_data <- file.path(experiment_dir, event.H5.dir)
  
  # Extract sensor and recording location from structure name
  # e.g., "daDMS" → sensor = "da", recLoc = "DMS"
  #        "achDLS" → sensor = "ach", recLoc = "DLS"
  sensor <- str_extract(structure.name, "^(da|ach)")
  recLoc <- str_extract(structure.name, "(DMS|DLS)$")
  
  # Read HDF5 data
  block0_values <- h5read(abspath_data, "df/block0_values")
  ts_row <- dim(block0_values)[1] - 2
  mean_row <- dim(block0_values)[1] - 1
  
  # Filter to time window
  time_filter <- which(block0_values[ts_row, ] >= min_ts & block0_values[ts_row, ] < max_ts)
  filt_block <- block0_values[, time_filter]
  time_axis <- filt_block[ts_row, ]
  
  # Downsample
  ds_indices <- seq(1, length(time_axis), by = downsample_factor)
  time_axis_ds <- time_axis[ds_indices]
  
  # Get all individual trial traces (rows before ts/mean/err)
  n_trials <- ts_row - 1
  if (n_trials < 1) {
    warning(glue("No trial data in {event.H5.dir}"))
    return(tibble())
  }
  
  all_instances <- block0_values[1:n_trials, time_filter, drop = FALSE]
  all_instances_ds <- all_instances[, ds_indices, drop = FALSE]
  
  # Check validity
  
  if (is.null(all_instances_ds) || nrow(all_instances_ds) == 0 || ncol(all_instances_ds) == 0) {
    warning(glue("No valid instances for {event.H5.dir}"))
    return(tibble())
  }
  
  date_value <- ymd(Paradigm.name)
  subject_num <- as.numeric(Subject.name)
  
  # Effective sampling rate after downsampling
  effective_sampRate <- raw_sampRate / downsample_factor
  
  # Determine max length for zero-padding across all trials in this file
  trace_len <- ncol(all_instances_ds)
  # For padding: use a consistent target length. Here we pad to the nearest
  
  # power-of-2 above the trace length, or simply keep original length.
  # Using the length from the full time window / downsample for consistency.
  target_len <- length(ds_indices)
  
  # Build one row per trial
  rows <- tibble(
    mouse     = as.character(subject_num),
    trial     = 1:n_trials,
    event     = event.name,
    recordingLoc = structure.name,
    sensor    = sensor,
    recLoc    = recLoc,
    Date      = date_value,
    sampRate  = effective_sampRate,
    photoTrace = lapply(1:n_trials, function(i) as.numeric(all_instances_ds[i, ])),
    time       = replicate(n_trials, as.numeric(time_axis_ds), simplify = FALSE)
  )
  
  rows
}

# ── 7. Get file list and process ─────────────────────────────────────────────
guppyfiles <- list.files(guppylocation, recursive = TRUE) %>%
  as_tibble() %>%
  rename(files = value) %>%
  mutate(Filename = str_extract(files, "^[^/]+")) %>%
  semi_join(synapse_eval_trim, by = join_by(Filename)) %>%
  dplyr::select(files)

possibleEvents <- c("cue_on", "cue_off", "cross", "escape", "avoid", "shock")

event.H5.Files <- guppyfiles %>%
  filter(str_detect(files, str_c("(", paste(possibleEvents, collapse = "|"), ").*z_score.*\\.h.*5")),
         !str_detect(files, "Uncorrected|peak"))

# Progress bar wrapper
cat(glue("Processing {nrow(event.H5.Files)} H5 files...\n"))
pb <- progress::progress_bar$new(total = nrow(event.H5.Files), force = TRUE)

all_photo_rows <- map_dfr(event.H5.Files$files, function(f) {
  pb$tick()
  tryCatch(
    readH5_photoDF(f),
    error = function(e) {
      warning(glue("Error reading {f}: {e$message}"))
      tibble()
    }
  )
})


# Add Paradigm.Day
all_photo_rows <- all_photo_rows %>%
  left_join(meta_data_dateday, by = "Date") %>%
  rename(dayOnType = Paradigm.Day)

# Add Sex from metadata
sex_lookup <- meta.data %>%
  ungroup() %>%
  mutate(mouse = as.character(Subject)) %>%
  dplyr::select(mouse, sex = Sex) %>%
  distinct()

all_photo_rows <- all_photo_rows %>%
  left_join(sex_lookup, by = "mouse")

# Add performance (% avoidance for that subject × date)
perf_lookup <- perc_avoid_per_day %>%
  mutate(mouse = as.character(Subject)) %>%
  dplyr::select(mouse, Date, performance) %>%
  distinct()

all_photo_rows <- all_photo_rows %>%
  left_join(perf_lookup, by = c("mouse", "Date"))

# Add trial outcome (avoid/escape) and latency
# For events like "avoid", "escape", "shock" the event name IS the outcome
# For "cue_on" we use cue_ids to determine if that cue led to avoid or escape
all_photo_rows <- all_photo_rows %>%
  mutate(`trial outcome` = case_when(
    event == "avoid"  ~ "avoid",
    event == "escape" ~ "escape",
    TRUE ~ NA_character_
  ))

# For cue_on events, try matching trial outcome from cue_ids
cue_outcome_lookup <- cue_trial_map %>%
  mutate(mouse = as.character(Subject)) %>%
  dplyr::select(mouse, Date, trial = trial_in_session, cue_outcome = trial_outcome)

all_photo_rows <- all_photo_rows %>%
  left_join(cue_outcome_lookup, by = c("mouse", "Date", "trial")) %>%
  mutate(`trial outcome` = coalesce(`trial outcome`, cue_outcome)) %>%
  dplyr::select(-cue_outcome)

latency_per_trial <- Operant_Data %>%
  filter(name %in% c("avoid.latency", "escape.latency")) %>%
  mutate(mouse = as.character(Subject)) %>%
  dplyr::select(mouse, Date, Trial, name, latency = value) %>%
  # Pivot so we get one row per trial with both possible latencies
  pivot_wider(names_from = name, values_from = latency, values_fill = NA_real_) %>%
  # Coalesce into a single latency column (a trial is either avoid OR escape, not both)
  mutate(latency = coalesce(avoid.latency, escape.latency)) %>%
  dplyr::select(mouse, Date, trial = Trial, latency)

avoid_trial_mapping <- Operant_Data %>%
  filter(name == "Avoid") %>%
  mutate(mouse = as.character(Subject)) %>%
  group_by(mouse, Date) %>%
  arrange(mouse, Date, Trial) %>%
  mutate(guppy_trial = row_number()) %>%  # This is the H5 trial index for "avoid" events
  ungroup() %>%
  dplyr::select(mouse, Date, guppy_trial, med_trial = Trial)

escape_trial_mapping <- Operant_Data %>%
  filter(name == "Escape") %>%
  mutate(mouse = as.character(Subject)) %>%
  group_by(mouse, Date) %>%
  arrange(mouse, Date, Trial) %>%
  mutate(guppy_trial = row_number()) %>%  # This is the H5 trial index for "escape" events
  ungroup() %>%
  dplyr::select(mouse, Date, guppy_trial, med_trial = Trial)

# For cue_on/cue_off/cross/shock events, all 30 trials are present in the H5,
# so guppy trial index == MED Trial number directly
all_trial_mapping <- Operant_Data %>%
  filter(name %in% c("Cue")) %>%  # Cue exists for every trial
  
  mutate(mouse = as.character(Subject)) %>%
  group_by(mouse, Date) %>%
  arrange(mouse, Date, Trial) %>%
  mutate(guppy_trial = row_number()) %>%
  ungroup() %>%
  dplyr::select(mouse, Date, guppy_trial, med_trial = Trial)

# Step 2: For each row in photoDF, map guppy trial → MED trial → latency
all_photo_rows <- all_photo_rows %>%
  # First, create the guppy_trial index per (mouse, Date, event, recordingLoc)
  # The `trial` column from readH5_photoDF is already 1:n_trials, which IS the guppy trial index
  left_join(
    # For avoid events
    avoid_trial_mapping %>% rename(med_trial_avoid = med_trial),
    by = c("mouse", "Date", "trial" = "guppy_trial"),
    relationship = "many-to-many"
  ) %>%
  # Remove the join if event != avoid (it will produce NAs for non-avoid, which is fine)
  mutate(med_trial_avoid = if_else(event == "avoid", med_trial_avoid, NA_integer_)) %>%
  
  left_join(
    escape_trial_mapping %>% rename(med_trial_escape = med_trial),
    by = c("mouse", "Date", "trial" = "guppy_trial"),
    relationship = "many-to-many"
  ) %>%
  mutate(med_trial_escape = if_else(event == "escape", med_trial_escape, NA_integer_)) %>%
  
  left_join(
    all_trial_mapping %>% rename(med_trial_all = med_trial),
    by = c("mouse", "Date", "trial" = "guppy_trial"),
    relationship = "many-to-many"
  ) %>%
  mutate(med_trial_all = if_else(!event %in% c("avoid", "escape"), med_trial_all, NA_integer_))

# Resolve to a single MED trial number
all_photo_rows <- all_photo_rows %>%
  mutate(med_trial = coalesce(med_trial_avoid, med_trial_escape, med_trial_all)) %>%
  dplyr::select(-med_trial_avoid, -med_trial_escape, -med_trial_all)

# Now join latency using the resolved MED trial number
all_photo_rows <- all_photo_rows %>%
  left_join(latency_per_trial, by = c("mouse", "Date", "med_trial" = "trial")) %>%
  dplyr::select(-med_trial)


# ── 9. Generate padded columns ──────────────────────────────────────────────
# Pad traces and time to a uniform length (max observed trace length)
max_trace_len <- max(sapply(all_photo_rows$photoTrace, length))

all_photo_rows <- all_photo_rows %>%
  mutate(
    padded_time  = map(time, ~ {
      len <- length(.x)
      if (len < max_trace_len) c(.x, rep(0, max_trace_len - len)) else .x
    }),
    padded_trace = map(photoTrace, ~ {
      len <- length(.x)
      if (len < max_trace_len) c(.x, rep(0, max_trace_len - len)) else .x
    })
  )

# ── 10. Select and reorder final columns ─────────────────────────────────────
photoDF <- all_photo_rows %>%
  filter(!is.na(dayOnType), dayOnType != 8) %>%  # remove day 8 if present
  filter(as.numeric(mouse) != 42, as.numeric(mouse) != 44) %>%  # exclude subjects 42, 44
  dplyr::select(
    mouse,
    sex,
    dayOnType,
    trial,
    event,
    latency,
    `trial outcome`,
    recordingLoc,
    performance,
    sampRate,
    photoTrace,
    sensor,
    recLoc,
    time #,
    # padded_time,
    # padded_trace
  ) %>%
  arrange(mouse, dayOnType, trial, event, recordingLoc) 



saveRDS(photoDF, file = glue("{Experiment}_photoDF.Rds"))
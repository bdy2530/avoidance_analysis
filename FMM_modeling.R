library(fastFMM)

# SECTION 1: RUNTIME PATCH FOR fastFMM ---------
# This function applies a "monkey patch" to the 'var_analytic' function within 
# the 'fastFMM' package at runtime. It fixes a known bug where the object 'Z' 
# is not found in specific execution paths.
patch_var_analytic_z <- function() {
  message("Patching fastFMM:::var_analytic to fix 'object Z not found'...")
  
  # 1. Access the internal function from the namespace
  ns <- asNamespace("fastFMM")
  if (!exists("var_analytic", where = ns, inherits = FALSE)) {
    stop("Could not find 'var_analytic' in fastFMM namespace.")
  }
  target_fun <- get("var_analytic", envir = ns)
  
  # 2. Get source code as text
  fun_src <- deparse(body(target_fun), width.cutoff = 500)
  
  # 3. Locate the specific anchor line: "HHat_trim <- NA"
  # We insert "Z <- NULL" immediately after to ensure Z is initialized.
  target_line <- "HHat_trim <- NA"
  
  if (!any(grepl(target_line, fun_src, fixed = TRUE))) {
    warning("Patch Failed: Could not find anchor line 'HHat_trim <- NA'.")
    return()
  }
  
  # 4. Apply the Patch: Replace "HHat_trim <- NA" with "HHat_trim <- NA; Z <- NULL"
  fun_src_patched <- gsub(target_line, "HHat_trim <- NA; Z <- NULL", fun_src, fixed = TRUE)
  
  # 5. Save modified function back to namespace
  body(target_fun) <- parse(text = paste(fun_src_patched, collapse = "\n"))[[1]]
  assignInNamespace("var_analytic", target_fun, ns = "fastFMM")
  
  message("Success: fastFMM:::var_analytic patched. Z initialized to NULL.")
}

# Execute the patch immediately
patch_var_analytic_z()


# SECTION 2: LIBRARIES AND SETUP ------
library(lme4)
library(parallel)
library(cAIC4)
library(magrittr)
library(dplyr)
library(mgcv) 
library(MASS)
library(lsei) 
library(refund)
library(stringr) 
library(Matrix) 
library(mvtnorm) 
library(arrangements) 
library(progress) 
library(ggplot2)
library(gridExtra)
library(Rfast)
library(arrow)
library(data.table)
library(tidyr)
library(doParallel)
library(foreach)
library(lubridate)

# Clear environment variables to ensure a clean start
rm(list=ls())

Experiment <- "Active.Avoidance"

photoDF <- readRDS(file = glue("{Experiment}_photoDF.Rds"))

# Display the reordered data frame
print(head(photoDF))


# SECTION 3: HELPER FUNCTION ------
clean_via_csv <- function(df, filename_base) {
  fname <- paste0(filename_base, ".csv")
  write.csv(df, file = fname, row.names = FALSE)
  
  read_df <- read.csv(fname)
  
  # Filter rows where timeSince_ReNP is NA (if column exists)
  if("timeSince_ReNP" %in% names(read_df)){
    read_df <- read_df[!is.na(read_df$timeSince_ReNP), ]
  }
  return(read_df)
}

# Function: Run FUI Analysis
# Purpose: Fits a Fast Univariate Inference (Functional Data Analysis) model.
# Inputs: 
#   - data: The dataset (wide format).
#   - fixed_formula: The fixed effects portion of the model formula.
#   - output_filename: Path to save the resulting RDS file.
run_fui_analysis <- function(data, fixed_formula, output_filename) {
  # Skip if analysis file already exists
  if (file.exists(output_filename)) {
    message(paste("File exists, skipping:", output_filename))
    return(NULL)
  }
  
  message(paste("Running FUI for:", output_filename))
  
  # CRITICAL FIX: Convert to pure data.frame and remove NAs.
  # Tibbles can cause "subscript out of bounds" errors in fastFMM parallel workers.
  data <- as.data.frame(na.omit(data))
  
  # Construct full mixed-effects formula (adding random intercept for mouse)
  full_formula <- as.formula(paste(fixed_formula, "+ (1 | mouse)"))
  
  # Run Model using fastFMM/refund logic
  # Uses n_cores - 1 to leave resources for OS
  mod <- fui(formula = full_formula,
             data = data,
             analytic = TRUE,
             parallel = TRUE,
             n_cores = detectCores() -1)
  
  # Generate Plot Data from the model object
  plot_obj <- plot_fui(mod,
                       x_rescale = 50, # Rescaling factor for X-axis
                       align_x = 5,    # Alignment point
                       xlab = "Time (s)",
                       return = TRUE)
  
  # Save the plot object (containing results) to RDS
  saveRDS(plot_obj, file = output_filename)
}

# SECTION 4: DATA PRE-PROCESSING ------
# Recode categorical variables for modeling (Male/Escape -> 0, Female/Avoid -> 1)
photoDF <- photoDF %>%
  rename(trialOutcome = 'trial outcome')  %>%
  mutate(
    sex = case_when(
      sex == "Male" ~ 0,
      sex == "Female" ~ 1,
      TRUE ~ as.numeric(sex)
    )
  ) %>% 
  mutate(
    trialOutcome = case_when(
      trialOutcome == "escape" ~ 0,
      trialOutcome == "avoid" ~ 1,
      TRUE ~ as.numeric(trialOutcome)
    )
  ) %>%
  rename(trialOnDay = trial) %>%
  # NEW: Calculate cumulative trials based on fixed structure (30 trials/day)
  # This formula works even if rows are missing
  mutate(totalTrials = (dayOnType - 1) * 30 + trialOnDay)

# Drop unused column
photoDF$recordingLoc <- NULL

events <- unique(photoDF$event)
events <- events[events != "Coff"] # Exclude 'Coff' event

# SECTION 5: DATA RESHAPING ------


# Downsample photoTrace (keep every 2nd point) to reduce computational load
x <- colMeans(do.call(rbind, photoDF$photoTrace), na.rm = TRUE)
photoDF$photoTrace <- lapply(photoDF$photoTrace, function(x) x[seq(1, length(x), by = 2)])

# Explode list-columns into wide format columns (e.g., photoTrace_1, photoTrace_2...)
# This creates the matrix format required by the regression model
photoDF_exploded <- photoDF %>%
  unnest_wider(photoTrace, names_sep = "_") 

# Remove list columns that weren't exploded to prevent errors
if("time" %in% names(photoDF_exploded)) {
  photoDF_exploded$time <- NULL
}

# Create subsets based on Recording Location (DLS/DMS) and Sensor (DA/ACh)
DLS_photoDF <- photoDF_exploded[photoDF_exploded$recLoc == 'DLS', ]
DMS_photoDF <- photoDF_exploded[photoDF_exploded$recLoc == 'DMS', ]


DMS_DA_photoDF  <- photoDF_exploded[photoDF_exploded$recLoc == 'DMS' & photoDF_exploded$sensor == 'da', ]
DMS_ACh_photoDF <- photoDF_exploded[photoDF_exploded$recLoc == 'DMS' & photoDF_exploded$sensor == 'ach', ]
DLS_DA_photoDF  <- photoDF_exploded[photoDF_exploded$recLoc == 'DLS' & photoDF_exploded$sensor == 'da', ]
DLS_ACh_photoDF <- photoDF_exploded[photoDF_exploded$recLoc == 'DLS' & photoDF_exploded$sensor == 'ach', ]

# Sanitize datasets using the CSV read/write hack
df_DMS_da   <- clean_via_csv(DMS_DA_photoDF, "DMS_DA_photoDF")
df_DMS_ach  <- clean_via_csv(DMS_ACh_photoDF, "DMS_ACh_photoDF")
df_DLS_da   <- clean_via_csv(DLS_DA_photoDF, "DLS_DA_photoDF")
df_DLS_ach  <- clean_via_csv(DLS_ACh_photoDF, "DLS_ACh_photoDF")

# Store cleaned dataframes in a named list for iteration
df_list <- list(
  DMS_da  = df_DMS_da,
  DMS_ach = df_DMS_ach,
  DLS_da  = df_DLS_da,
  DLS_ach = df_DLS_ach
)

# create folder for outputs
setwd("FMM_Results")

# SECTION 6: MAIN ANALYSIS LOOP ------
for (name in names(df_list)) {
  for (targetEv in events) {
    # Extract the current dataframe
    current_df <- df_list[[name]]
    
    # Determine dependent variable: 'filtered_corr' for correlation data, 'photoTrace' for photometry
    y_var <- if (grepl("corr", name)) "filtered_corr" else "photoTrace"
    
    # Subset by Event Type
    subset_df <- current_df[current_df$event == targetEv, ]
    
    # Subset by Sex for sex-specific models
    subset_df_M <- subset_df[subset_df$sex == 0,]
    subset_df_F <- subset_df[subset_df$sex == 1,]
    
    # Run Trial Outcome model only for "Cues" event
    if (targetEv == "Cues") {
      run_fui_analysis(subset_df, paste(y_var, "~ trialOutcome"), paste0(targetEv, "_", name, "_FMM_trialOutcome.rds"))
    }
    
    # Run standard battery of models
    # Formulas are dynamically constructed using paste() to insert the correct y_var
    run_fui_analysis(subset_df,   paste(y_var, "~ performance"),       paste0(targetEv, "_", name, "_FFM_perf.rds"))
    run_fui_analysis(subset_df,   paste(y_var, "~ totalTrials"),       paste0(targetEv, "_", name, "_FFM_totTri.rds"))
    run_fui_analysis(subset_df,   paste(y_var, "~ dayOnType"),         paste0(targetEv, "_", name, "_FFM_DOT.rds"))
    run_fui_analysis(subset_df,   paste(y_var, "~ latency"),           paste0(targetEv, "_", name, "_FFM_latency.rds"))
    run_fui_analysis(subset_df,   paste(y_var, "~ sex"),               paste0(targetEv, "_", name, "_FFM_sex.rds"))
    run_fui_analysis(subset_df,   paste(y_var, "~ sex * performance"), paste0(targetEv, "_", name, "_FFM_sex_perf.rds"))
    run_fui_analysis(subset_df,   paste(y_var, "~ sex * latency"), paste0(targetEv, "_", name, "_FFM_sex_lat.rds"))
    run_fui_analysis(subset_df,   paste(y_var, "~ sex * totalTrials"), paste0(targetEv, "_", name, "_FFM_sex_totTri.rds"))
    
    
    
    # # Run sex-specific performance models
    # run_fui_analysis(subset_df_M, paste(y_var, "~ performance"),       paste0(targetEv, "_", name, "_FFM_perf_M.rds"))
    # run_fui_analysis(subset_df_F, paste(y_var, "~ performance"),       paste0(targetEv, "_", name, "_FFM_perf_F.rds"))
    # run_fui_analysis(subset_df_M, paste(y_var, "~ latency"),       paste0(targetEv, "_", name, "_FFM_latency_M.rds"))
    # run_fui_analysis(subset_df_F, paste(y_var, "~ latency"),       paste0(targetEv, "_", name, "_FFM_latency_F.rds"))
    # run_fui_analysis(subset_df_M, paste(y_var, "~ totalTrials"),       paste0(targetEv, "_", name, "_FFM_totalTrials_M.rds"))
    # run_fui_analysis(subset_df_F, paste(y_var, "~ totalTrials"),       paste0(targetEv, "_", name, "_FFM_totalTrials_F.rds"))
    
  }
}


# SECTION 7: RESULT EXPORT ------
rds_files <- list.files(path = "C:/Users/bdy2530/Documents/FMM_Results", pattern = "\\.rds$", full.names = TRUE)
rds_directory <- "C:/Users/bdy2530/Documents/FMM_Results"


for (file in rds_files) {
  data <- readRDS(file)
  file_name <- tools::file_path_sans_ext(basename(file))
    
  # Iterate through objects in the RDS list structure
  for (name in names(data)) {
    df <- data[[name]]
    safe_name <- gsub("[^[:alnum:]_]", "_", name) # Sanitize component name
    csv_file_name <- file.path(rds_directory, paste0(file_name, "_", safe_name, ".csv"))
      
    # Check if object is data frame/matrix AND file doesn't exist before writing
    if ((is.data.frame(df) || is.matrix(df)) && !file.exists(csv_file_name)) {
      write.csv(df, file = csv_file_name, row.names = FALSE)
      }
    }
  }

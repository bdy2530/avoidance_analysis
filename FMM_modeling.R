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

# SECTION 8: PLOT RESULTS ------
library(ggplot2)
library(tools)

# ---- CONFIGURATION ----
rds_directory <- "C:/Users/bdy2530/Documents/FMM_Results"
plot_directory <- file.path(rds_directory, "Plots")
if (!dir.exists(plot_directory)) dir.create(plot_directory, recursive = TRUE)

rds_files <- list.files(path = rds_directory, pattern = "\\.rds$", full.names = TRUE)

# ---- TIME AXIS SETUP ----
# Your original data spans -10 to 10 seconds.
# After downsampling by 2 in your script, you have 509 points.
# Map index 1..509 back to real time:
time_start <- -10  # seconds
time_end   <-  10  # seconds
n_points   <- 509  # number of points after downsampling

real_time <- seq(time_start, time_end, length.out = n_points)

# ---- Plotting function ----
plot_fui_result <- function(rds_path, save_dir) {
  data <- readRDS(rds_path)
  file_label <- file_path_sans_ext(basename(rds_path))
  
  for (term_name in names(data)) {
    df <- data[[term_name]]
    
    if (!is.data.frame(df) || nrow(df) == 0) next
    
    # Match column names
    x_col   <- intersect(names(df), c("s", "x", "argvals", "time"))[1]
    fit_col <- intersect(names(df), c("beta.hat", "fit", "est", "estimate", "coef"))[1]
    pw_lwr  <- intersect(names(df), c("CI.lower.pointwise", "lower", "lwr"))[1]
    pw_upr  <- intersect(names(df), c("CI.upper.pointwise", "upper", "upr"))[1]
    jt_lwr  <- intersect(names(df), c("CI.lower.joint"))[1]
    jt_upr  <- intersect(names(df), c("CI.upper.joint"))[1]
    
    if (is.na(x_col) || is.na(fit_col)) {
      message(paste("  Skipping term", term_name, "- can't find x/fit columns"))
      next
    }
    
    # ---- CONVERT INDEX TO REAL TIME ----
    n_rows <- nrow(df)
    if (n_rows == n_points) {
      # Exact match — use precomputed time vector
      time_vec <- real_time
    } else {
      # Different length (safety fallback) — recompute
      message(paste("  Note:", term_name, "has", n_rows, "rows, expected", n_points, "- rescaling anyway"))
      time_vec <- seq(time_start, time_end, length.out = n_rows)
    }
    
    # Build plotting data frame with REAL time
    plot_df <- data.frame(
      x   = time_vec,
      fit = df[[fit_col]]
    )
    
    has_pw <- !is.na(pw_lwr) && !is.na(pw_upr)
    has_jt <- !is.na(jt_lwr) && !is.na(jt_upr)
    
    if (has_pw) {
      plot_df$pw_lower <- df[[pw_lwr]]
      plot_df$pw_upper <- df[[pw_upr]]
    }
    if (has_jt) {
      plot_df$jt_lower <- df[[jt_lwr]]
      plot_df$jt_upper <- df[[jt_upr]]
    }
    
    safe_term <- gsub("[^[:alnum:]_]", "_", term_name)
    
    # ---- Build the plot ----
    p <- ggplot(plot_df, aes(x = x, y = fit)) +
      # Joint CI band (wider, lighter)
      { if (has_jt) geom_ribbon(aes(ymin = jt_lower, ymax = jt_upper),
                                fill = "steelblue", alpha = 0.15) } +
      # Pointwise CI band (narrower, darker)
      { if (has_pw) geom_ribbon(aes(ymin = pw_lower, ymax = pw_upper),
                                fill = "steelblue", alpha = 0.3) } +
      # Effect estimate
      geom_line(color = "steelblue", linewidth = 1) +
      # ---- AXIS LINES AT ORIGIN ----
    # Horizontal: y = 0 (no effect reference)
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5) +
      # Vertical: x = 0 (event onset)
      geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
      # ---- LABELS ----
    labs(
      title    = file_label,
      subtitle = paste("Term:", term_name),
      x        = "Time from event (s)",
      y        = "Coefficient Estimate (ΔF/F)"
    ) +
      # ---- CLEAN AXIS SCALING ----
    scale_x_continuous(
      breaks = seq(time_start, time_end, by = 2),  # tick every 2 seconds
      limits = c(time_start, time_end)
    ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title    = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 10, color = "grey40")
      )
    
    # Save
    out_file <- file.path(save_dir, paste0(file_label, "_", safe_term, ".png"))
    ggsave(out_file, plot = p, width = 8, height = 5, dpi = 300)
    message(paste("  Saved:", out_file))
  }
}

# ---- Loop through all RDS files ----
for (f in rds_files) {
  message(paste("Processing:", basename(f)))
  plot_fui_result(f, plot_directory)
}

message("Done! All plots saved to: ", plot_directory)

# SECTION 9: PRIORITY LISTING ------
library(tools)
library(dplyr)

# ---- CONFIGURATION ----
rds_directory <- "C:/Users/bdy2530/Documents/FMM_Results"
plot_directory <- file.path(rds_directory, "Plots")

rds_files <- list.files(path = rds_directory, pattern = "\\.rds$", full.names = TRUE)

# Time mapping
time_start <- -10
time_end   <-  10
n_points   <- 509
real_time  <- seq(time_start, time_end, length.out = n_points)

# ---- SCORE EVERY TERM IN EVERY FILE ----
results <- list()

for (f in rds_files) {
  data <- readRDS(f)
  file_label <- file_path_sans_ext(basename(f))
  
  for (term_name in names(data)) {
    df <- data[[term_name]]
    if (!is.data.frame(df) || nrow(df) == 0) next
    if (term_name == "(Intercept)") next
    
    fit_col <- intersect(names(df), c("beta.hat", "fit", "est"))[1]
    jt_lwr  <- intersect(names(df), c("CI.lower.joint"))[1]
    jt_upr  <- intersect(names(df), c("CI.upper.joint"))[1]
    pw_lwr  <- intersect(names(df), c("CI.lower.pointwise"))[1]
    pw_upr  <- intersect(names(df), c("CI.upper.pointwise"))[1]
    
    if (is.na(fit_col)) next
    
    n_rows <- nrow(df)
    time_vec <- if (n_rows == n_points) real_time else seq(time_start, time_end, length.out = n_rows)
    
    # ---- Significance at each timepoint ----
    if (!is.na(pw_lwr) && !is.na(pw_upr)) {
      sig_pw <- (df[[pw_lwr]] * df[[pw_upr]]) > 0
    } else {
      sig_pw <- rep(FALSE, n_rows)
    }
    
    if (!is.na(jt_lwr) && !is.na(jt_upr)) {
      sig_joint <- (df[[jt_lwr]] * df[[jt_upr]]) > 0
    } else {
      sig_joint <- rep(FALSE, n_rows)
    }
    
    # ---- Pre / Post breakdown ----
    pre_idx  <- time_vec < 0
    post_idx <- time_vec > 0
    
    n_sig_pw       <- sum(sig_pw, na.rm = TRUE)
    n_sig_pw_pre   <- sum(sig_pw[pre_idx], na.rm = TRUE)
    n_sig_pw_post  <- sum(sig_pw[post_idx], na.rm = TRUE)
    n_sig_joint    <- sum(sig_joint, na.rm = TRUE)
    n_sig_jt_pre   <- sum(sig_joint[pre_idx], na.rm = TRUE)
    n_sig_jt_post  <- sum(sig_joint[post_idx], na.rm = TRUE)
    
    pct_sig_pw       <- round(100 * n_sig_pw / n_rows, 1)
    pct_sig_pw_pre   <- round(100 * n_sig_pw_pre / sum(pre_idx), 1)
    pct_sig_pw_post  <- round(100 * n_sig_pw_post / sum(post_idx), 1)
    pct_sig_joint    <- round(100 * n_sig_joint / n_rows, 1)
    pct_sig_jt_pre   <- round(100 * n_sig_jt_pre / sum(pre_idx), 1)
    pct_sig_jt_post  <- round(100 * n_sig_jt_post / sum(post_idx), 1)
    
    # ---- Effect sizes ----
    peak_effect <- round(max(abs(df[[fit_col]]), na.rm = TRUE), 4)
    
    if (n_sig_pw > 0) {
      peak_effect_sig <- round(max(abs(df[[fit_col]][sig_pw]), na.rm = TRUE), 4)
    } else {
      peak_effect_sig <- 0
    }
    
    # ---- Direction ----
    if (n_sig_pw > 0) {
      sig_vals <- df[[fit_col]][sig_pw]
      n_pos <- sum(sig_vals > 0)
      n_neg <- sum(sig_vals < 0)
      direction <- if (n_pos > 0 & n_neg > 0) "mixed" else if (n_pos > 0) "positive" else "negative"
    } else {
      direction <- "—"
    }
    
    # ---- Characterize WHERE significance occurs ----
    sig_times_pw <- time_vec[sig_pw]
    if (length(sig_times_pw) > 0) {
      has_pre  <- any(sig_times_pw < 0)
      has_post <- any(sig_times_pw > 0)
      sig_location <- if (has_pre & has_post) "pre + post" else if (has_pre) "pre only" else "post only"
      sig_time_range <- paste0("[", round(min(sig_times_pw), 2), "s to ", 
                               round(max(sig_times_pw), 2), "s]")
    } else {
      sig_location <- "—"
      sig_time_range <- "—"
    }
    
    # ---- Parse filename ----
    parts <- strsplit(file_label, "_FFM_|_FMM_")[[1]]
    if (length(parts) == 2) {
      model_part <- parts[2]
      desc_parts <- strsplit(parts[1], "_")[[1]]
      n_desc <- length(desc_parts)
      sensor <- if (n_desc >= 1) desc_parts[n_desc] else "?"
      region <- if (n_desc >= 2) desc_parts[n_desc - 1] else "?"
      event  <- if (n_desc >= 3) paste(desc_parts[1:(n_desc - 2)], collapse = "_") else "?"
    } else {
      event <- file_label; region <- "?"; sensor <- "?"; model_part <- "?"
    }
    
    safe_term <- gsub("[^[:alnum:]_]", "_", term_name)
    png_name  <- paste0(file_label, "_", safe_term, ".png")
    
    results[[length(results) + 1]] <- data.frame(
      event, region, sensor, model = model_part, term = term_name,
      direction, sig_location,
      pct_sig_pw, pct_sig_pw_pre, pct_sig_pw_post,
      pct_sig_joint, pct_sig_jt_pre, pct_sig_jt_post,
      peak_effect, peak_effect_sig, sig_time_range,
      png = png_name, file = file_label,
      stringsAsFactors = FALSE
    )
  }
}

# ---- COMBINE AND RANK ----
# Primary sort: overall pointwise significance (pre + post both count)
priority_df <- bind_rows(results) %>%
  arrange(desc(pct_sig_pw), desc(peak_effect_sig))

# ---- EXPLORATORY TIERS (pre and post both valued equally) ----
priority_df <- priority_df %>%
  mutate(
    tier_exploratory = case_when(
      pct_sig_pw >= 20                                      ~ "A - Strong",
      pct_sig_pw >= 10                                      ~ "B - Moderate",
      pct_sig_pw_pre >= 10 | pct_sig_pw_post >= 10         ~ "C - One-sided",   # strong in one half
      pct_sig_pw > 0                                        ~ "D - Marginal",
      TRUE                                                  ~ "E - Nothing"
    ),
    confirmed_by_joint = case_when(
      pct_sig_joint >= 10 ~ "YES",
      pct_sig_joint >   0 ~ "partial",
      TRUE                ~ "no"
    )
  )

# ---- PRINT SUMMARY ----
cat("\n=============================================\n")
cat("  EXPLORATORY PRIORITY RANKING (Pointwise CI)\n")
cat("=============================================\n\n")

cat("Tier counts:\n")
print(table(priority_df$tier_exploratory))
cat("\n")

for (tier in c("A - Strong", "B - Moderate", "C - One-sided")) {
  cat(paste0("\n--- ", tier, " ---\n\n"))
  tier_df <- priority_df[priority_df$tier_exploratory == tier, ]
  if (nrow(tier_df) > 0) {
    print_cols <- c("event", "region", "sensor", "term", "direction", "sig_location",
                    "pct_sig_pw_pre", "pct_sig_pw_post", "pct_sig_pw",
                    "peak_effect_sig", "sig_time_range", "confirmed_by_joint", "png")
    print(tibble::as_tibble(tier_df[, print_cols]), n = Inf, width = 300)
  } else {
    cat("  (none)\n")
  }
}

cat("\n\nTier D (marginal):", sum(priority_df$tier_exploratory == "D - Marginal"), "plots\n")
cat("Tier E (nothing):", sum(priority_df$tier_exploratory == "E - Nothing"), "plots — ignore these.\n")

# ---- SAVE CSVs ----
exploratory_csv <- file.path(rds_directory, "priority_1_exploratory.csv")
write.csv(priority_df, file = exploratory_csv, row.names = FALSE)
cat("\nExploratory ranking saved to:", exploratory_csv, "\n")

pub_df <- priority_df %>%
  filter(confirmed_by_joint %in% c("YES", "partial")) %>%
  arrange(desc(pct_sig_joint), desc(peak_effect_sig))
pub_csv <- file.path(rds_directory, "priority_2_publication.csv")
write.csv(pub_df, file = pub_csv, row.names = FALSE)
cat("Publication ranking saved to:", pub_csv, "\n")
cat("  →", sum(pub_df$confirmed_by_joint == "YES"), "fully confirmed by joint CI\n")
cat("  →", sum(pub_df$confirmed_by_joint == "partial"), "partially confirmed\n")

# ---- COPY TOP RESULTS ----
explore_folder <- file.path(plot_directory, "Explore_Top")
if (!dir.exists(explore_folder)) dir.create(explore_folder)

top_explore <- priority_df[priority_df$tier_exploratory %in% c("A - Strong", "B - Moderate"), ]
copied <- 0
for (i in seq_len(nrow(top_explore))) {
  src <- file.path(plot_directory, top_explore$png[i])
  dst <- file.path(explore_folder, top_explore$png[i])
  if (file.exists(src) && !file.exists(dst)) { file.copy(src, dst); copied <- copied + 1 }
}
cat("Copied", copied, "exploratory top plots to:", explore_folder, "\n")

pub_folder <- file.path(plot_directory, "Publication_Ready")
if (!dir.exists(pub_folder)) dir.create(pub_folder)

top_pub <- pub_df[pub_df$confirmed_by_joint == "YES", ]
copied <- 0
for (i in seq_len(nrow(top_pub))) {
  src <- file.path(plot_directory, top_pub$png[i])
  dst <- file.path(pub_folder, top_pub$png[i])
  if (file.exists(src) && !file.exists(dst)) { file.copy(src, dst); copied <- copied + 1 }
}
cat("Copied", copied, "publication-ready plots to:", pub_folder, "\n")
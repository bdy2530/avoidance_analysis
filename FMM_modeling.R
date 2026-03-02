library(fastFMM)
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
library(feather)
library(data.table)
library(tidyr)
library(doParallel)
library(parallel)
library(foreach)

# Initializing --------------
rm(list=ls())
#TAIL
# Load necessary libraries
library(dplyr)
library(lubridate)

# Read the data
photoDF <- readRDS("Active.Avoidance_Operant_Data.Zscores.Rds")
meta_data <- readRDS("Active.Avoidance_meta_Data.Rds")

# Extract the date part from DateTime in meta_data
meta_data <- meta_data %>%
  mutate(Date = as_date(DateTime))

# Summarize meta_data to ensure unique dates
meta_data_unique <- meta_data %>%
  group_by(Date) %>%
  summarise(Paradigm.Day = first(Paradigm.Day))

# Merge the data frames by matching the Date column
merged_data <- photoDF %>%
  left_join(meta_data_unique, by = "Date")

# Reorder the columns
important_columns <- c("Subject", "Trial", "Event", "Structure", "Date", "Paradigm.Day")
data_columns <- setdiff(names(merged_data), important_columns)

# Combine important columns and data columns
merged_data <- merged_data %>%
  dplyr::select(all_of(important_columns), all_of(data_columns))

# Assign the merged data back to photoDF and remove the temporary variable
photoDF <- merged_data
rm(merged_data)
colnames(photoDF) <- gsub("\\.", "_", colnames(photoDF))
photoDF <- filter(photoDF, !Paradigm_Day == 8)

# Display the reordered data frame
print(head(photoDF))

# FMM for cueon ----------------------------------------------------
event <- 'cue_on'
cueOn_photoDF <- photoDF[photoDF$Event == event, ]
gc()

#x <- colMeans(do.call(rbind, cueOn_photoDF$trimTime), na.rm = TRUE)
#cueOn_photoDF <- cueOn_photoDF[, !(names(cueOn_photoDF) %in% c('recordingLoc', 'event', 'sesType', 'trimTime'))]

# cueOn_photoDF$trimTrace <- lapply(cueOn_photoDF$trimTrace, function(x) x[seq(1, length(x), by = 5)])
# cueOn_photoDF$trimTime <- lapply(cueOn_photoDF$trimTime, function(x) x[seq(1, length(x), by = 5)])


# Step 1: Determine the maximum length of photoTrace
#max_length <- max(sapply(subset_dat$photoTrace, length))


write.csv(cueOn_photoDF, file='cueOn_photoDF.csv', row.names = FALSE)
df <- read.csv('cueOn_photoDF.csv')
#df <- df %>%
#  mutate(group = ifelse(group == "ach", 1, 0))

achDLS_DF <- df[df$Structure == "achDLS", ]
daDLS_DF <- df[df$Structure == "daDLS", ]
achDMS_DF <- df[df$Structure == "achDMS", ]
daDMS_DF <- df[df$Structure == "daDMS", ]

# Register parallel backend
num_cores <- 8
Sys.setenv(OMP_NUM_THREAD = num_cores)
Sys.setenv(OPENBLAS_NUM_THREADS = num_cores)
cl <- makeCluster(4, type = "PSOCK")  # 4 clusters for 4 models
registerDoParallel(cl)
options(mc.cores = num_cores)

# Run models in parallel
models <- foreach(data_subset = list(achDLS_DF, daDLS_DF, achDMS_DF, daDMS_DF), 
                  .packages = c("fastFMM"), .options.multicore = list(preschedule = FALSE)) %dopar% {
                        fui(Y_V ~ Paradigm_Day + Trial + (1 | Subject),
                            data = data_subset,
                            parallel = TRUE,  
                            analytic = FALSE)
                  }

# Stop the cluster
stopCluster(cl)

# Extract results
mod_achDLS <- models[[1]]
mod_daDLS <- models[[2]]
mod_achDMS <- models[[3]]
mod_daDMS <- models[[4]]


# Plot the fitted model
fui_plot_ach <- plot_fui(mod_ach,           # model fit object
                     x_rescale = 10, # rescale x-axis to sampling rate
                     align_x = 10,
                     xlab = "Time (s)",
                     #main = paste(file, " timeToFall"), ,    # use file name as the title
                     return = TRUE)
saveRDS(fui_plot_ach, file='FMM_Paradigm.Day_ach_cueon')


fui_plot_da <- plot_fui(mod_da,           # model fit object
                             x_rescale = 10, # rescale x-axis to sampling rate
                             align_x = 10,
                            xlab = "Time (s)",
                            #main = paste(file, " timeToFall"), ,    # use file name as the title
                            return = TRUE)
saveRDS(fui_plot_da, file='FMM_Paradigm.Day_da_cueon')

# FMM for shock ----------------------------------------------------
event <- 'shock'
shock_photoDF <- photoDF[photoDF$Event == event, ]
gc()

write.csv(shock_photoDF, file='shock_photoDF.csv', row.names = FALSE)
df <- read.csv('shock_photoDF.csv')

achDLS_DF <- df[df$Structure == "achDLS", ]
daDLS_DF <- df[df$Structure == "daDLS", ]

# Register parallel backend
num_cores <- 8
Sys.setenv(OMP_NUM_THREAD = num_cores)
Sys.setenv(OPENBLAS_NUM_THREADS = num_cores)
cl <- makeCluster(2, type = "PSOCK")  # 2 clusters for 2 models
registerDoParallel(cl)
options(mc.cores = num_cores)

# Run models in parallel
models <- foreach(data_subset = list(achDLS_DF, daDLS_DF), 
                  .packages = c("fastFMM"), .options.multicore = list(preschedule = FALSE)) %dopar% {
                    fui(Y_V ~ Paradigm_Day + Trial + (1 | Subject),
                        data = data_subset,
                        parallel = TRUE,  
                        analytic = FALSE)
                  }

# Stop the cluster
stopCluster(cl)

# Extract results
mod_ach <- models[[1]]
mod_da <- models[[2]]

# Plot the fitted model
fui_plot_ach <- plot_fui(mod_ach,           # model fit object
                          x_rescale = 10, # rescale x-axis to sampling rate
                          align_x = 10,
                          xlab = "Time (s)",
                          #main = paste(file, " timeToFall"), ,    # use file name as the title
                          return = TRUE)
saveRDS(fui_plot_ach, file='FMM_Paradigm.Day_ach_shock')

fui_plot_da <- plot_fui(mod_da,           # model fit object
                         x_rescale = 10, # rescale x-axis to sampling rate
                         align_x = 10,
                         xlab = "Time (s)",
                         #main = paste(file, " timeToFall"), ,    # use file name as the title
                         return = TRUE)
saveRDS(fui_plot_da, file='FMM_Paradigm.Day_da_shock')


# FMM for avoid ----------------------------------------------------
event <- 'avoid'
avoid_photoDF <- photoDF[photoDF$Event == event, ]
gc()

write.csv(avoid_photoDF, file='avoid_photoDF.csv', row.names = FALSE)
df <- read.csv('avoid_photoDF.csv')
achDLS_DF <- df[df$Structure == "achDLS", ]
daDLS_DF <- df[df$Structure == "daDLS", ]

# Register parallel backend
# Register parallel backend
num_cores <- 8
Sys.setenv(OMP_NUM_THREAD = num_cores)
Sys.setenv(OPENBLAS_NUM_THREADS = num_cores)
cl <- makeCluster(2, type = "PSOCK")  # 2 clusters for 2 models
registerDoParallel(cl)
options(mc.cores = num_cores)

# Run models in parallel
models <- foreach(data_subset = list(achDLS_DF, daDLS_DF), 
                  .packages = c("fastFMM"), .options.multicore = list(preschedule = FALSE)) %dopar% {
                    fui(Y_V ~ Paradigm_Day + Trial + (1 | Subject),
                        data = data_subset,
                        parallel = TRUE,  
                        analytic = FALSE)
                  }

# Stop the cluster
stopCluster(cl)

# Extract results
mod_ach <- models[[1]]
mod_da <- models[[2]]

# Plot the fitted model
fui_plot_ach <- plot_fui(mod_ach,           # model fit object
                          x_rescale = 10, # rescale x-axis to sampling rate
                          align_x = 10,
                          xlab = "Time (s)",
                          #main = paste(file, " timeToFall"), ,    # use file name as the title
                          return = TRUE)
saveRDS(fui_plot_ach, file='FMM_Paradigm.Day_ach_avoid')

fui_plot_da <- plot_fui(mod_da,           # model fit object
                         x_rescale = 10, # rescale x-axis to sampling rate
                         align_x = 10,
                         xlab = "Time (s)",
                         #main = paste(file, " timeToFall"), ,    # use file name as the title
                         return = TRUE)
saveRDS(fui_plot_da, file='FMM_Paradigm.Day_da_avoid')

# FMM for escape ----------------------------------------------------
event <- 'escape'
escape_photoDF <- photoDF[photoDF$Event == event, ]
gc()

write.csv(escape_photoDF, file='escape_photoDF.csv', row.names = FALSE)
df <- read.csv('escape_photoDF.csv')
achDLS_DF <- df[df$Structure == "achDLS", ]
daDLS_DF <- df[df$Structure == "daDLS", ]

# Register parallel backend
# Register parallel backend
num_cores <- 8
Sys.setenv(OMP_NUM_THREAD = num_cores)
Sys.setenv(OPENBLAS_NUM_THREADS = num_cores)
cl <- makeCluster(2, type = "PSOCK")  # 2 clusters for 2 models
registerDoParallel(cl)
options(mc.cores = num_cores)

# Run models in parallel
models <- foreach(data_subset = list(achDLS_DF, daDLS_DF), 
                  .packages = c("fastFMM"), .options.multicore = list(preschedule = FALSE)) %dopar% {
                    fui(Y_V ~ Paradigm_Day + Trial + (1 | Subject),
                        data = data_subset,
                        parallel = TRUE,  
                        analytic = FALSE)
                  }

# Stop the cluster
stopCluster(cl)

# Extract results
mod_ach <- models[[1]]
mod_da <- models[[2]]

# Plot the fitted model
fui_plot_ach <- plot_fui(mod_ach,           # model fit object
                         x_rescale = 10, # rescale x-axis to sampling rate
                         align_x = 10,
                         xlab = "Time (s)",
                         #main = paste(file, " timeToFall"), ,    # use file name as the title
                         return = TRUE)
saveRDS(fui_plot_ach, file='FMM_Paradigm.Day_ach_escape')

fui_plot_da <- plot_fui(mod_da,           # model fit object
                        x_rescale = 10, # rescale x-axis to sampling rate
                        align_x = 10,
                        xlab = "Time (s)",
                        #main = paste(file, " timeToFall"), ,    # use file name as the title
                        return = TRUE)
saveRDS(fui_plot_da, file='FMM_Paradigm.Day_da_escape')

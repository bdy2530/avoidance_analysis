# to import movement data from sleap/simba into R

library(data.table)

file_path <- "C:/Users/bdy2530/Desktop/simba-activeavoidance-proj/activeavoidance/project_folder/logs/Time_bins_0.1s_movement_results_20260512153226.csv"
df <- fread(file_path)
velocity_df <- df[MEASUREMENT == "Velocity (cm/s)"]

percentile_98 <- quantile(velocity_df$VALUE, 0.98, na.rm = TRUE)
threshold <- percentile_98 

velocity_df[VALUE > threshold, VALUE := NA]
velocity_df[, Time_sec := `TIME BIN #` * 0.1]

fwrite(velocity_df, "cleaned_velocity_data.csv")

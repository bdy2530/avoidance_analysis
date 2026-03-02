library(rhdf5)
library(tidyverse)
library(progress)
library(googlesheets4)
library(ggrepel)
library(glue)
library(lubridate)
library(broom)
library(purrr)
library(ggprism)
gs4_deauth()

#### SETUP FOR ALL -----------------------------------------------------------
# Loading data and presets/functions 
Experiment <- "Active.Avoidance"

#pull in Subject metadata
meta.data.link <- "https://docs.google.com/spreadsheets/d/1VPx4tm8clOViS7U8v_KSBg8S7oL2NFnLAkSvROfv-6o/edit?usp=sharing"
meta.data <- read_sheet(meta.data.link) %>%
  mutate(Subject = as.numeric(Subject)) %>% 
  rowwise() %>% 
  dplyr::select(Subject, Group, Sex)

#behavior data to match Dates to experiments
Operant_Data <- readRDS(file = glue("{Experiment}_Operant_Data.Rds"))
Operant_Data <- Operant_Data %>% 
  mutate(Subject = as.numeric(as.character(Subject)),  # Convert factor/character to numeric properly
         Date = date(DateTime))
Operant_metadata <- Operant_Data %>% 
  mutate(Date = date(DateTime)) %>% 
  dplyr::select(Subject, Date, Paradigm.Day) %>% 
  distinct() %>% 
  arrange(Subject, Paradigm.Day) %>% 
  left_join(meta.data) %>% filter(Subject != 42) %>% filter(Subject != 44)

#synapse data:
# synapse_eval_trim <- readRDS(file = "Photometry_Eval.Active_Avoidance")
timeaxis_h5 <- readRDS(file = glue("{Experiment}_timeaxis_h5.Rds"))
Zmeans <- readRDS(file = glue("{Experiment}_Operant_Data.Zmeans.Rds"))

#subset time axis by index timeaxis_subsample_index - downsample by 20 for plotting
timeaxis_subsample_index <- seq(10,length(timeaxis_h5),20) %>% as.integer()
timeaxis <- timeaxis_h5[timeaxis_subsample_index]
#formatting with rownumbers/samples:
timeaxisTable <- tibble(Time = timeaxis) %>%
  dplyr::mutate(TimeKey = row_number()) %>%
  relocate(TimeKey)

#for counting later
forGroups <- meta.data %>%
  dplyr::select(Subject, Group, Sex) %>%
  distinct()

#filter out unused events prior to more analysis and add metadata+
Zmeans <- Zmeans %>% 
  mutate(Subject = as.numeric(as.character(Subject)))

Zmeans <-  Zmeans %>% 
  filter(!str_detect(Event, "timer")) %>% 
  left_join(Operant_metadata, by = c("Subject", "Date"))


Zmeans_unpack <- Zmeans %>%
  rename(zScore = Zmean) %>%
  rowwise() %>%
  mutate(plotParadigm = Paradigm.Day, #plotParaName(Paradigm),
         zScore = unlist(zScore) %>% .[timeaxis_subsample_index] %>% list(),
         zpack = list(tibble(zScore = unlist(zScore),
                             Time = timeaxisTable$Time,
                             Timekey = timeaxisTable$TimeKey))) %>%
  select(!zScore) %>%
  ungroup() %>% 
  unnest(zpack)

# Identify and save the rows with non-finite values
non_finite_rows <- Zmeans_unpack %>% 
  filter(!is.finite(Time) | !is.finite(zScore))

# Select the columns to save
non_finite_metadata <- non_finite_rows %>%
  dplyr::select(Subject, Date, Event, Structure, `Paradigm.Day`, Group, Sex) %>% 
  distinct()

# Remove the non-finite rows from the original data frame
Zmeans_unpack <- Zmeans_unpack %>%
  filter(is.finite(Time) & is.finite(zScore))


# Plotting prep and functions 
sem2 <-function(x){ tribble( #try implementing with stat_summary as before in commented section
  ~ymin,                           ~y,               ~ymax,
  mean(x)-sd(x)/sqrt(length(x)), mean(x), mean(x)+sd(x)/sqrt(length(x))
)
}
#sem for rows
sem2Row <-function(x){ tribble( #try implementing with stat_summary as before in commented section
  ~ymin,                           ~y,               ~ymax,
  rowMeans(x)-rowSds(x)/sqrt(dim(x)[2]), rowMeans(x), rowMeans(x)+rowSds(x)/sqrt(dim(x)[2])
)
}

plot.name.mutate_Active.Avoidance  <- function(df, column.name.out = plotParadigm){
  df <- df %>% 
    mutate({{column.name.out}} := str_c("Day ", as.character(plotParadigm)),
           {{column.name.out}} := factor(plotParadigm, 
                                         levels = df$plotParadigm %>% unique() %>% sort()  %>% str_c("Day ", .), 
                                         ordered = TRUE)
    )
  return(df)
}

# My_Theme1 = theme(
#   legend.position = "top",
#   legend.title=element_blank(),
#   plot.caption = element_text(hjust = 0, face= "italic", size = 8),
#   plot.caption.position = "plot",
#   strip.text = element_text(size = 8),
#   title = element_text(size = 10),
#   plot.title = element_text(hjust = 0.5),
#   axis.title.x = element_text(size = 8),
#   axis.text.x = element_text(size = 6),
#   axis.title.y = element_text(size = 8),
#   axis.text.y = element_text(size = 6)
# )

My_Theme1 <- theme_prism(
  base_fontface = "plain",
  base_line_size = 0.7,
  base_rect_size = 0.7
) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.caption = element_text(hjust = 0, face = "italic", size = 8),
    plot.caption.position = "plot",
    strip.text = element_text(size = 8),
    title = element_text(size = 10),
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 8),
    axis.text.x = element_text(size = 6),
    axis.title.y = element_text(size = 8),
    axis.text.y = element_text(size = 6)
  )

# #### COLLAPSING GROUPS - actually useful so it will stay for now-------------------------------------------------------
# mypath <- file.path(getwd(),"guppy_plots",  glue("{Experiment } {Sys.Date()}"), "horizontal time", "event_traces_over_days_collapse_groups")
# if (!dir.exists(mypath)){
#   dir.create(mypath, recursive = TRUE)
# }
# 
# TracePlotsSelect <-  list()
# Structures <-  c('achDLS', 'daDLS', 'achDMS', 'daDMS') #ADDED IT BACK!!!! had previously removed achDMS and daDMS cuz I flopped...
# Session <- c(1:8)
# Event_plotnames <- c("Avoid Shock", "Escape Shock", "Shock", "Cue On", "Cue Off", "Cross (generic)")
# Events <- c("avoid", "escape", "shock", "cue_on", "cue_off", "cross")#removed cue_on_avoid and cue_on_escape cuz buggy and don't have time to fix
# 
# for (i in 1:length(Structures)){
#   i2 <- 1
#   for (i2 in 1:length(Events)){
#     Zmeans_filt <- Zmeans_unpack %>%
#       filter(Structure == Structures[i], plotParadigm %in% Session, Event == Events[i2]) %>%
#       plot.name.mutate_Active.Avoidance(.) %>% 
#       group_by(Group, plotParadigm, Time) 
#     
#     #get n's
#     Zmeans_filt_n <- Zmeans_filt %>% ungroup() %>%  
#       select(!c(zScore, Time, Timekey)) %>% 
#       distinct() %>% 
#       group_by(plotParadigm, Event, Group, Sex) %>% 
#       count() %>% 
#       mutate(n = as.character(n))
#     
#     FigName <- str_c(Structures[i]," - ", Events[i2],  " Collapsing Groups")
#     TracePlotsSelect[[FigName]] <- ggplot(data = Zmeans_filt, mapping = aes(x = Time, y = zScore)) +
#       geom_path(stat = "summary", fun.data = sem2, linewidth = .2) +
#       geom_ribbon(stat = "summary", fun.data = sem2, alpha = 0.4, linetype = 0) +
#       geom_text(data = Zmeans_filt_n, mapping = aes(x = Inf, y = Inf, label = n), size = 3, hjust = 1, vjust = 1) +
#       geom_vline(xintercept = 0, alpha = 0.5, lwd = 0.1)+
#       coord_cartesian(xlim=c(-2, 3), ylim=c(-2, 2))+
#       scale_x_continuous(breaks = c(-2, 0, 3)) +
#       labs(title = str_c(Structures[i]," ", Event_plotnames[i2]),
#            x = "Time (s)", y = "z-score", caption = "Line plots are mean with SEM error ribbons.") +
#       My_Theme1 + 
#       facet_grid(rows = vars(Sex), cols = vars(plotParadigm), margins = TRUE)
#     ggsave(plot = TracePlotsSelect[[FigName]], filename = str_c(FigName, '.pdf'), path = mypath, width = 10, height = 6, units = "in")
#   }
# }
# 
# #### OVERLAP FACETS - updated block for DLS and DMS separate plotting ---------------------------
# mypath_dls <- file.path(getwd(), "guppy_plots", glue("{Experiment} {Sys.Date()}"), "horizontal time", "event_traces_over_days_overlap_DLS")
# if (!dir.exists(mypath_dls)) {
#   dir.create(mypath_dls, recursive = TRUE)
# }
# 
# mypath_dms <- file.path(getwd(), "guppy_plots", glue("{Experiment} {Sys.Date()}"), "horizontal time", "event_traces_over_days_overlap_DMS")
# if (!dir.exists(mypath_dms)) {
#   dir.create(mypath_dms, recursive = TRUE)
# }
# 
# # Define the session days and event names (unchanged)
# Session <- 1:8
# Event_plotnames <- c("Avoid Shock", "Escape Shock", "Shock", "Cue On", "Cue Off", "Cross (generic)")
# Events <- c("avoid", "escape", "shock", "cue_on", "cue_off", "cross")#removed cue_on_avoid and cue_on_escape cuz buggy and don't have time to fix
# 
# # For DLS: we want to compare achDLS and daDLS together
# TracePlotsSelect_dls <- list()
# DLS_structures <- c('achDLS', 'daDLS')
# 
# for (i2 in 1:length(Events)){
#   
#   # Filter Zmeans_unpack for DLS structures only
#   Zmeans_filt <- Zmeans_unpack %>%
#     filter(Structure %in% DLS_structures, plotParadigm %in% Session, Event == Events[i2]) %>%
#     plot.name.mutate_Active.Avoidance(.) %>%
#     group_by(Group, plotParadigm, Time)
#   
#   # Compute counts for annotations
#   Zmeans_filt_n <- Zmeans_filt %>%
#     ungroup() %>%
#     select(Subject, plotParadigm, Event, Group, Sex) %>%  # Exclude Structure
#     distinct() %>%  # Unique subjects per day/group/sex
#     group_by(plotParadigm, Event, Group, Sex) %>%
#     count() %>%
#     mutate(n = as.character(n))
#   
#   # Build a figure name indicating DLS group and the event
#   FigName <- str_c("DLS - ", Events[i2], " overlap")
#   
#   # Create the plot with separate colors for achDLS and daDLS
#   TracePlotsSelect_dls[[FigName]] <- ggplot(data = Zmeans_filt, 
#                                             mapping = aes(x = Time, y = zScore, color = Structure)) +
#     scale_color_manual(values = c("achDLS" = "blue", "daDLS" = "red")) +
#     # Change ribbon mapping: fill now based on Structure
#     geom_ribbon(stat = "summary", fun.data = sem2, mapping = aes(fill = Structure), alpha = 0.4, linetype = 0) +
#     # Add manual scale for fill to match the line colors
#     scale_fill_manual(values = c("achDLS" = "blue", "daDLS" = "red")) +
#     geom_path(stat = "summary", fun.data = sem2, linewidth = 0.1) +
#     geom_text_repel(data = Zmeans_filt_n, 
#                     mapping = aes(x = Inf, y = Inf, label = n, segment.color = 'transparent'), 
#                     size = 3, inherit.aes = FALSE) +
#     geom_vline(xintercept = 0, alpha = 0.5, lwd = 0.1) +
#     coord_cartesian(xlim=c(-2, 3), ylim = c(-2, 2)) +
#     scale_x_continuous(breaks = c(-2, 0, 3)) +
#     labs(title = str_c("DLS: ", Event_plotnames[i2]),
#          x = "Time (s)", y = "z-score", 
#          caption = "Line plots are mean with SEM error ribbons.") +
#     My_Theme1 +
#     facet_grid(rows = vars(Sex), cols = vars(plotParadigm), margins = TRUE)
#   
#   # Save the DLS plot to its dedicated folder
#   ggsave(plot = TracePlotsSelect_dls[[FigName]], filename = str_c(FigName, '.pdf'), path = mypath_dls, width = 10, height = 6, units = "in")
# }
# 
# # For DMS: compare achDMS and daDMS together -
# TracePlotsSelect_dms <- list()
# DMS_structures <- c('achDMS', 'daDMS')
# 
# for (i2 in 1:length(Events)){
#   
#   # Filter data for DMS signals
#   Zmeans_filt <- Zmeans_unpack %>%
#     filter(Structure %in% DMS_structures, plotParadigm %in% Session, Event == Events[i2]) %>%
#     plot.name.mutate_Active.Avoidance(.) %>%
#     group_by(Group, plotParadigm, Time)
#   
#   # Compute counts for annotations
#   Zmeans_filt_n <- Zmeans_filt %>%
#     ungroup() %>%
#     select(Subject, plotParadigm, Event, Group, Sex) %>%  # Exclude Structure
#     distinct() %>%  # Unique subjects per day/group/sex
#     group_by(plotParadigm, Event, Group, Sex) %>%
#     count() %>%
#     mutate(n = as.character(n))
#   
#   # Build a figure name indicating DMS group and the event
#   FigName <- str_c("DMS - ", Events[i2], " overlap")
#   
#   # Create the plot with separate colors for achDMS and daDMS
#   TracePlotsSelect_dms[[FigName]] <- ggplot(data = Zmeans_filt,
#                                             mapping = aes(x = Time, y = zScore, color = Structure)) +
#     scale_color_manual(values = c("achDMS" = "blue", "daDMS" = "red")) +  # Assign colors
#     geom_ribbon(stat = "summary", fun.data = sem2, mapping = aes(fill = Structure), alpha = 0.4, linetype = 0) +
#     # Add manual scale for fill to match the line colors
#     scale_fill_manual(values = c("achDMS" = "blue", "daDMS" = "red")) +
#     geom_path(stat = "summary", fun.data = sem2, linewidth = 0.1) +
#     geom_text_repel(data = Zmeans_filt_n, 
#                     mapping = aes(x = Inf, y = Inf, label = n, segment.color = 'transparent'), 
#                     size = 3, inherit.aes = FALSE) +
#     geom_vline(xintercept = 0, alpha = 0.5, lwd = 0.1) +
#     coord_cartesian(xlim=c(-2, 3), ylim = c(-2, 2)) +
#     scale_x_continuous(breaks = c(-2, 0, 3)) +
#     labs(title = str_c("DMS: ", Event_plotnames[i2]),
#          x = "Time (s)", y = "z-score",
#          caption = "Line plots are mean with SEM error ribbons.") +
#     My_Theme1 +
#     facet_grid(rows = vars(Sex), cols = vars(plotParadigm), margins = TRUE)
#   
#   # Save the DMS plot to its dedicated folder
#   ggsave(plot = TracePlotsSelect_dms[[FigName]], filename = str_c(FigName, '.pdf'), path = mypath_dms, width = 10, height = 6, units = "in")
# }
# 
# 
# 

#### PLOTTING BY SUBJECT (DLS DMS separate) -------------------------------------------------------
# Define the session days and event names (unchanged)
Session <- 1:8
Event_plotnames <- c("Avoid Shock", "Escape Shock", "Shock", "Cue On", "Cue Off", "Cross (generic)")
Events <- c("avoid", "escape", "shock", "cue_on", "cue_off", "cross")#removed cue_on_avoid and cue_on_escape cuz buggy and don't have time to fix


mypath_subject_dls <- file.path(getwd(), "guppy_plots", glue("{Experiment} {Sys.Date()}"), "horizontal time", "event_traces_over_days_by-subject_DLS")
if (!dir.exists(mypath_subject_dls)) {
  dir.create(mypath_subject_dls, recursive = TRUE)
}

TracePlotsSelect_dls <- list()
DLS_structures <- c('achDLS', 'daDLS')

Subjects <- Zmeans_unpack %>%
  select(Subject, Structure) %>%
  distinct() %>%
  filter(Structure %in% DLS_structures) %>%
  pull(Subject) %>% unique()

for (i3 in 1:length(Subjects)){
  for (i2 in 1:length(Events)){
    # Filter Zmeans_unpack for DLS structures only
    Zmeans_filt <- Zmeans_unpack %>%
      filter(Structure %in% DLS_structures, plotParadigm %in% Session, Event == Events[i2], Subject == Subjects[i3]) %>%
      plot.name.mutate_Active.Avoidance(.) %>%
      group_by(Group, plotParadigm, Time)
    
    # Before plotting, create a summary dataset with day-averaged data
    Zmeans_filt_with_avg <- Zmeans_filt %>%
      # First, create the original data
      bind_rows(
        # Add averaged data for "All Days"
        Zmeans_filt %>%
          group_by(Subject, Structure, Time, Timekey, Group, Sex) %>%
          summarise(
            zScore = mean(zScore, na.rm = TRUE),
            zSD = sqrt(mean(zSD^2, na.rm = TRUE)),  # RMS of standard deviations
            .groups = "drop"
          ) %>%
          mutate(plotParadigm = "All Days")
      ) %>%
      # Ensure proper factor ordering with "All Days" at the end
      mutate(plotParadigm = factor(plotParadigm, 
                                   levels = c(paste("Day", 1:8), "All Days")))
    
    # Compute counts for annotations
    Zmeans_filt_n <- 1
    
    # Build a figure name indicating DLS group and the event
    FigName <- str_c(Subjects[i3]," DLS - ", Events[i2], " DA+ACh")
    
    # Create the plot with separate colors for achDLS and daDLS
    TracePlotsSelect_dls[[FigName]] <- ggplot(data = Zmeans_filt_with_avg, 
                                              mapping = aes(x = Time, y = zScore, color = Structure)) +
      scale_color_manual(values = c("achDLS" = "blue", "daDLS" = "red")) +
      geom_ribbon(mapping = aes(ymin = zScore - zSD, ymax = zScore + zSD, fill = Structure), 
                  alpha = 0.4, linetype = 0) +
      scale_fill_manual(values = c("achDLS" = "blue", "daDLS" = "red")) +
      geom_line(linewidth = 0.5) +
      geom_vline(xintercept = 0, alpha = 0.5, lwd = 0.1) +
      coord_cartesian(xlim=c(-2, 3), ylim = c(-2, 2)) +
      scale_x_continuous(breaks = c(-2, 0, 3)) +
      labs(title = str_c("DLS: ", Event_plotnames[i2], " Subject: ", Subjects[i3]),
           x = "Time (s)", y = "z-score", 
           caption = "Line plots show individual traces with SD error ribbons. 'All Days' shows average across days.") +
      My_Theme1 +
      facet_grid(rows = vars(Sex), cols = vars(plotParadigm))  # Remove margins argument
    
    # Save the DLS plot to its dedicated folder
    ggsave(plot = TracePlotsSelect_dls[[FigName]], filename = str_c(FigName, '.pdf'), path = mypath_subject_dls, width = 12, height = 4, units = "in")
  }
}



mypath_subject_dms <- file.path(getwd(), "guppy_plots", glue("{Experiment} {Sys.Date()}"), "horizontal time", "event_traces_over_days_by-subject_DMS")
if (!dir.exists(mypath_subject_dms)) {
  dir.create(mypath_subject_dms, recursive = TRUE)
}

TracePlotsSelect_dms <- list()
DMS_structures <- c('achDMS', 'daDMS')

Subjects <- Zmeans_unpack %>%
  select(Subject, Structure) %>%
  distinct() %>%
  filter(Structure %in% DMS_structures) %>%
  pull(Subject) %>% unique()

for (i3 in 1:length(Subjects)){
  for (i2 in 1:length(Events)){
    # Filter Zmeans_unpack for DMS structures only
    Zmeans_filt <- Zmeans_unpack %>%
      filter(Structure %in% DMS_structures, plotParadigm %in% Session, Event == Events[i2], Subject == Subjects[i3]) %>%
      plot.name.mutate_Active.Avoidance(.) %>%
      group_by(Group, plotParadigm, Time)
    
    # Before plotting, create a summary dataset with day-averaged data
    Zmeans_filt_with_avg <- Zmeans_filt %>%
      # First, create the original data
      bind_rows(
        # Add averaged data for "All Days"
        Zmeans_filt %>%
          group_by(Subject, Structure, Time, Timekey, Group, Sex) %>%
          summarise(
            zScore = mean(zScore, na.rm = TRUE),
            zSD = sqrt(mean(zSD^2, na.rm = TRUE)),  # RMS of standard deviations
            .groups = "drop"
          ) %>%
          mutate(plotParadigm = "All Days")
      ) %>%
      # Ensure proper factor ordering with "All Days" at the end
      mutate(plotParadigm = factor(plotParadigm, 
                                   levels = c(paste("Day", 1:8), "All Days")))
    
    # Compute counts for annotations
    Zmeans_filt_n <- 1
    
    # Build a figure name indicating DMS group and the event
    FigName <- str_c(Subjects[i3]," DMS - ", Events[i2], " DA+ACh")
    
    # Create the plot with separate colors
    TracePlotsSelect_dms[[FigName]] <- ggplot(data = Zmeans_filt_with_avg, 
                                              mapping = aes(x = Time, y = zScore, color = Structure)) +
      scale_color_manual(values = c("achDMS" = "blue", "daDMS" = "red")) +
      geom_ribbon(mapping = aes(ymin = zScore - zSD, ymax = zScore + zSD, fill = Structure), 
                  alpha = 0.4, linetype = 0) +
      scale_fill_manual(values = c("achDMS" = "blue", "daDMS" = "red")) +
      geom_line(linewidth = 0.5) +
      geom_vline(xintercept = 0, alpha = 0.5, lwd = 0.1) +
      coord_cartesian(xlim=c(-2, 3), ylim = c(-2, 2)) +
      scale_x_continuous(breaks = c(-2, 0, 3)) +
      labs(title = str_c("DMS: ", Event_plotnames[i2], " Subject: ", Subjects[i3]),
           x = "Time (s)", y = "z-score", 
           caption = "Line plots show individual traces with SD error ribbons. 'All Days' shows average across days.") +
      My_Theme1 +
      facet_grid(rows = vars(Sex), cols = vars(plotParadigm))  # Remove margins argument
    
    # Save the DMS plot to its dedicated folder
    ggsave(plot = TracePlotsSelect_dms[[FigName]], filename = str_c(FigName, '.pdf'), path = mypath_subject_dms, width = 12, height = 4, units = "in")
  }
}


#### PLOTTING BY PERFORMANCE (DLS DMS separate) -----------------------------------------------------------
mypath_performance_DLS <- file.path(getwd(), "guppy_plots", glue("{Experiment} {Sys.Date()}"), "horizontal time", "event_traces_over_days_by-performance_DLS")
if (!dir.exists(mypath_performance_DLS)) {
  dir.create(mypath_performance_DLS, recursive = TRUE)
}

mypath_performance_DMS <- file.path(getwd(), "guppy_plots", glue("{Experiment} {Sys.Date()}"), "horizontal time", "event_traces_over_days_by-performance_DMS")
if (!dir.exists(mypath_performance_DMS)) {
  dir.create(mypath_performance_DMS, recursive = TRUE)
}

# Define the session days and event names (drop day 8)
Session <- 1:7
Event_plotnames <- c("Avoid Shock", "Escape Shock", "Shock", "Cue On", "Cue Off", "Cross (generic)", "Cue -> Avoid", "Cue -> Escape") 
Events <- c("avoid", "escape", "shock", "cue_on", "cue_off", "cross", "cue_on_avoid", "cue_on_escape") 

# Define performance groups each day based on %avoided per day
performance_groups <- c("Lower", "Higher")
# Step 1: Calculate mouse median avoidance across Days 4-7
late_phase_avoid <- Operant_Data %>%
  filter(name %in% c("total.avoids", "total.trials.run")) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  filter(Day %in% 4:7) %>%
  group_by(Subject) %>%
  summarise(median_avoid = median(100 * total.avoids / total.trials.run, na.rm=TRUE), .groups = "drop") 

# Step 2: Calculate median cutoff for group assignment
threshold <- median(late_phase_avoid$median_avoid, na.rm=TRUE)

# Step 3: Assign “Low” or “High” based on this threshold
subject_group <- late_phase_avoid %>%
  mutate(performance_group = if_else(median_avoid < threshold, "Low", "High"))

# Step 4: Join back to all records for plotting/statistics
perc_avoid <- Operant_Data %>%
  filter(name %in% c("total.avoids", "total.trials.run")) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  left_join(subject_group %>% select(Subject, performance_group), by = "Subject")

# Now, each row of perc_avoid has the stable group for that mouse, used for all subsequent analysis

# For DLS: we want to compare achDLS and daDLS together
TracePlotsSelect_performance_dls <- list()
DLS_structures <- c('achDLS', 'daDLS')

for (i2 in 1:length(Events)){
  Zmeans_filt <- Zmeans_unpack %>%
    filter(Structure %in% DLS_structures, 
           plotParadigm %in% Session, 
           Event == Events[i2]) %>%
    plot.name.mutate_Active.Avoidance(.) %>%
    mutate(Day_numeric = as.numeric(plotParadigm)) %>%
    left_join(perc_avoid %>% select(Subject, Day, performance_group), 
              by = c("Subject", "Day_numeric" = "Day")) %>%
    filter(!is.na(performance_group)) %>%
    mutate(day_period = case_when(
      Day_numeric %in% 1:3 ~ "Early (Days 1-3)",
      Day_numeric %in% 4:7 ~ "Late (Days 4-7)"
    )) %>%
    # STEP 1: First average within subjects (proper hierarchical approach)
    group_by(Subject, performance_group, day_period, Structure, Time, Timekey) %>%
    summarise(
      subject_mean = mean(zScore, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # STEP 2: Then calculate group statistics with proper SEM across subjects
    group_by(performance_group, day_period, Structure, Time, Timekey) %>%
    summarise(
      zScore = mean(subject_mean, na.rm = TRUE),
      zSEM = sd(subject_mean, na.rm = TRUE) / sqrt(n()),  # Proper SEM across subjects
      n_subjects = n(),
      .groups = "drop"
    ) %>%
    # Add "All Days" average using same hierarchical approach
    bind_rows(
      Zmeans_unpack %>%
        filter(Structure %in% DLS_structures, 
               plotParadigm %in% Session, 
               Event == Events[i2]) %>%
        plot.name.mutate_Active.Avoidance(.) %>%
        mutate(Day_numeric = as.numeric(plotParadigm)) %>%
        left_join(perc_avoid %>% select(Subject, Day, performance_group), 
                  by = c("Subject", "Day_numeric" = "Day")) %>%
        filter(!is.na(performance_group)) %>%
        # STEP 1: First average within subjects across all days
        group_by(Subject, performance_group, Structure, Time, Timekey) %>%
        summarise(
          subject_mean = mean(zScore, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        # STEP 2: Then across subjects with proper SEM
        group_by(performance_group, Structure, Time, Timekey) %>%
        summarise(
          zScore = mean(subject_mean, na.rm = TRUE),
          zSEM = sd(subject_mean, na.rm = TRUE) / sqrt(n()),  # Proper SEM across subjects
          n_subjects = n(),
          .groups = "drop"
        ) %>%
        mutate(day_period = "All Days")
    ) %>%
    # Ensure proper factor ordering for plotting
    mutate(
      performance_group = factor(performance_group, levels = c("Low", "High")),
      day_period = factor(day_period, levels = c("Early (Days 1-3)", "Late (Days 4-7)", "All Days"))
    )
  
  FigName <- str_c("DLS Performance - ", Events[i2], " DA+ACh")
  
  TracePlotsSelect_performance_dls[[FigName]] <- ggplot(data = Zmeans_filt, 
                                                        mapping = aes(x = Time, y = zScore, color = Structure)) +
    
    scale_color_manual(
      name = "Neurotransmitter",
      breaks = c("achDLS", "daDLS"),
      values = c("achDLS" = "blue", "daDLS" = "red"),
      labels = c("achDLS" = "ACh", "daDLS" = "DA")
    ) +
    scale_fill_manual(
      name = "Neurotransmitter",
      breaks = c("achDLS", "daDLS"),
      values = c("achDLS" = "blue", "daDLS" = "red"),
      labels = c("achDLS" = "ACh", "daDLS" = "DA")
    ) +
    
    geom_ribbon(mapping = aes(ymin = zScore - zSEM, ymax = zScore + zSEM, fill = Structure), 
                alpha = 0.4, linetype = 0) +
    geom_line(linewidth = 0.5) +
    geom_vline(xintercept = 0, alpha = 0.5, lwd = 0.1) +
    coord_cartesian(xlim=c(-2, 3), ylim = c(-2, 2)) +
    scale_x_continuous(breaks = c(-2, 0, 3)) +
    labs(title = str_c("DLS: ", Event_plotnames[i2], " by Performance"),
         x = "Time (s)", y = "z-score", 
         caption = "Rows: Performance level (Low/High %avoid). Columns: Training periods. Error bars: SEM across subjects.") +
    My_Theme1 + theme(
      legend.box.spacing = unit(0.1, "cm"),
      legend.position = "right",
      legend.key.size = unit(0.7, "cm"),
      legend.key.width = unit(0.7, "cm"),
      legend.spacing = unit(0.8, "cm"),
      legend.key.spacing.y = unit(0.15, "cm"),
      legend.text = element_text(margin = margin(r = 7, unit = "pt")),
      legend.title = element_text(margin = margin(b = 7, unit = "pt"))
    ) +
    facet_grid(rows = vars(performance_group), cols = vars(day_period))
  
  # Save the plot
  ggsave(plot = TracePlotsSelect_performance_dls[[FigName]], 
         filename = str_c(FigName, '.pdf'), 
         path = mypath_performance_DLS, 
         width = 12, height = 8, units = "in")
}


TracePlotsSelect_performance_dms <- list()
DMS_structures <- c('achDMS', 'daDMS')
for (i2 in 1:length(Events)){
  Zmeans_filt <- Zmeans_unpack %>%
    filter(Structure %in% DMS_structures, 
           plotParadigm %in% Session, 
           Event == Events[i2]) %>%
    plot.name.mutate_Active.Avoidance(.) %>%
    mutate(Day_numeric = as.numeric(plotParadigm)) %>%
    left_join(perc_avoid %>% select(Subject, Day, performance_group), 
              by = c("Subject", "Day_numeric" = "Day")) %>%
    filter(!is.na(performance_group)) %>%
    mutate(day_period = case_when(
      Day_numeric %in% 1:3 ~ "Early (Days 1-3)",
      Day_numeric %in% 4:7 ~ "Late (Days 4-7)"
    )) %>%
    # STEP 1: First average within subjects (proper hierarchical approach)
    group_by(Subject, performance_group, day_period, Structure, Time, Timekey) %>%
    summarise(
      subject_mean = mean(zScore, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # STEP 2: Then calculate group statistics with proper SEM across subjects
    group_by(performance_group, day_period, Structure, Time, Timekey) %>%
    summarise(
      zScore = mean(subject_mean, na.rm = TRUE),
      zSEM = sd(subject_mean, na.rm = TRUE) / sqrt(n()),  # Proper SEM across subjects
      n_subjects = n(),
      .groups = "drop"
    ) %>%
    # Add "All Days" average using same hierarchical approach
    bind_rows(
      Zmeans_unpack %>%
        filter(Structure %in% DMS_structures, 
               plotParadigm %in% Session, 
               Event == Events[i2]) %>%
        plot.name.mutate_Active.Avoidance(.) %>%
        mutate(Day_numeric = as.numeric(plotParadigm)) %>%
        left_join(perc_avoid %>% select(Subject, Day, performance_group), 
                  by = c("Subject", "Day_numeric" = "Day")) %>%
        filter(!is.na(performance_group)) %>%
        # STEP 1: First average within subjects across all days
        group_by(Subject, performance_group, Structure, Time, Timekey) %>%
        summarise(
          subject_mean = mean(zScore, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        # STEP 2: Then across subjects with proper SEM
        group_by(performance_group, Structure, Time, Timekey) %>%
        summarise(
          zScore = mean(subject_mean, na.rm = TRUE),
          zSEM = sd(subject_mean, na.rm = TRUE) / sqrt(n()),  # Proper SEM across subjects
          n_subjects = n(),
          .groups = "drop"
        ) %>%
        mutate(day_period = "All Days")
    ) %>%
    # Ensure proper factor ordering for plotting
    mutate(
      performance_group = factor(performance_group, levels = c("Low", "High")),
      day_period = factor(day_period, levels = c("Early (Days 1-3)", "Late (Days 4-7)", "All Days"))
    )
  
  FigName <- str_c("DMS Performance - ", Events[i2], " DA+ACh")
  
  TracePlotsSelect_performance_dms[[FigName]] <- ggplot(data = Zmeans_filt, 
                                                        mapping = aes(x = Time, y = zScore, color = Structure)) +
    scale_color_manual(
      breaks = c("achDMS", "daDMS"),
      values = c("achDMS" = "blue", "daDMS" = "red"),
      labels = c("achDMS" = "ACh", "daDMS" = "DA"),
      name = "Neurotransmitter"
    ) +
    scale_fill_manual(
      breaks = c("achDMS", "daDMS"),
      values = c("achDMS" = "blue", "daDMS" = "red"),
      labels = c("achDMS" = "ACh", "daDMS" = "DA"), 
      name = "Neurotransmitter"
    ) +
    geom_ribbon(mapping = aes(ymin = zScore - zSEM, ymax = zScore + zSEM, fill = Structure), 
                alpha = 0.4, linetype = 0) +
    geom_line(linewidth = 0.5) +
    geom_vline(xintercept = 0, alpha = 0.5, lwd = 0.1) +
    coord_cartesian(xlim=c(-2, 3), ylim = c(-2, 2)) +
    scale_x_continuous(breaks = c(-2, 0, 3)) +
    labs(title = str_c("DMS: ", Event_plotnames[i2], " by Performance"),
         x = "Time (s)", y = "z-score", 
         caption = "Rows: Performance level (Low/High %avoid). Columns: Training periods. Error bars: SEM across subjects.") +
    My_Theme1 +
    facet_grid(rows = vars(performance_group), cols = vars(day_period)) + theme(
      legend.box.spacing = unit(0.1, "cm"),
      legend.position = "right",
      legend.key.size = unit(0.7, "cm"),
      legend.key.width = unit(0.7, "cm"),
      legend.spacing = unit(0.8, "cm"),
      legend.key.spacing.y = unit(0.15, "cm"),
      legend.text = element_text(margin = margin(r = 7, unit = "pt")),
      legend.title = element_text(margin = margin(b = 7, unit = "pt"))
    )
  
  # Save the plot
  ggsave(plot = TracePlotsSelect_performance_dms[[FigName]], 
         filename = str_c(FigName, '.pdf'), 
         path = mypath_performance_DMS, 
         width = 12, height = 8, units = "in")
}



#### PLOTTING BY PERFORMANCE (ACh DA separate) -----------------------------------------------------------
mypath_regional <- file.path(getwd(), "guppy_plots", glue("{Experiment} {Sys.Date()}"), "horizontal time", "regional_comparison_DLS_vs_DMS_by_performance")
if (!dir.exists(mypath_regional)) {
  dir.create(mypath_regional, recursive = TRUE)
}

TracePlotsSelect_regional <- list()

# Compare DA systems: daDLS vs daDMS with performance levels
for (i2 in 1:length(Events)){
  cat("Processing DA regional comparison with performance for event:", Events[i2], "\n")
  
  Zmeans_filt_DA <- Zmeans_unpack %>%
    filter(Structure %in% c('daDLS', 'daDMS'), 
           plotParadigm %in% Session, 
           Event == Events[i2]) %>%
    plot.name.mutate_Active.Avoidance(.) %>%
    # Convert ordered factor to numeric for joining
    mutate(Day_numeric = as.numeric(plotParadigm)) %>%
    # Join with performance data
    left_join(perc_avoid %>% select(Subject, Day, performance_group), 
              by = c("Subject", "Day_numeric" = "Day")) %>%
    filter(!is.na(performance_group)) %>%
    # Create region labels
    mutate(Region = ifelse(Structure == "daDLS", "DLS", "DMS")) %>%
    # Create day period groupings
    mutate(day_period = case_when(
      Day_numeric %in% 1:2 ~ "Early (Days 1-2)",
      Day_numeric %in% 3:5 ~ "Mid (Days 3-5)", 
      Day_numeric %in% 6:7 ~ "Late (Days 6-7)"
    )) %>%
    filter(!is.na(day_period)) %>%
    # Group for averaging by performance, day period, and region
    group_by(performance_group, day_period, Region, Time, Timekey) %>%
    summarise(
      zScore = mean(zScore, na.rm = TRUE),
      zSD = sqrt(mean(zSD^2, na.rm = TRUE))/sqrt(n()),
      n_subjects = n_distinct(Subject),
      .groups = "drop"
    ) %>%
    # Add "All Days" average for each performance group
    bind_rows(
      Zmeans_unpack %>%
        filter(Structure %in% c('daDLS', 'daDMS'), 
               plotParadigm %in% Session, 
               Event == Events[i2]) %>%
        plot.name.mutate_Active.Avoidance(.) %>%
        mutate(Day_numeric = as.numeric(plotParadigm)) %>%
        left_join(perc_avoid %>% select(Subject, Day, performance_group), 
                  by = c("Subject", "Day_numeric" = "Day")) %>%
        filter(!is.na(performance_group)) %>%
        mutate(Region = ifelse(Structure == "daDLS", "DLS", "DMS")) %>%
        group_by(performance_group, Region, Time, Timekey) %>%
        summarise(
          zScore = mean(zScore, na.rm = TRUE),
          zSD = sqrt(mean(zSD^2, na.rm = TRUE))/sqrt(n()),
          n_subjects = n_distinct(Subject),
          .groups = "drop"
        ) %>%
        mutate(day_period = "All Days")
    ) %>%
    # Factor ordering
    mutate(
      performance_group = factor(performance_group, levels = c("Low", "Medium", "High")),
      day_period = factor(day_period, levels = c("Early (Days 1-2)", "Mid (Days 3-5)", "Late (Days 6-7)", "All Days")),
      Region = factor(Region, levels = c("DLS", "DMS"))
    )
  
  if(nrow(Zmeans_filt_DA) == 0) {
    cat("No DA data for event:", Events[i2], "- skipping\n")
    next
  }
  
  clean_event_name <- str_replace_all(Events[i2], "[^A-Za-z0-9_]", "_")
  FigName_DA <- str_c("Regional_DA_Performance_", clean_event_name)
  
  TracePlotsSelect_regional[[FigName_DA]] <- ggplot(data = Zmeans_filt_DA, 
                                                    mapping = aes(x = Time, y = zScore, color = Region)) +
    scale_color_manual(values = c("DLS" = "#E31A2C", "DMS" = "#FF7F00")) +  # Red for DLS, Orange for DMS
    geom_ribbon(mapping = aes(ymin = zScore - zSD, ymax = zScore + zSD, fill = Region), 
                alpha = 0.3, linetype = 0) +
    scale_fill_manual(values = c("DLS" = "#E31A2C", "DMS" = "#FF7F00")) +
    geom_line(linewidth = 0.8) +
    geom_vline(xintercept = 0, alpha = 0.5, lwd = 0.1) +
    geom_hline(yintercept = 0, alpha = 0.3, linetype = "dotted") +
    coord_cartesian(xlim=c(-2, 3), ylim = c(-2, 2)) +
    scale_x_continuous(breaks = c(-2, 0, 3)) +
    labs(title = str_c("Dopamine: ", Event_plotnames[i2], " - DLS vs DMS by Performance"),
         x = "Time (s)", y = "z-score", 
         caption = "Red = DLS, Orange = DMS. Rows = Performance level. Columns = Training periods.") +
    My_Theme1 +
    theme(legend.position = "top") +
    facet_grid(rows = vars(performance_group), cols = vars(day_period))
  
  # Save the plot
  ggsave(plot = TracePlotsSelect_regional[[FigName_DA]], 
         filename = str_c(FigName_DA, '.pdf'), 
         path = mypath_regional, 
         width = 14, height = 9, units = "in")  # Taller for 3 performance rows
}

# Compare ACh systems: achDLS vs achDMS with performance levels
for (i2 in 1:length(Events)){
  cat("Processing ACh regional comparison with performance for event:", Events[i2], "\n")
  
  Zmeans_filt_ACh <- Zmeans_unpack %>%
    filter(Structure %in% c('achDLS', 'achDMS'), 
           plotParadigm %in% Session, 
           Event == Events[i2]) %>%
    plot.name.mutate_Active.Avoidance(.) %>%
    # Convert ordered factor to numeric for joining
    mutate(Day_numeric = as.numeric(plotParadigm)) %>%
    # Join with performance data
    left_join(perc_avoid %>% select(Subject, Day, performance_group), 
              by = c("Subject", "Day_numeric" = "Day")) %>%
    filter(!is.na(performance_group)) %>%
    # Create region labels
    mutate(Region = ifelse(Structure == "achDLS", "DLS", "DMS")) %>%
    # Create day period groupings
    mutate(day_period = case_when(
      Day_numeric %in% 1:2 ~ "Early (Days 1-2)",
      Day_numeric %in% 3:5 ~ "Mid (Days 3-5)", 
      Day_numeric %in% 6:7 ~ "Late (Days 6-7)"
    )) %>%
    filter(!is.na(day_period)) %>%
    # Group for averaging by performance, day period, and region
    group_by(performance_group, day_period, Region, Time, Timekey) %>%
    summarise(
      zScore = mean(zScore, na.rm = TRUE),
      zSD = sqrt(mean(zSD^2, na.rm = TRUE))/sqrt(n()),
      n_subjects = n_distinct(Subject),
      .groups = "drop"
    ) %>%
    # Add "All Days" average for each performance group
    bind_rows(
      Zmeans_unpack %>%
        filter(Structure %in% c('achDLS', 'achDMS'), 
               plotParadigm %in% Session, 
               Event == Events[i2]) %>%
        plot.name.mutate_Active.Avoidance(.) %>%
        mutate(Day_numeric = as.numeric(plotParadigm)) %>%
        left_join(perc_avoid %>% select(Subject, Day, performance_group), 
                  by = c("Subject", "Day_numeric" = "Day")) %>%
        filter(!is.na(performance_group)) %>%
        mutate(Region = ifelse(Structure == "achDLS", "DLS", "DMS")) %>%
        group_by(performance_group, Region, Time, Timekey) %>%
        summarise(
          zScore = mean(zScore, na.rm = TRUE),
          zSD = sqrt(mean(zSD^2, na.rm = TRUE))/sqrt(n()),
          n_subjects = n_distinct(Subject),
          .groups = "drop"
        ) %>%
        mutate(day_period = "All Days")
    ) %>%
    # Factor ordering
    mutate(
      performance_group = factor(performance_group, levels = c("Low", "Medium", "High")),
      day_period = factor(day_period, levels = c("Early (Days 1-2)", "Mid (Days 3-5)", "Late (Days 6-7)", "All Days")),
      Region = factor(Region, levels = c("DLS", "DMS"))
    )
  
  if(nrow(Zmeans_filt_ACh) == 0) {
    cat("No ACh data for event:", Events[i2], "- skipping\n")
    next
  }
  
  clean_event_name <- str_replace_all(Events[i2], "[^A-Za-z0-9_]", "_")
  FigName_ACh <- str_c("Regional_ACh_Performance_", clean_event_name)
  
  TracePlotsSelect_regional[[FigName_ACh]] <- ggplot(data = Zmeans_filt_ACh, 
                                                     mapping = aes(x = Time, y = zScore, color = Region)) +
    scale_color_manual(values = c("DLS" = "#1F78B4", "DMS" = "#33A02C")) +  # Blue for DLS, Green for DMS
    geom_ribbon(mapping = aes(ymin = zScore - zSD, ymax = zScore + zSD, fill = Region), 
                alpha = 0.3, linetype = 0) +
    scale_fill_manual(values = c("DLS" = "#1F78B4", "DMS" = "#33A02C")) +
    geom_line(linewidth = 0.8) +
    geom_vline(xintercept = 0, alpha = 0.5, lwd = 0.1) +
    geom_hline(yintercept = 0, alpha = 0.3, linetype = "dotted") +
    coord_cartesian(xlim=c(-2, 3), ylim = c(-2, 2)) +
    scale_x_continuous(breaks = c(-2, 0, 3)) +
    labs(title = str_c("Acetylcholine: ", Event_plotnames[i2], " - DLS vs DMS by Performance"),
         x = "Time (s)", y = "z-score", 
         caption = "Blue = DLS, Green = DMS. Rows = Performance level. Columns = Training periods.") +
    My_Theme1 +
    theme(legend.position = "top") +
    facet_grid(rows = vars(performance_group), cols = vars(day_period))
  
  # Save the plot
  ggsave(plot = TracePlotsSelect_regional[[FigName_ACh]], 
         filename = str_c(FigName_ACh, '.pdf'), 
         path = mypath_regional, 
         width = 14, height = 9, units = "in")  # Taller for 3 performance rows
}



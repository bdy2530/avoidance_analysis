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
  left_join(meta.data)

#synapse data:
# synapse_eval_trim <- readRDS(file = "Photometry_Eval.Active_Avoidance")
timeaxis_h5 <- readRDS(file = glue("{Experiment}_timeaxis_h5.Rds"))
Zmeans <- readRDS(file = glue("{Experiment}_Operant_Data.Zmeans.Rds"))

#subset time axis by index timeaxis_subsample_index - downsample by 20 for plotting
timeaxis_subsample_index <- seq(10,length(timeaxis_h5),20) %>% as.integer()
timeaxis <- timeaxis_h5[timeaxis_subsample_index]
#formatting with rownumbers/samples:
timeaxisTable <- tibble(Time = timeaxis) %>%
  mutate(TimeKey = row_number()) %>%
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
  rename(zSD = Zerr) %>% 
  rowwise() %>%
  mutate(plotParadigm = Paradigm.Day,
         # Subsample both zScore AND zSD
         zScore = unlist(zScore) %>% .[timeaxis_subsample_index] %>% list(),
         zSD = unlist(zSD) %>% .[timeaxis_subsample_index] %>% list(),
         zpack = list(tibble(zScore = unlist(zScore),
                             zSD = unlist(zSD),      # Include zSD in the tibble
                             Time = timeaxisTable$Time,
                             Timekey = timeaxisTable$TimeKey))) %>%
  dplyr::select(-zScore, -zSD) %>%  # Remove the original list columns
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
  filter(is.finite(Time) & is.finite(zScore) & is.finite(zSD))


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
Event_plotnames <- c("Avoid Shock", "Escape Shock", "Shock", "Cue On", "Cue Off", "Cross (generic)", "Cue+Avoid", "Cue+Escape")
Events <- c("avoid", "escape", "shock", "cue_on", "cue_off", "cross", "cue_on_avoid", "cue_on_escape")

# Define performance groups each day based on %avoided per day
performance_groups <- c("Low", "Medium", "High")
perc_avoid <-  Operant_Data %>% 
  filter(name %in% c("total.avoids", "total.trials.run")) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  filter(Subject != 41 & Subject != 42 & Subject != 43 & Subject != 44) %>% #remove weird subjects
  mutate(perc_avoid = 100*total.avoids/total.trials.run) %>% filter(Day != 8) %>%
  mutate(performance_group = case_when(
    perc_avoid < 33 ~ "Low",
    perc_avoid >= 33 & perc_avoid < 66 ~ "Medium",
    perc_avoid >= 66 ~ "High"
  ))


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
      Day_numeric %in% 1:2 ~ "Early (Days 1-2)",
      Day_numeric %in% 3:5 ~ "Mid (Days 3-5)",
      Day_numeric %in% 6:7 ~ "Late (Days 6-7)"
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
      performance_group = factor(performance_group, levels = c("Low", "Medium", "High")),
      day_period = factor(day_period, levels = c("Early (Days 1-2)", "Mid (Days 3-5)", "Late (Days 6-7)", "All Days"))
    )
  
  FigName <- str_c("DLS Performance - ", Events[i2], " DA+ACh")
  
  TracePlotsSelect_performance_dls[[FigName]] <- ggplot(data = Zmeans_filt, 
                                                        mapping = aes(x = Time, y = zScore, color = Structure)) +
    scale_color_manual(values = c("achDLS" = "blue", "daDLS" = "red")) +
    geom_ribbon(mapping = aes(ymin = zScore - zSEM, ymax = zScore + zSEM, fill = Structure), 
                alpha = 0.4, linetype = 0) +
    scale_fill_manual(values = c("achDLS" = "blue", "daDLS" = "red")) +
    geom_line(linewidth = 0.5) +
    geom_vline(xintercept = 0, alpha = 0.5, lwd = 0.1) +
    coord_cartesian(xlim=c(-2, 3), ylim = c(-2, 2)) +
    scale_x_continuous(breaks = c(-2, 0, 3)) +
    labs(title = str_c("DLS: ", Event_plotnames[i2], " by Performance"),
         x = "Time (s)", y = "z-score", 
         caption = "Rows = Performance level (Low/Med/High % avoid). Columns = Training periods. Error bars show SEM across subjects. No error bars shown for single-subject groups.") +
    My_Theme1 +
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
      Day_numeric %in% 1:2 ~ "Early (Days 1-2)",
      Day_numeric %in% 3:5 ~ "Mid (Days 3-5)",
      Day_numeric %in% 6:7 ~ "Late (Days 6-7)"
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
      performance_group = factor(performance_group, levels = c("Low", "Medium", "High")),
      day_period = factor(day_period, levels = c("Early (Days 1-2)", "Mid (Days 3-5)", "Late (Days 6-7)", "All Days"))
    )
  
  FigName <- str_c("DMS Performance - ", Events[i2], " DA+ACh")
  
  TracePlotsSelect_performance_dms[[FigName]] <- ggplot(data = Zmeans_filt, 
                                                        mapping = aes(x = Time, y = zScore, color = Structure)) +
    scale_color_manual(values = c("achDMS" = "blue", "daDMS" = "red")) +
    geom_ribbon(mapping = aes(ymin = zScore - zSEM, ymax = zScore + zSEM, fill = Structure), 
                alpha = 0.4, linetype = 0) +
    scale_fill_manual(values = c("achDMS" = "blue", "daDMS" = "red")) +
    geom_line(linewidth = 0.5) +
    geom_vline(xintercept = 0, alpha = 0.5, lwd = 0.1) +
    coord_cartesian(xlim=c(-2, 3), ylim = c(-2, 2)) +
    scale_x_continuous(breaks = c(-2, 0, 3)) +
    labs(title = str_c("DMS: ", Event_plotnames[i2], " by Performance"),
         x = "Time (s)", y = "z-score", 
         caption = "Rows = Performance level (Low/Med/High % avoid). Columns = Training periods. Error bars show SEM across subjects. No error bars shown for single-subject groups.") +
    My_Theme1 +
    facet_grid(rows = vars(performance_group), cols = vars(day_period))
  
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

#### CRITICAL TIME WINDOWS ANALYSIS -----------------------------------------------------------
mypath_timewindows <- file.path(getwd(), "guppy_plots", glue("{Experiment} {Sys.Date()}"), "horizontal time", "critical_time_windows")
if (!dir.exists(mypath_timewindows)) {
  dir.create(mypath_timewindows, recursive = TRUE)
}

# Define critical time windows - all with equal weight
time_windows <- list(
  "Anticipatory" = c(-2, 0),
  "Immediate" = c(0, 1),
  "Late" = c(1, 3),
  "All" = c(-2, 3)
)

# CREATE LABELS WITH TIME PERIODS - ADD THIS
time_window_labels <- c(
  "Anticipatory" = "Anticipatory\n(-2 to 0s)",
  "Immediate" = "Immediate\n(0 to 1s)",
  "Late" = "Late\n(1 to 3s)",
  "All" = "All\n(-2 to 3s)"
)

# Create time window analysis for each event
TimeWindow_Results <- list()

for (i2 in 1:length(Events)) {
  # Calculate average activity in each time window
  window_data <- Zmeans_unpack %>%
    filter(plotParadigm %in% Session, 
           Event == Events[i2]) %>%
    plot.name.mutate_Active.Avoidance(.) %>%
    mutate(Day_numeric = as.numeric(plotParadigm)) %>%
    # Join with performance data
    left_join(perc_avoid %>% select(Subject, Day, performance_group, perc_avoid), 
              by = c("Subject", "Day_numeric" = "Day")) %>%
    filter(!is.na(performance_group)) %>%
    # Calculate activity in each time window
    crossing(window_name = names(time_windows)) %>%
    mutate(
      window_start = map_dbl(window_name, ~time_windows[[.x]][1]),
      window_end = map_dbl(window_name, ~time_windows[[.x]][2])
    ) %>%
    filter(Time >= window_start, Time <= window_end) %>%
    # Average within each time window for each subject/structure
    group_by(Subject, Structure, performance_group, perc_avoid, window_name, window_start, window_end) %>%
    summarise(
      window_avg = mean(zScore, na.rm = TRUE),
      window_peak = max(zScore, na.rm = TRUE),
      window_min = min(zScore, na.rm = TRUE),
      n_timepoints = n(),
      .groups = "drop"
    ) %>%
    # Add region and neurotransmitter labels
    mutate(
      Region = case_when(
        str_detect(Structure, "DLS") ~ "DLS",
        str_detect(Structure, "DMS") ~ "DMS"
      ),
      Neurotransmitter = case_when(
        str_detect(Structure, "da") ~ "DA",
        str_detect(Structure, "ach") ~ "ACh"
      )
    ) %>%
    filter(!is.na(Region), !is.na(Neurotransmitter)) %>%
    # Factor ordering
    mutate(
      window_name = factor(window_name, levels = names(time_windows)),
      performance_group = factor(performance_group, levels = c("Low", "Medium", "High"))
    )
  
  TimeWindow_Results[[Events[i2]]] <- window_data
}

## HEATMAP - which time windows show strongest responses
TracePlotsSelect_timewindows <- list()

for (i2 in 1:length(Events)) {
  if (is.null(TimeWindow_Results[[Events[i2]]])) next
  
  # Create heatmap data
  heatmap_data <- TimeWindow_Results[[Events[i2]]] %>%
    group_by(Structure, window_name, performance_group) %>%
    summarise(
      mean_activity = mean(window_avg, na.rm = TRUE),
      se_activity = sd(window_avg, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  clean_event_name <- str_replace_all(Events[i2], "[^A-Za-z0-9_]", "_")
  
  # Heatmap plot - CHANGE THE X AESTHETIC AND ADD SCALE
  p_heatmap <- ggplot(heatmap_data, aes(x = window_name, y = Structure, fill = mean_activity)) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(aes(label = round(mean_activity, 2)), color = "white", size = 2.5, fontface = "bold") +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                         name = "z-score") +
    scale_x_discrete(labels = time_window_labels) +  # ADD THIS LINE
    labs(title = paste("Time Window Heatmap:", Event_plotnames[i2]),
         x = "Time Window", y = "Neural Structure",
         subtitle = "Color intensity = average neural activity in each time window",
         caption = "Numbers show exact z-score values. Columns = Performance groups.") +
    My_Theme1 +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right") +
    facet_wrap(~ performance_group, ncol = 3)
  
  TracePlotsSelect_timewindows[[paste0("Heatmap_", clean_event_name)]] <- p_heatmap
  
  ggsave(p_heatmap, 
         filename = paste0("TimeWindow_Heatmap_", clean_event_name, ".pdf"),
         path = mypath_timewindows, width = 14, height = 8)
}

## CORRELATION ANALYSIS which time windows predict performance best
correlation_results <- list()

for (i2 in 1:length(Events)) {
  if (is.null(TimeWindow_Results[[Events[i2]]])) next
  
  # Calculate correlations between time window activity and performance
  cor_data <- TimeWindow_Results[[Events[i2]]] %>%
    group_by(Structure, window_name) %>%
    summarise(
      correlation = cor(window_avg, perc_avoid, use = "complete.obs"),
      p_value = cor.test(window_avg, perc_avoid)$p.value,
      n_subjects = n(),
      .groups = "drop"
    ) %>%
    mutate(
      significant = p_value < 0.05,
      Event = Events[i2]
    )
  
  correlation_results[[Events[i2]]] <- cor_data
  
  # Plot correlations
  clean_event_name <- str_replace_all(Events[i2], "[^A-Za-z0-9_]", "_")
  
  # ALSO ADD THE SCALE HERE
  p_cor <- ggplot(cor_data, aes(x = window_name, y = Structure, fill = correlation)) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(aes(label = ifelse(significant, 
                                 paste0(round(correlation, 2), "*"), 
                                 round(correlation, 2))), 
              color = "white", size = 3, fontface = "bold") +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                         limits = c(-1, 1), name = "Correlation\nwith Performance") +
    scale_x_discrete(labels = time_window_labels) +  # ADD THIS LINE
    labs(title = paste("Performance Prediction:", Event_plotnames[i2]),
         x = "Time Window", y = "Neural Structure",
         subtitle = "Correlation between neural activity and % avoidance performance",
         caption = "* = significant correlation (p < 0.05). Red = positive correlation, Blue = negative.") +
    My_Theme1 +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  TracePlotsSelect_timewindows[[paste0("Correlation_", clean_event_name)]] <- p_cor
  
  ggsave(p_cor, 
         filename = paste0("Performance_Correlation_", clean_event_name, ".pdf"),
         path = mypath_timewindows, width = 12, height = 6)
}

# Combine all correlation results for summary
all_correlations <- bind_rows(correlation_results) %>%
  arrange(desc(abs(correlation)))

# Print summary of strongest correlations
print(all_correlations %>% 
        filter(significant) %>%
        select(Event, Structure, window_name, correlation, p_value))


#### NEW - FUZZY PERFORMANCE GROUPING --------------------
# ADJUSTABLE PARAMETERS
MIN_FUZZY_WEIGHT_THRESHOLD <- 0.2   # Minimum weight for group inclusion
FUZZY_BANDWIDTH <- 0.3             # Controls fuzziness (higher = more fuzzy)
PERFORMANCE_WEIGHT <- 0.5         # Weight for overall performance (0-1)
LEARNING_WEIGHT <- 0.5             # Weight for learning capacity (0-1)
# Note: PERFORMANCE_WEIGHT + LEARNING_WEIGHT should equal 1.0

# create directory
create_plot_directory <- function(region, method_name = "fuzzy_hybrid") {
  mypath <- file.path(getwd(), "guppy_plots", glue("{Experiment} {Sys.Date()}"), 
                      "horizontal time", glue("event_traces_over_days_by-performance_{region}_{method_name}"))
  if (!dir.exists(mypath)) {
    dir.create(mypath, recursive = TRUE)
  }
  return(mypath)
}

# constants
Session <- 1:7
Event_plotnames <- c("Avoid Shock", "Escape Shock", "Shock", "Cue On", "Cue Off", 
                     "Cross (generic)", "Cue+Avoid", "Cue+Escape")
Events <- c("avoid", "escape", "shock", "cue_on", "cue_off", "cross", 
            "cue_on_avoid", "cue_on_escape")

# avoidance percentages
perc_avoid_base <- Operant_Data %>% 
  filter(name %in% c("total.avoids", "total.trials.run")) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  filter(!Subject %in% 41:44) %>% filter(Subject < 1000) %>%
  mutate(perc_avoid = 100 * total.avoids / total.trials.run) %>% 
  filter(Day != 8)

calculate_all_performance_metrics <- function(perc_avoid_data) {
  #  mean performance
  mean_performance <- perc_avoid_data %>%
    group_by(Subject) %>%
    summarise(mean_perc_avoid = mean(perc_avoid, na.rm = TRUE), .groups = "drop")
  
  # temporal learning 
  temporal_metrics <- perc_avoid_data %>%
    group_by(Subject) %>%
    summarise(
      early_performance = mean(perc_avoid[Day %in% c(1, 2)], na.rm = TRUE),
      mid_performance = mean(perc_avoid[Day %in% c(3, 4, 5)], na.rm = TRUE),
      late_performance = mean(perc_avoid[Day %in% c(6, 7)], na.rm = TRUE),
      early_to_mid = mid_performance - early_performance,
      mid_to_late = late_performance - mid_performance,
      early_to_late = late_performance - early_performance,
      learning_slope = {
        day_data <- data.frame(day = Day[!is.na(perc_avoid)], perf = perc_avoid[!is.na(perc_avoid)])
        if(nrow(day_data) >= 3) {
          lm_fit <- lm(perf ~ day, data = day_data)
          coef(lm_fit)[2]
        } else NA
      },
      .groups = "drop"
    )
  
  # combine all 
  all_metrics <- mean_performance %>%
    left_join(temporal_metrics, by = "Subject")
  
  return(all_metrics)
}

all_performance_metrics <- calculate_all_performance_metrics(perc_avoid_base)

create_fuzzy_hybrid_groups <- function(performance_metrics, 
                                       data_weight = PERFORMANCE_WEIGHT,    # Use adjustable parameter
                                       learning_weight = LEARNING_WEIGHT,   # Use adjustable parameter
                                       fuzzy_bandwidth = FUZZY_BANDWIDTH) { # Use adjustable parameter
  
  # Validate weights sum to 1
  if (abs(data_weight + learning_weight - 1.0) > 0.001) {
    warning("Performance and learning weights should sum to 1.0")
  }
  
  # hybrid scores 
  hybrid_data <- performance_metrics %>%
    mutate(
      data_z = scale(mean_perc_avoid)[,1],
      learning_z = scale(early_to_late)[,1],
      hybrid_score = data_weight * data_z + learning_weight * learning_z
    )
  
  # hybrid tertiles and group centers
  hybrid_tertiles <- quantile(hybrid_data$hybrid_score, probs = c(1/3, 2/3), na.rm = TRUE)
  low_center <- median(hybrid_data$hybrid_score[hybrid_data$hybrid_score <= hybrid_tertiles[1]], na.rm = TRUE)
  medium_center <- median(hybrid_data$hybrid_score[hybrid_data$hybrid_score > hybrid_tertiles[1] & 
                                                     hybrid_data$hybrid_score <= hybrid_tertiles[2]], na.rm = TRUE)
  high_center <- median(hybrid_data$hybrid_score[hybrid_data$hybrid_score > hybrid_tertiles[2]], na.rm = TRUE)
  
  cat("Fuzzy Parameters - Bandwidth:", fuzzy_bandwidth, 
      "| Weights: Performance", data_weight, "Learning", learning_weight, "\n")
  cat("Hybrid tertile cutoffs:", round(hybrid_tertiles, 2), "\n")
  cat("Group centers - Low:", round(low_center, 2), "Medium:", round(medium_center, 2), "High:", round(high_center, 2), "\n\n")
  
  # fuzzy groups
  fuzzy_hybrid_groups <- hybrid_data %>%
    mutate(
      # distances to each group center
      dist_to_low = abs(hybrid_score - low_center),
      dist_to_medium = abs(hybrid_score - medium_center), 
      dist_to_high = abs(hybrid_score - high_center),
      
      # fuzzy membership weights (closer = higher weight)
      raw_low_weight = exp(-(dist_to_low^2) / (2 * fuzzy_bandwidth^2)),
      raw_medium_weight = exp(-(dist_to_medium^2) / (2 * fuzzy_bandwidth^2)),
      raw_high_weight = exp(-(dist_to_high^2) / (2 * fuzzy_bandwidth^2)),
      
      # normalize weights
      total_weight = raw_low_weight + raw_medium_weight + raw_high_weight,
      low_weight = raw_low_weight / total_weight,
      medium_weight = raw_medium_weight / total_weight,
      high_weight = raw_high_weight / total_weight,
      
      # fuzzy classification (dominant group)
      performance_group = case_when(
        low_weight >= medium_weight & low_weight >= high_weight ~ "Low",
        medium_weight >= low_weight & medium_weight >= high_weight ~ "Medium",
        TRUE ~ "High"
      ),
      
      # confidence
      classification_confidence = pmax(low_weight, medium_weight, high_weight),
      fuzziness = 1 - classification_confidence,
      
      method_name = glue("Fuzzy_Hybrid_{round(data_weight*100)}perf_{round(learning_weight*100)}learn")
    ) %>%
    select(-dist_to_low, -dist_to_medium, -dist_to_high,
           -raw_low_weight, -raw_medium_weight, -raw_high_weight, -total_weight) %>%
    arrange(desc(hybrid_score))
  
  return(fuzzy_hybrid_groups)
}

# fuzzy hybrid groups using adjustable parameters
subject_overall_performance <- create_fuzzy_hybrid_groups(all_performance_metrics)

# summary
cat("=== FUZZY HYBRID GROUP RESULTS ===\n")
cat("Parameters: Performance Weight =", PERFORMANCE_WEIGHT, "| Learning Weight =", LEARNING_WEIGHT, 
    "| Bandwidth =", FUZZY_BANDWIDTH, "| Min Threshold =", MIN_FUZZY_WEIGHT_THRESHOLD, "\n\n")
print(subject_overall_performance %>%
        select(Subject, mean_perc_avoid, early_to_late, performance_group, 
               low_weight, medium_weight, high_weight, classification_confidence) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))))

perc_avoid_weighted <- perc_avoid_base %>%
  left_join(subject_overall_performance, by = "Subject")

apply_fuzzy_weighted_performance <- function(neural_data, weight_col, group_name, min_weight = MIN_FUZZY_WEIGHT_THRESHOLD) {
  neural_data %>%
    left_join(perc_avoid_weighted %>% select(Subject, Day, all_of(weight_col)), 
              by = c("Subject", "Day_numeric" = "Day")) %>%
    filter(!!sym(weight_col) >= min_weight) %>%  # Use adjustable threshold
    mutate(
      performance_group = group_name,
      weight = !!sym(weight_col)
    )
}

# process fuzzy weighted data for plotting
process_fuzzy_weighted_zmeans_data <- function(structures, event, min_weight = MIN_FUZZY_WEIGHT_THRESHOLD) {
  base_data <- Zmeans_unpack %>%
    filter(Structure %in% structures, 
           plotParadigm %in% Session, 
           Event == event) %>%
    plot.name.mutate_Active.Avoidance(.) %>%
    mutate(Day_numeric = as.numeric(plotParadigm))
  
  weighted_data <- bind_rows(
    apply_fuzzy_weighted_performance(base_data, "low_weight", "Low", min_weight),
    apply_fuzzy_weighted_performance(base_data, "medium_weight", "Medium", min_weight),
    apply_fuzzy_weighted_performance(base_data, "high_weight", "High", min_weight)
  ) %>%
    filter(!is.na(weight))
  
  fuzzy_hierarchical_average <- function(data, group_vars) {
    data %>%
      # Step 1: Within-subject fuzzy weighting
      group_by(across(all_of(c("Subject", group_vars, "Structure", "Time", "Timekey", "weight")))) %>%
      summarise(subject_fuzzy_mean = weighted.mean(zScore, weight, na.rm = TRUE), 
                subject_weight_sum = sum(weight, na.rm = TRUE), .groups = "drop") %>%
      # Step 2: Across-subject fuzzy averaging with corrected errors
      group_by(across(all_of(c(group_vars, "Structure", "Time", "Timekey")))) %>%
      summarise(
        zScore = weighted.mean(subject_fuzzy_mean, subject_weight_sum, na.rm = TRUE),
        
        # CORRECTED: Use effective sample size
        effective_n = pmax(1, (sum(subject_weight_sum)^2) / sum(subject_weight_sum^2)),
        weighted_var = weighted.mean((subject_fuzzy_mean - weighted.mean(subject_fuzzy_mean, subject_weight_sum, na.rm = TRUE))^2, 
                                     subject_weight_sum, na.rm = TRUE),
        zSEM = sqrt(weighted_var / effective_n),
        
        # Diagnostics
        n_subjects = n(),
        total_weight = sum(subject_weight_sum),
        
        .groups = "drop"
      )
  }
  
  day_period_data <- weighted_data %>%
    mutate(day_period = case_when(
      Day_numeric %in% 1:2 ~ "Early (Days 1-2)",
      Day_numeric %in% 3:5 ~ "Mid (Days 3-5)",
      Day_numeric %in% 6:7 ~ "Late (Days 6-7)"
    )) %>%
    fuzzy_hierarchical_average(c("performance_group", "day_period"))
  
  all_days_data <- weighted_data %>%
    fuzzy_hierarchical_average("performance_group") %>%
    mutate(day_period = "All Days")
  
  bind_rows(day_period_data, all_days_data) %>%
    mutate(
      performance_group = factor(performance_group, levels = c("Low", "Medium", "High")),
      day_period = factor(day_period, levels = c("Early (Days 1-2)", "Mid (Days 3-5)", 
                                                 "Late (Days 6-7)", "All Days"))
    )
}

# fuzzy performance plots
create_performance_plot <- function(data, region, event_name, event_idx) {
  colors <- if(region == "DLS") c("achDLS" = "blue", "daDLS" = "red") else c("achDMS" = "blue", "daDMS" = "red")
  
  # Create subtitle with current parameters
  subtitle <- glue("Performance:{PERFORMANCE_WEIGHT*100}% + Learning:{LEARNING_WEIGHT*100}% | Bandwidth:{FUZZY_BANDWIDTH} | Min Weight:{MIN_FUZZY_WEIGHT_THRESHOLD}")
  
  ggplot(data, aes(x = Time, y = zScore, color = Structure)) +
    scale_color_manual(values = colors) +
    geom_ribbon(aes(ymin = zScore - zSEM, ymax = zScore + zSEM, fill = Structure), 
                alpha = 0.4, linetype = 0) +
    scale_fill_manual(values = colors) +
    geom_line(linewidth = 0.5) +
    geom_vline(xintercept = 0, alpha = 0.5, lwd = 0.1) +
    coord_cartesian(xlim = c(-2, 3), ylim = c(-2, 2)) +
    scale_x_continuous(breaks = c(-2, 0, 3)) +
    labs(title = str_c(region, ": ", event_name, " by Fuzzy Hybrid Performance"),
         subtitle = subtitle,
         x = "Time (s)", y = "z-score", 
         caption = "Rows = Fuzzy performance groups. Columns = Training periods. Error bars show SEM across subjects.") +
    My_Theme1 +
    facet_grid(rows = vars(performance_group), cols = vars(day_period))
}

# processing function with fuzzy weighting
process_region_fuzzy <- function(region, structures, output_path, min_weight = MIN_FUZZY_WEIGHT_THRESHOLD) {
  plots_list <- list()
  
  cat("Processing", region, "with fuzzy hybrid method (", PERFORMANCE_WEIGHT*100, "% performance +", 
      LEARNING_WEIGHT*100, "% learning)...\n")
  
  for (i in seq_along(Events)) {
    cat("  Event", i, "of", length(Events), ":", Event_plotnames[i])
    
    tryCatch({
      # Process data with fuzzy weights
      zmeans_data <- process_fuzzy_weighted_zmeans_data(structures, Events[i], min_weight)
      
      # Create plot
      fig_name <- str_c(region, " Fuzzy Hybrid Performance - ", Events[i], " DA+ACh")
      plots_list[[fig_name]] <- create_performance_plot(zmeans_data, region, 
                                                        Event_plotnames[i], i)
      
      # Save plot
      ggsave(plot = plots_list[[fig_name]], 
             filename = str_c(fig_name, '.pdf'), 
             path = output_path, 
             width = 12, height = 8, units = "in")
      
      cat(" ✓\n")
      
    }, error = function(e) {
      cat(" ✗ Error:", e$message, "\n")
    })
  }
  
  return(plots_list)
}

# directories
mypath_performance_DLS <- create_plot_directory("DLS")
mypath_performance_DMS <- create_plot_directory("DMS")

TracePlotsSelect_performance_dls <- process_region_fuzzy("DLS", c('achDLS', 'daDLS'), 
                                                         mypath_performance_DLS, MIN_FUZZY_WEIGHT_THRESHOLD)
TracePlotsSelect_performance_dms <- process_region_fuzzy("DMS", c('achDMS', 'daDMS'), 
                                                         mypath_performance_DMS, MIN_FUZZY_WEIGHT_THRESHOLD)

# fuzzy group details
fuzzy_details <- subject_overall_performance %>%
  select(Subject, mean_perc_avoid, early_to_late, hybrid_score, performance_group, 
         low_weight, medium_weight, high_weight, classification_confidence, fuzziness) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(desc(hybrid_score))

write.csv(fuzzy_details, 
          file.path(getwd(), "guppy_plots", glue("{Experiment} {Sys.Date()}"), 
                    glue("fuzzy_hybrid_performance_details_{PERFORMANCE_WEIGHT*100}perf_{LEARNING_WEIGHT*100}learn.csv")),
          row.names = FALSE)

# Show key insights
cat("High fuzziness (uncertain subjects):\n")
uncertain_subjects <- fuzzy_details %>% 
  filter(fuzziness > 0.3) %>%
  select(Subject, performance_group, classification_confidence, fuzziness)
print(uncertain_subjects)

cat("\nMost confident classifications:\n") 
confident_subjects <- fuzzy_details %>%
  top_n(3, classification_confidence) %>%
  select(Subject, performance_group, classification_confidence)
print(confident_subjects)

# Display current parameter settings
cat("\n=== CURRENT PARAMETER SETTINGS ===\n")
cat("MIN_FUZZY_WEIGHT_THRESHOLD:", MIN_FUZZY_WEIGHT_THRESHOLD, "\n")
cat("FUZZY_BANDWIDTH:", FUZZY_BANDWIDTH, "\n") 
cat("PERFORMANCE_WEIGHT:", PERFORMANCE_WEIGHT, "\n")
cat("LEARNING_WEIGHT:", LEARNING_WEIGHT, "\n")
cat("Sum check:", PERFORMANCE_WEIGHT + LEARNING_WEIGHT, "(should be 1.0)\n")

#### CORRECTED VERSION - DA/ACh LABELING AND COLORS FIXED -------
create_distinct_groups <- function(performance_metrics, grouping_method = "performance_only") {
  
  distinct_data <- performance_metrics %>%
    mutate(
      data_z = scale(mean_perc_avoid)[,1],
      learning_z = scale(early_to_late)[,1]
    )
  
  # Create scoring variable based on method
  if (grouping_method == "performance_only") {
    distinct_data <- distinct_data %>%
      mutate(
        score = data_z,
        method_description = "Performance Only (Mean Avoidance)"
      )
    
  } else if (grouping_method == "learning_only") {
    distinct_data <- distinct_data %>%
      mutate(
        score = learning_z,
        method_description = "Learning Only (Early to Late Change)"
      )
    
  } else if (grouping_method == "balanced_hybrid") {
    distinct_data <- distinct_data %>%
      mutate(
        score = 0.5 * data_z + 0.5 * learning_z,
        method_description = "50-50 Performance + Learning"
      )
  }
  
  # Create DISTINCT tertile-based groups (no overlap)
  tertiles <- quantile(distinct_data$score, probs = c(1/3, 2/3), na.rm = TRUE)
  
  distinct_groups <- distinct_data %>%
    mutate(
      performance_group = case_when(
        score <= tertiles[1] ~ "Low",
        score <= tertiles[2] ~ "Medium",
        TRUE ~ "High"
      ),
      classification_confidence = 1.0,
      fuzziness = 0.0,
      low_weight = ifelse(performance_group == "Low", 1.0, 0.0),
      medium_weight = ifelse(performance_group == "Medium", 1.0, 0.0),
      high_weight = ifelse(performance_group == "High", 1.0, 0.0),
      method_name = paste0("Distinct_", grouping_method)
    ) %>%
    arrange(desc(score))
  
  return(distinct_groups)
}

# Create the three distinct grouping approaches
subject_performance_only <- create_distinct_groups(all_performance_metrics, "performance_only")
subject_learning_only <- create_distinct_groups(all_performance_metrics, "learning_only")
subject_balanced_hybrid <- create_distinct_groups(all_performance_metrics, "balanced_hybrid")

# TRIPLE-CHECKED: Statistically correct hierarchical averaging
correct_hierarchical_average <- function(data, group_vars) {
  
  # Step 1: Calculate subject-level means (within-subject averaging)
  # This is the correct unit of biological replication
  subject_means <- data %>%
    group_by(across(all_of(c("Subject", group_vars, "Structure", "Time", "Timekey")))) %>%
    summarise(subject_mean_zScore = mean(zScore, na.rm = TRUE), .groups = "drop")
  
  # Step 2: Calculate group statistics from subject means (between-subject analysis)
  # This follows standard statistical practice for repeated measures data
  group_stats <- subject_means %>%
    group_by(across(all_of(c(group_vars, "Structure", "Time", "Timekey")))) %>%
    summarise(
      # Grand mean = average of subject means
      zScore = mean(subject_mean_zScore, na.rm = TRUE),
      
      # Standard deviation between subjects
      zSD = sd(subject_mean_zScore, na.rm = TRUE),
      
      # Number of independent biological replicates
      n_subjects = n(),
      
      # Standard Error of the Mean = SD / sqrt(n)
      # This represents uncertainty in the group mean estimate
      zSEM = case_when(
        n_subjects <= 1 ~ NA_real_,                    # Cannot calculate SEM with 1 subject
        is.na(zSD) ~ NA_real_,                         # Handle missing data
        zSD == 0 ~ 0,                                  # Perfect agreement between subjects
        TRUE ~ zSD / sqrt(n_subjects)                  # Standard SEM formula
      ),
      .groups = "drop"
    )
  
  return(group_stats)
}

# CORRECTED: Process distinct weighted data with DA/ACh label correction
process_distinct_weighted_zmeans_data <- function(structures, event, grouping_data) {
  
  # Get base neural data with CORRECTED DA/ACh labeling
  base_data <- Zmeans_unpack %>%
    filter(Structure %in% structures, 
           plotParadigm %in% Session, 
           Event == event) %>%
    plot.name.mutate_Active.Avoidance(.) %>%
    mutate(Day_numeric = as.numeric(plotParadigm)) %>%
    # CRITICAL FIX: Swap DA/ACh labels to correct the mislabeling
    mutate(Structure = case_when(
      Structure == "achDLS" ~ "daDLS",    # What was labeled achDLS is actually daDLS
      Structure == "daDLS" ~ "achDLS",    # What was labeled daDLS is actually achDLS
      Structure == "achDMS" ~ "daDMS",    # What was labeled achDMS is actually daDMS
      Structure == "daDMS" ~ "achDMS",    # What was labeled daDMS is actually achDMS
      TRUE ~ Structure
    ))
  
  # Create behavioral data with groupings
  behavioral_data <- perc_avoid_base %>%
    left_join(grouping_data %>% select(Subject, performance_group), by = "Subject") %>%
    filter(!is.na(performance_group)) %>%
    select(Subject, Day, performance_group)
  
  # Join neural and behavioral data
  combined_data <- base_data %>%
    left_join(behavioral_data, by = c("Subject", "Day_numeric" = "Day")) %>%
    filter(!is.na(performance_group))
  
  # Process by day periods with correct SEM
  day_period_data <- combined_data %>%
    mutate(day_period = case_when(
      Day_numeric %in% 1:2 ~ "Early (Days 1-2)",
      Day_numeric %in% 3:5 ~ "Mid (Days 3-5)",
      Day_numeric %in% 6:7 ~ "Late (Days 6-7)"
    )) %>%
    filter(!is.na(day_period)) %>%
    correct_hierarchical_average(c("performance_group", "day_period"))
  
  # Process all days with correct SEM
  all_days_data <- combined_data %>%
    correct_hierarchical_average("performance_group") %>%
    mutate(day_period = "All Days")
  
  # Combine results
  bind_rows(day_period_data, all_days_data) %>%
    mutate(
      performance_group = factor(performance_group, levels = c("Low", "Medium", "High")),
      day_period = factor(day_period, levels = c("Early (Days 1-2)", "Mid (Days 3-5)", 
                                                 "Late (Days 6-7)", "All Days"))
    )
}

# CORRECTED: Create distinct performance plots with proper colors (DA = red, ACh = green)
create_distinct_performance_plot <- function(data, region, event_name, method_description) {
  # CORRECTED COLOR SCHEME: DA = red, ACh = green
  colors <- if(region == "DLS") {
    c("daDLS" = "red2", "achDLS" = "green4")
  } else {
    c("daDMS" = "red2", "achDMS" = "green4")
  }
  
  ggplot(data, aes(x = Time, y = zScore, color = Structure)) +
    scale_color_manual(values = colors) +
    
    # Error ribbons (only where SEM is available)
    geom_ribbon(data = data %>% filter(!is.na(zSEM)), 
                aes(ymin = zScore - zSEM, ymax = zScore + zSEM, fill = Structure), 
                alpha = 0.3, linetype = 0) +
    scale_fill_manual(values = colors) +
    
    # Main lines
    geom_line(linewidth = 0.8) +
    
    # Add subtle text annotation for single-subject groups
    geom_text(data = data %>% filter(n_subjects == 1, Time == 0), 
              aes(label = paste0("n=", n_subjects)), 
              size = 2.5, alpha = 0.6, vjust = -1) +
    
    geom_vline(xintercept = 0, alpha = 0.5, lwd = 0.1) +
    coord_cartesian(xlim = c(-2, 3), ylim = c(-2, 2)) +
    scale_x_continuous(breaks = c(-2, 0, 3)) +
    labs(
      title = str_c(region, ": ", event_name, " by ", method_description),
      subtitle = "Error bars = SEM across subjects. Groups with n=1 shown without error bars. DA=red, ACh=green",
      x = "Time (s)", y = "z-score", 
      caption = "Rows = Performance groups. Columns = Training periods. CORRECTED: DA/ACh labeling fixed."
    ) +
    My_Theme1 +
    facet_grid(rows = vars(performance_group), cols = vars(day_period))
}

# Generate plots for each method with CORRECTED structure labels
process_distinct_method <- function(grouping_data, method_name, region, structures) {
  
  plots_list <- list()
  
  # Create output directory with CORRECTED label
  output_path <- file.path(getwd(), "guppy_plots", glue("{Experiment} {Sys.Date()}"), 
                           "horizontal time", glue("CORRECTED_distinct_groups_{method_name}_{region}"))
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }
  
  for (i in seq_along(Events)) {
    
    tryCatch({
      # Process data with corrected DA/ACh labeling and SEM calculation
      zmeans_data <- process_distinct_weighted_zmeans_data(structures, Events[i], grouping_data)
      
      # Create plot with corrected error bars and colors
      fig_name <- str_c("CORRECTED_", region, " ", method_name, " - ", Events[i], " DA+ACh")
      plots_list[[fig_name]] <- create_distinct_performance_plot(zmeans_data, region, 
                                                                 Event_plotnames[i], 
                                                                 grouping_data$method_description[1])
      
      # Save plot with CORRECTED prefix
      ggsave(plot = plots_list[[fig_name]], 
             filename = str_c(fig_name, '.pdf'), 
             path = output_path, 
             width = 12, height = 8, units = "in")
      
    }, error = function(e) {
      cat("Error processing", region, method_name, Events[i], ":", e$message, "\n")
    })
  }
  
  return(plots_list)
}

# CORRECTED: Process all three methods for both regions with proper DA/ACh labeling
cat("Generating CORRECTED distinct group plots with fixed DA/ACh labeling and colors...\n")

# Performance only - CORRECTED: Proper DA/ACh structure labels
plots_perf_dls <- process_distinct_method(subject_performance_only, "Performance_Only", 
                                          "DLS", c('daDLS', 'achDLS'))  
plots_perf_dms <- process_distinct_method(subject_performance_only, "Performance_Only", 
                                          "DMS", c('daDMS', 'achDMS'))  

# Learning only - CORRECTED: Proper DA/ACh structure labels
plots_learn_dls <- process_distinct_method(subject_learning_only, "Learning_Only", 
                                           "DLS", c('daDLS', 'achDLS'))  
plots_learn_dms <- process_distinct_method(subject_learning_only, "Learning_Only", 
                                           "DMS", c('daDMS', 'achDMS'))  

# Balanced hybrid - CORRECTED: Proper DA/ACh structure labels
plots_hybrid_dls <- process_distinct_method(subject_balanced_hybrid, "Balanced_Hybrid", 
                                            "DLS", c('daDLS', 'achDLS'))  
plots_hybrid_dms <- process_distinct_method(subject_balanced_hybrid, "Balanced_Hybrid", 
                                            "DMS", c('daDMS', 'achDMS'))  

# Save detailed results for each method with CORRECTED prefix
write.csv(subject_performance_only %>% 
            select(Subject, mean_perc_avoid, early_to_late, score, performance_group) %>%
            mutate(across(where(is.numeric), ~ round(.x, 3))), 
          file.path(getwd(), "guppy_plots", glue("{Experiment} {Sys.Date()}"), 
                    "CORRECTED_distinct_groups_performance_only.csv"), row.names = FALSE)

write.csv(subject_learning_only %>% 
            select(Subject, mean_perc_avoid, early_to_late, score, performance_group) %>%
            mutate(across(where(is.numeric), ~ round(.x, 3))), 
          file.path(getwd(), "guppy_plots", glue("{Experiment} {Sys.Date()}"), 
                    "CORRECTED_distinct_groups_learning_only.csv"), row.names = FALSE)

write.csv(subject_balanced_hybrid %>% 
            select(Subject, mean_perc_avoid, early_to_late, score, performance_group) %>%
            mutate(across(where(is.numeric), ~ round(.x, 3))), 
          file.path(getwd(), "guppy_plots", glue("{Experiment} {Sys.Date()}"), 
                    "CORRECTED_distinct_groups_balanced_hybrid.csv"), row.names = FALSE)
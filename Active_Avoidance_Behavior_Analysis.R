library(tidyverse)
library(googlesheets4)
library(ggrepel)
library(glue)
library(ggprism)
gs4_deauth()


#### SETUP ----------------------------------
Experiment <- "Active.Avoidance"

#pull in Subject metadata
meta.data.link <- "https://docs.google.com/spreadsheets/d/1VPx4tm8clOViS7U8v_KSBg8S7oL2NFnLAkSvROfv-6o/edit?usp=sharing"
meta.data <- read_sheet(meta.data.link) %>%
  mutate(Subject = as.numeric(Subject)) %>%   
  rowwise() %>% 
  select(Subject, Group, Sex)

#behavior data to match Dates to experiments
Operant_Data <- readRDS(file = glue("{Experiment}_Operant_Data.Rds"))
Operant_Data <- Operant_Data %>% 
  mutate(Subject = as.numeric(as.character(Subject)),  # Convert factor/character to numeric properly
         Date = date(DateTime)) %>%
#  select(Subject, Date, Paradigm.Day) %>% 
  distinct() %>% 
  arrange(Subject, Paradigm.Day) %>% 
  left_join(meta.data)

Operant_Data <- Operant_Data %>% 
  left_join(meta.data) %>% filter(Subject != 42) %>% filter(Subject != 44)

col.pal.Latency = setNames(object = c("#2278bd", "#f66723"),
                       nm =     c("total.avoid.latency", "total.escape.latency"))
sem2 <-function(x){ tribble( #try implementing with stat_summary as before in commented section
  ~ymin,                           ~y,               ~ymax,
  mean(x)-sd(x)/sqrt(length(x)), mean(x), mean(x)+sd(x)/sqrt(length(x))
)
}

Operant_Data <- Operant_Data %>% filter(Subject < 1000)


#### percent avoided -----
perc_avoid <-  Operant_Data %>% 
  filter(name %in% c("total.avoids", "total.trials.run")) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  mutate(perc_avoid = 100*total.avoids/total.trials.run)

perc_avoid_ctrl <- perc_avoid %>% 
  filter(Group == "CTRL")  # Keep only CTRL data

perc_avoid_all_sex <- perc_avoid_ctrl %>% 
  mutate(Sex = "All")  # Create artificial "All" category

plot_data <- bind_rows(perc_avoid_ctrl, perc_avoid_all_sex) %>% 
  mutate(Sex = factor(Sex, levels = c("Female", "Male", "All")))  
# 
# ggplot(plot_data, aes(x = Paradigm.Day, y = perc_avoid)) +
#   geom_path(aes(group = Subject), alpha = 0.6) +
#   geom_path(stat = "summary", fun.data = sem2,
#             linewidth = 1.5, color = "red") +
#   facet_grid(rows = vars(Sex)) +
#   scale_x_continuous(breaks = 1:7) + xlim(1,7)
#   labs(title = "Percent Avoidance Across Days",
#        x = "Day", y = "% Avoided") +
#     theme_prism(border = TRUE, base_size = 14) +
#     scale_y_continuous(breaks = 1:100)

ggplot(plot_data, aes(x = Paradigm.Day, y = perc_avoid)) +
  geom_line(aes(group = Subject), alpha = 0.6) +
  geom_vline(xintercept = 7, linewidth = 2.5, color = "pink2") +
  geom_vline(xintercept = 8, linewidth = 2.5, color = "lightblue2") +
  geom_vline(xintercept = 10, linewidth = 2.5, color = "yellow3") +
  geom_vline(xintercept = 11, linewidth = 2.5, color = "lightgreen") +
  geom_vline(xintercept = 12, linewidth = 2.5, color = "orchid4") +
  geom_path(stat = "summary", fun.data = sem2,            
            linewidth = 1.5, color = "#FF0000") +
  # facet_grid(rows = vars(Sex)) +                         
  labs(title = "Percent Avoidance Across Days",
       x = "Day", y = "% Avoided") +
  theme_prism(border = TRUE, base_size = 14) +
  theme(
    legend.position = "right",
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12)
  ) +
  # scale_x_continuous(breaks = 1:7, limits = c(1,7)) +
  scale_x_continuous(breaks = 1:12, limits = c(1,12))+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20))



#### avoid escape latency -------
cross_latency <-  Operant_Data %>% 
   filter(name %in% c("total.escape.latency", "total.avoid.latency"), value > 0) %>% 
   select(!c(Trial, Date, DateTime, Instance, Paradigm))

cross_latency_all_sex <- cross_latency %>%
  mutate(Sex = "All")  # Duplicate data with Sex = "All"

plot_data <- bind_rows(cross_latency, cross_latency_all_sex) %>%
  mutate(Sex = factor(Sex, levels = c("Female", "Male", "All")))  # Control order

ggplot(plot_data, aes(x = Paradigm.Day, y = value, color = name)) +
  scale_color_manual(values = col.pal.Latency, labels = c(
    "total.escape.latency" = "Escape",  
    "total.avoid.latency" = "Avoid"
  )) +
  geom_point(size = 1.5, alpha = 0.8, position = position_jitter(width = 0.1)) +
  geom_path(stat = "summary", fun.data = sem2, linewidth = 0.2) +
  scale_x_continuous(breaks = 1:7) + scale_y_continuous(limits = c(0, 8)) + xlim(1,7) +
  facet_grid(rows = vars(Sex), cols = vars(Group)) +  
  labs(title = "Cross Latency (to Avoid or Escape)", color = "Latency Type",
       x = "Day", y = "Time (s)") 


#### cross latency ---------
cross_latency_freq <- Operant_Data %>% 
  filter(name %in% c("Cue", "Cue.End"),
         Group == "CTRL") %>%  # Keep only CTRL
  pivot_wider(values_from = value, names_from = name) %>% 
  mutate(latency = Cue.End - Cue) %>% 
  select(!c(Trial, Date, DateTime, Instance, Paradigm, Cue, Cue.End))

cross_latency_summarize_freq <- cross_latency_freq %>%
  group_by(Subject, Day, Sex, Paradigm.Day) %>%  # Removed Group grouping
  summarize(mean_latency = mean(latency)) %>% 
  bind_rows(
    cross_latency_freq %>%
      group_by(Subject, Day, Paradigm.Day) %>%  # No Sex grouping
      summarize(mean_latency = mean(latency)) %>%
      mutate(Sex = "All")
  ) %>%
  mutate(Sex = factor(Sex, levels = c("Female", "Male", "All")))

ggplot(cross_latency_summarize_freq, aes(x = Paradigm.Day, y = mean_latency)) +
  geom_point(alpha = 0.5, size = 2, position = position_jitter(width = 0.1)) +
  geom_path(stat = "summary", fun.data = sem2, linewidth = 0.5) +
  # scale_x_continuous(breaks = 1:7) +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(limits = c(0, 8)) +
  # facet_grid(rows = vars(Sex)) +  # Only Sex facets (no columns needed)
  labs(
    title = "Mean Latency to Cross",
    x = "Day", 
    y = "Time (s)"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(),
    plot.caption = element_text(color = "gray40")
  ) +
  theme_prism(border = TRUE, base_size = 14)


#### freq distribution M/F split -----
calculate_freq_distribution <- function(data, binwidth) {
  data %>%
    group_by(Subject, Paradigm.Day, Group, Sex) %>%
    mutate(bin = floor(latency / binwidth) * binwidth) %>%
    count(bin) %>%
    ungroup()
}

freq_distribution <- calculate_freq_distribution(cross_latency_freq, 1)

average_freq_distribution <- freq_distribution %>%
  group_by(Paradigm.Day, bin, Group, Sex) %>%
  summarise(mean_count = mean(n, na.rm = TRUE)) %>%
  ungroup()

ggplot(average_freq_distribution, aes(x = bin, y = mean_count, color = factor(Paradigm.Day))) +
  geom_line(size = 1) +
  facet_grid(rows = vars(Sex), cols = vars(Group), margins = FALSE) +
  labs(title = "Average Frequency of Latency to Cross by Day",
       x = "Cross Latency (binned)", y = "Average Frequency",
       color = "Paradigm Day") +
  scale_x_continuous(limits = c(0, 10)) +
  theme_minimal()


#### cross latency binned all subjects -----
calculate_freq_distribution_all <- function(data, binwidth) {
  data %>%
    group_by(Subject, Paradigm.Day) %>%
    mutate(bin = floor(latency / binwidth) * binwidth) %>%
    count(bin) %>%
    ungroup()
}

freq_distribution_all <- calculate_freq_distribution_all(cross_latency_freq, 1)

average_freq_distribution_all <- freq_distribution_all %>%
  group_by(Paradigm.Day, bin) %>%
  summarise(mean_count = mean(n, na.rm = TRUE)) %>%
  ungroup()

ggplot(average_freq_distribution_all, aes(x = bin, y = mean_count, color = factor(Paradigm.Day))) +
  geom_line(size = 1) +
  labs(title = "Average Frequency of Latency to Cross by Day",
       x = "Cross Latency (s, binned)", y = "Average Frequency",
       color = "Day") +
  scale_x_continuous(limits = c(0, 10)) +
  theme_minimal()

#### plot percent avoided binned -----
plot_avoid_perc_bin <-  Operant_Data %>% 
  filter(name %in% c("Avoid", "Escape")) %>% 
  arrange(Subject, Paradigm.Day, value) %>% 
  group_by(Subject, Paradigm.Day) %>% 
  mutate(rn = row_number(), bin = case_when(rn < 11 ~ 1,
                                            rn < 21 ~ 2,
                                            rn < 31 ~ 3)) %>% 
  filter(name == "Avoid") %>% 
  group_by(Subject, Paradigm.Day, bin) %>% 
  summarise(perc_avoided = 100 * n()/10, .groups = 'drop')

plot_avoid_perc_bin_complete <- plot_avoid_perc_bin %>%
  complete(Subject, Paradigm.Day, bin = 1:3) %>%
  replace_na(list(perc_avoided = 0)) %>% 
  arrange(Subject, Paradigm.Day) %>% 
  left_join(Operant_Data %>% select(Subject, Group, Sex) %>% distinct()) 

ggplot(plot_avoid_perc_bin_complete, aes(x = factor(Paradigm.Day), y = perc_avoided, fill = factor(Paradigm.Day), group = factor(bin))) +
  geom_bar(stat = "summary", fun.data = sem2, position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(stat = "summary", fun.data = sem2, position = position_dodge(width = 0.8), width = 0.25)+
  labs(x = "Day", y = "Percentage Avoided", fill = "Day", title = "Percentage Avoided by Day (Binned)") + scale_y_continuous(limits = c(0, 100))



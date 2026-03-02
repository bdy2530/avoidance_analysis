# > Operant_Data$name %>% unique()
# [1] "Cue"                  "Cue.End"              "Escape"               "Shock"                "crossings.count"     
# [6] "escape.latency"       "Avoid"                "avoid.latency"        "total.trials.run"     "total.avoids"        
# [11] "total.avoid.latency"  "total.escapes"        "total.escape.latency" "total.left.movement"  "total.right.movement"
# [16] "total.crossings"      "total.ITI.shocks"     "crossings"      

library(tidyverse)
library(googlesheets4)
library(ggrepel)
library(glue)
gs4_deauth()

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
         Date = date(DateTime))
Operant_metadata <- Operant_Data %>% 
  mutate(Date = date(DateTime)) %>% 
  mutate(Subject = as.numeric(Subject)) %>%
  select(Subject, Date, Paradigm.Day) %>% 
  distinct() %>% 
  arrange(Subject, Paradigm.Day) %>% 
  left_join(meta.data) %>% filter(Subject != 42) %>% filter(Subject != 44)

Operant_Data <- Operant_Data %>% 
  mutate(Date = date(DateTime)) %>% 
  left_join(meta.data)

#Find and attach Cue with leading 
cue_ids <- Operant_Data %>% 
  arrange(Subject, DateTime, value) %>% 
  filter(!name == "Cue.End", name %in% c("Cue", "Escape", "Avoid")) %>% 
  mutate(name = case_when(name == "Cue" & lead(name) == "Escape" ~ "Cue_Escape",
                          name == "Cue" & lead(name) == "Avoid" ~ "Cue_Avoid",
                          TRUE ~ name)) %>% 
  filter(str_detect(name, "Cue"))

saveRDS(cue_ids, file = glue("cue_ids_{Experiment}"))

# cue_ids <- readRDS( file = "cue_ids_Active_Avoidance")

#now run readGuppy_means_Active_Avoidance_CueOnTrialDefined.R next to extract escape/avoid trials







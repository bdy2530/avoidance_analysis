#Find photometry recordings worth analysis based on guppy screening/notes


library(tidyverse)
library(ggrepel)
library(gridExtra)
library(glue)
library(tictoc)
library(patchwork)
library(grid)
library(googlesheets4)
library(xlsx)
library(lubridate)
gs4_deauth()

synapse_eval_link <- "https://docs.google.com/spreadsheets/d/1FNJXXd2jJ7Pbq_uwR8aN11q5FHH3kqWftUo_nug5vdM/edit?usp=sharing"
synapse_eval <- read_sheet(synapse_eval_link) %>% as_tibble() 
Experiment <- "Active.Avoidance"

setup_metadata_link <- "https://docs.google.com/spreadsheets/d/1VPx4tm8clOViS7U8v_KSBg8S7oL2NFnLAkSvROfv-6o/edit?usp=sharing"
setup_metadata <- read_sheet(setup_metadata_link) %>% as_tibble() %>% 
select(Subject, Group, Sex)

Operant_Data <- readRDS(file = glue("{Experiment}_Operant_Data.Rds")) %>% 
  mutate(Subject = as.numeric(Subject)) %>%   # Convert to numeric (09497 becomes 9497)
  left_join(setup_metadata, by = "Subject")


meta_Data <- readRDS(file = glue("{Experiment}_meta_Data.Rds"))

meta_addDay <- meta_Data %>% select(Subject, Paradigm, Paradigm.Day, Day)


#keep in mind, this screening is for verdicts to keep and evaluations/Quality of better than "poor" and more than 6 qualifying entries
#may want to rerun with all non-poor data, or with higher standards too
synapse_eval_trim <- synapse_eval %>% 
  select(!c(Notes, Artifacts_daDMS, Artifacts_daDLS, Artifacts_achDMS, Artifacts_achDLS)) %>% 
  mutate(Subject = str_extract(Filename, "^\\d+(?=-)"),
         Date = str_match(Filename, "(?<=-)\\d{6}") %>% str_c("20", .)) %>%
  mutate(Quality_daDMS = str_c(Quality_daDMS, " ", Verdict_daDMS), Quality_daDLS = str_c(Quality_daDLS, " ", Verdict_daDLS),
         Quality_achDMS = str_c(Quality_achDMS, " ", Verdict_achDMS), Quality_achDLS = str_c(Quality_achDLS, " ", Verdict_achDLS)) %>% 
  select(!c(Verdict_daDMS, Verdict_daDLS, Verdict_achDMS, Verdict_achDLS)) %>%
  pivot_longer(cols = starts_with("Quality"), names_to = "Structure", values_to = "eval") %>%
  separate(eval, into = c("eval", "verdict"), " ") %>% 
  mutate(Structure = str_extract(Structure, "(?<=_).*")) %>% 
  filter(!eval == "poor", !is.na(eval), str_detect(verdict, "keep")) %>% 
  group_by(Subject, Structure) %>% 
  # filter(n() > 6) %>%  #this is the line that changes how many days need to have good signal to run
  ungroup()

saveRDS(synapse_eval_trim, "Photometry_Eval.Active_Avoidance")

  
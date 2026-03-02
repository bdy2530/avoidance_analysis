# MED to PRISM
library(readtext)
library(tidyverse)
library(lubridate)
library(readxl)
library(glue)
source("C:/Users/bdy2530/Desktop/avoidance_analysis/functionsPIT_New.R")

Experiment <- c("Active.Avoidance")

Operant_Data <- readRDS(file = glue("{Experiment}_Operant_Data.Rds"))
meta.data <- readRDS(file = glue("{Experiment}_meta_Data.Rds"))

Operant_Data <- Operant_Data %>% select(!DateTime) %>% select(!Paradigm) %>% select(!Day)

escapelat_persub_perday <- Operant_Data %>%
  filter(name == "total.escape.latency") %>%
  select(-Trial, -Instance) %>%
  mutate(
    Subject = as.character(Subject),
    value   = na_if(value, 0)   # turn 0 into NA
  ) %>%
  pivot_wider(names_from = Subject, values_from = value) %>%
  select(-name) %>%
  write.csv(file = glue("{Experiment}_escapelat_persub_perday.csv"), row.names = FALSE)

avoidlat_persub_perday <- Operant_Data %>%
  filter(name == "total.avoid.latency") %>%
  select(-Trial, -Instance) %>%
  mutate(
    Subject = as.character(Subject),
    value   = na_if(value, 0)
  ) %>%
  pivot_wider(names_from = Subject, values_from = value) %>%
  select(-name) %>%
  write.csv(file = glue("{Experiment}_avoidlat_persub_perday.csv"), row.names = FALSE)

totalcross_persub_perday <- Operant_Data %>%
  filter(name == "total.crossings") %>%
  select(-Trial, -Instance) %>%
  mutate(
    Subject = as.character(Subject),
    value   = na_if(value, 0)
  ) %>% 
  pivot_wider(names_from = Subject, values_from = value) %>%
  select(-name) %>%
  write.csv(file = glue("{Experiment}_totalcross_persub_perday.csv"), row.names = FALSE)

percentavoid_persub_perday <- Operant_Data %>%
  filter(name == "total.avoids") %>% 
  select(-Trial, -Instance) %>%
  mutate(
    Subject = as.character(Subject),
    value   = 100*value/30
  ) %>% 
  pivot_wider(names_from = Subject, values_from = value) %>%
  select(-name) %>%
  write.csv(file = glue("{Experiment}_percentavoid_persub_perday.csv"), row.names = FALSE)
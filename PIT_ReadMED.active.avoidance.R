#loads packages
library(readtext)
library(tidyverse)
library(lubridate)
library(readxl)
library(glue)
#library(zoo)
#options(warn = 2)
source("C:/Users/bdy2530/Desktop/Analysis/functionsPIT_New.R")

Experiments <- c("Active.Avoidance")

for (i in Experiments){
  Experiment <- i
  
  #Gets list of files in path excluding folders
  DirWithMEDfiles <- r"(R:\Basic_Sciences\Phys\Lerner_Lab_tnl2633\Baran\MedPC\ActiveAvoidance\)"
  MedFiles <- setdiff(list.files(path = DirWithMEDfiles, full.names = TRUE), 
                     list.dirs(path = DirWithMEDfiles, full.names = TRUE, recursive = FALSE))
  
  #scans folder for files without .txt and adds so they can be opened (due to laziness on rec days). KEEP OTHER FILE TYPES AWAY FROM THIS
  for (i in 1:length(MedFiles)){
    if (!str_detect(MedFiles[i],'.txt')){
      file.rename(MedFiles[i], str_c(MedFiles[i],'.txt'))
    }
  }
  
  #rerun this line to get updated list if .txt added (could add if statement, not necessary if no .txt added)
  MedFiles <- setdiff(list.files(path = DirWithMEDfiles, full.names = TRUE), 
                     list.dirs(path = DirWithMEDfiles, full.names = TRUE, recursive = FALSE))
  
  #Loads preexisting data, if nothing makes an empty and saves it prior to loading
  
  MED.R.Name.Conversion <- read_xlsx("RK_ActiveAvoidance_MEDtoR_variableNames.xlsx") #MED to R variable name associations
  
  #Subject.Paradigm <- readRDS(file = glue("{Experiment}.Subject.Paradigm.Rds")) #Paradigm assignment data
  
  if (!file_test("-f", glue("{Experiment}_Operant_Data.Rds"))){ #behavioral data
    Operant_Data <- tibble()
    saveRDS(Operant_Data, file = glue("{Experiment}_Operant_Data.Rds"))
  }else {
    Operant_Data <- readRDS(file = glue("{Experiment}_Operant_Data.Rds"))
  }
  
  if (!file_test("-f", glue("{Experiment}_meta_Data.Rds"))){ #meta data (somoe of which is packages wtih behavioral data)
    meta_Data <- tibble()
    saveRDS(meta_Data, file = glue("{Experiment}_meta_Data.Rds"))
  }else {
    meta_Data <- readRDS(file = glue("{Experiment}_meta_Data.Rds"))
  }
  
  if (!file_test("-f", glue("{Experiment}_settings_num.Rds"))){ #MED settings harvested from output (all numeric)
    settings_num <- tibble()
    saveRDS(settings_num, file = glue("{Experiment}_settings_num.Rds"))
  }else {
    settings_num <- readRDS(file = glue("{Experiment}_settings_num.Rds"))
  }

  # Main Loop for extracting MED data into R --------------------------------
  for (i0 in 1:length(MedFiles)){
    
    fileNameData <- str_split(MedFiles[i0], "/|\\\\")[[1]]
    fileNameData <- str_replace(fileNameData[length(fileNameData)], ".txt", "")
    
    if (str_detect(fileNameData, glue("^{Experiment}_"))){
      MED <- ReadNameMED_ActiveAvoidance(MedFiles, i0)
      
      #this can be moved into modulesLBN.R when you have more data to import
      #MEDOut is superfluous and should BE the output of m$ReaNamMED... the fct things can also be added, though piping will have to be removed
      meta.add <- as_tibble(MED$meta) %>%
        mutate(DateTime = mdy_hms(str_c(Date, StartTime, sep = "T"), tz = "US/Central"), 
               DateTimeEnd = mdy_hms(str_c(Date, EndTime, sep = "T"), tz = "US/Central"), 
        ) %>%
        select(!c(Date, StartTime, EndTime)) %>%
        relocate(DateTime, Subject, Paradigm, Paradigm.Day, Box, ProgramName)
     # meta.add <- meta.add %>% mutate(Paradigm = factor(Paradigm, levels = levels(Subject.Paradigm$Paradigm), ordered = TRUE))
      
      meta_Data <- bind_rows(meta_Data, meta.add) %>% 
        group_by(Subject) %>%
        arrange(Paradigm, Paradigm.Day) %>%
        mutate(Day = row_number(), Paradigm.Day = as.numeric(Paradigm.Day)) %>%
        relocate(Day, .after = Paradigm.Day) %>%
        ungroup()
      
      
      
      
      MEDOut <-  tibble(enframe(MED$out)) %>% 
        unnest_longer(value) %>%
        group_by(name) %>%
        mutate(Instance = row_number()) %>%
        left_join(meta.add %>% select(DateTime, Subject, Paradigm, Paradigm.Day), by = character()) %>%
        left_join(meta_Data %>% select(Subject, Paradigm, Paradigm.Day, Day)) %>%
        relocate(DateTime, Subject, Paradigm, Paradigm.Day) %>%
        ungroup()
      #let's remove the data and leave settings and counters behind to label according to MED.R.Name.Conversion
      settings_num_add <- MEDOut %>% 
        filter(name %in% c("settings")) %>%
        select(c(Subject, Paradigm, Paradigm.Day, Day,  name, value, Instance)) %>%
        left_join(MED.R.Name.Conversion %>% rename(name = R) %>% select(!DIM), by = c("name", "Instance")) %>% 
        select(!c(name,Instance)) %>%
        rename(name = rename.R) %>%
        filter(!name == "unused") %>%
        pivot_wider(names_from = c(name), names_sep = "", values_from = value)
      
      #let's include in only data, not settings, move settings into MEDOut.meta above
      op.summary.data <- MEDOut %>% filter(name %in% c("summary")) %>% 
        left_join(MED.R.Name.Conversion %>% rename(name = R) %>% select(!DIM), by = c("name", "Instance")) %>% 
        select(!c(name,Instance)) %>%
        rename(name = rename.R) %>%
        filter(!name == "unused")
      
      op.trial.data <-  tibble(enframe(MED$out)) %>% 
        unnest_longer(value) %>%
        group_by(name) %>%
        mutate(Instance = row_number()) %>% 
        filter(name == "trial.data") %>% 
        .$value %>% 
        MED.array.detangle() %>% left_join(meta.add %>% select(DateTime, Subject, Paradigm, Paradigm.Day), by = character()) %>%
        left_join(meta_Data %>% select(Subject, Paradigm, Paradigm.Day, Day)) %>%
        relocate(DateTime, Subject, Paradigm, Paradigm.Day)
      
      op.MEDOut <- MEDOut %>% filter(name %in% c("elapsed.time", "crossings"))
      
      
      
      #aggregates rows as they load in
      Operant_Data <- bind_rows(Operant_Data, op.trial.data, op.summary.data, op.MEDOut)
      settings_num <- bind_rows(settings_num, settings_num_add)

      
      
      #move file once read
      if(!dir.exists(str_c(DirWithMEDfiles, "/movedByRScript/"))){
        dir.create(str_c(DirWithMEDfiles, "/movedByRScript/"))
      }
      
      
      invisible(file.rename(MedFiles[i0], str_c(DirWithMEDfiles, "movedByRScript/", basename(MedFiles[i0]))))
      
      #save PIT_Operant_Data
      saveRDS(Operant_Data, file = glue("{Experiment}_Operant_Data.Rds"))
      saveRDS(meta_Data, file = glue("{Experiment}_meta_Data.Rds"))
      saveRDS(settings_num, file = glue("{Experiment}_settings_num.Rds"))
    }
  }
}
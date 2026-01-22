# Next Day functions for particular Experiments ---------------------------

next.day.LBN <- function(settings_char, settings_num, Subject.Paradigm){
  #here we progress down the logic tree in order of paradigm progression
  max.day <- settings_char %>% #get the last day run
    group_by(Subject) %>%
    filter(Day == max(Day))
  
  next.day.all <- tibble() #this will be our primary output, we will bind rows onto this as we iterate
  for (i in seq_len(nrow(max.day))){ #iterate through most recent days per animal
    max.day.sub <-  max.day[i,] #grabs i'th subject
    max.day <- max.day.sub$Day #this is the last day (also length of Subject column/table)
    
    Subject.Paradigm.sub <- Subject.Paradigm %>% filter(Subject == max.day.sub$Subject) #this is the the 'planned days' and we are getting the subset for the current subject
    
    settings_char.sub <- settings_char %>% filter(Subject == max.day.sub$Subject) #_char and _num are virtually the same but one has descriptions and one has numbers for inputs/outputs (e.g. 'left port' vs 1)
    settings_num.sub <- settings_num %>% filter(Subject == max.day.sub$Subject) #here we are just getting filtering/subsetting by subject
    max.day.num <- settings_num.sub %>% filter(Day == max.day.sub$Day)
    
    update <- FALSE #if we add days we have to update the 'planned days' object to keep all the days consistent on a per-subject basis
    if (max.day.sub$Paradigm == "LBN.Hab"){
      next.day.add <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day +1)
    } else if (max.day.sub$Paradigm == "LBN.Mag"){
      next.day.add <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day +1)
    } else if (max.day.sub$Paradigm == "LBN.FR1b"){ #compare two values and choose path (FR1 both -> FR1 S)
      if (max.day.sub$N_Lever_1_Rew <= 25 & max.day.sub$N_Lever_2_Rew <= 25){
        #add another of the same day
        next.day.add <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day) %>% 
          mutate(Day = Day + 1, Paradigm.Day = Paradigm.Day +1)
        update <- TRUE
      } else if (max.day.sub$N_Lever_1_Rew > 25) { #lever.reward1 stays on, turn off lever.reward2
        #move to next day and null reward2
        next.day.add <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day +1) %>% 
          mutate(lever.reward2 = 0)
      } else{ #by elimination max.day.sub$N_Lever_2_Rew  > 25 so move to next day and null reward1
        next.day.add <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day +1) %>% 
          mutate(lever.reward1 = 0)
      }
    } else if (max.day.sub$Paradigm == "LBN.FR1S"){
      if (max.day.sub$Paradigm.Day == 1) { #for checking 2 days back, for instance
        #add another of the same day
        next.day.add <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day) %>% 
          mutate(Day = Day + 1, Paradigm.Day = Paradigm.Day +1)
        update <- TRUE
      } else {
        vals <- settings_char.sub$N_Rewards[(max.day-1):max.day] #set number of days to test over
        if (all(vals > 30)){
          #move to next day - replace prior day lever values with current day lever value 
          next.day.add <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day +1) %>% 
            mutate(lever.reward1 = max.day.num$lever.reward1, lever.reward2 = max.day.num$lever.reward2)
        } else{
          #add another of the same day
          next.day.add <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day) %>% 
            mutate(Day = Day + 1, Paradigm.Day = Paradigm.Day +1)
          update <- TRUE
        }
      }
    } else if (max.day.sub$Paradigm == "LBN.RI30S"){
      if (settings_char.sub$N_Rewards > 30){
        #move to next day - replace prior day lever values with current day lever value 
        next.day.add <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day +1) %>% 
          mutate(lever.reward1 = max.day.num$lever.reward1, lever.reward2 = max.day.num$lever.reward2)
      } else {
        #add another of the same day
        next.day.add <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day) %>% 
          mutate(Day = Day + 1, Paradigm.Day = Paradigm.Day +1)
        update <- TRUE
      }
    } else if (max.day.sub$Paradigm == "LBN.RI60S"){
      #move to next day - replace prior day lever values with current day lever value 
      next.day.add <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day +1) %>% 
        mutate(lever.reward1 = max.day.num$lever.reward1, lever.reward2 = max.day.num$lever.reward2)
    } else if (max.day.sub$Paradigm == "LBN.Shock1"){
      #move to next day - replace prior day lever values with current day lever value 
      next.day.add <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day +1) %>% 
        mutate(lever.reward1 = max.day.num$lever.reward1, lever.reward2 = max.day.num$lever.reward2)
    } else if (max.day.sub$Paradigm == "LBN.RI60S_S1"){
      if (max.day.sub$Paradigm.Day >= 12){
        #move to next day - replace prior day lever values with current day lever value 
        next.day.add <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day +1) %>% 
          mutate(lever.reward1 = max.day.num$lever.reward1, lever.reward2 = max.day.num$lever.reward2)
      } else {
        next.day.add <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day) %>% 
          mutate(Day = Day + 1, Paradigm.Day = Paradigm.Day +1)
        update <- TRUE
      }
    } else if (max.day.sub$Paradigm == "LBN.Shock2"){
      #move to next day - replace prior day lever values with current day lever value 
      next.day.add <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day +1) %>% 
        mutate(lever.reward1 = max.day.num$lever.reward1, lever.reward2 = max.day.num$lever.reward2)
    } else if (max.day.sub$Paradigm == "LBN.RI60S_S2"){
      DayPriorShock2 <- settings_num.sub %>% filter(Paradigm == "LBN.Shock2") %>% .$Day-1
      NPtoBeat <- settings_num.sub %>% filter(Day == DayPriorShock2) %>% 
        mutate(NPtoBeat = case_when(lever.reward1 > 0 & lever.reward2 > 0 ~ N_Lever_1 + N_Lever_2,
                                    lever.reward1 > 0 ~ N_Lever_1, 
                                    lever.reward2 > 0 ~ N_Lever_2,
                                    TRUE ~ NA_real_)) %>%
        .$NPtoBeat
      CurrNum <- max.day.num %>% 
        mutate(NPtoBeat = case_when(lever.reward1 > 0 & lever.reward2 > 0 ~ N_Lever_1 + N_Lever_2,
                                    lever.reward1 > 0 ~ N_Lever_1, 
                                    lever.reward2 > 0 ~ N_Lever_2,
                                    TRUE ~ NA_real_)) %>%
        if (CurrNum < NPtoBeat){
          #move to next day - replace prior day lever values with current day lever value 
          next.day.add <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day +1) %>% 
            mutate(lever.reward1 = max.day.num$lever.reward1, lever.reward2 = max.day.num$lever.reward2)
        } else {
          next.day.add <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day) %>% 
            mutate(Day = Day + 1, Paradigm.Day = Paradigm.Day +1)
          update <- TRUE
        }
    } else {next.day.add <- NA}
    #last two I think.... here we are looking to match or exceed the last day of pre shock2 effort on the active side
    
    next.day.all <- bind_rows(next.day.all, next.day.add)
    
    if (update){
      Subject.Paradigm <- Subject.Paradigm %>%
        add_row(next.day.add) %>%
        rowwise() %>%
        run.create.macro.cell(.) %>%
        group_by(Subject) %>%
        arrange(Paradigm, Paradigm.Day) %>%
        mutate(Day = row_number()) %>% 
        group_by(Subject, Paradigm) %>%
        mutate(Paradigm.Day = row_number(), nParadigm.Day = max(Paradigm.Day)-(Paradigm.Day-1)) %>%
        ungroup()
      update <- FALSE
    }
  }
  next.day.all <- run.create.macro.cell(next.day.all)
  
  
  out <- list("next.day" = next.day.all, "Subject.Paradigm2" = Subject.Paradigm )
  # assign("next.day", next.day.all, envir = .GlobalEnv)
  # assign("Subject.Paradigm2", Subject.Paradigm, envir = .GlobalEnv)
}

next.day.PITTrial <- function(settings_char, settings_num, Subject.Paradigm){
  #here we progress down the logic tree in order of paradigm progression
  max.day.subjects <- settings_char %>% #get the last day run
    group_by(Subject) %>%
    filter(Day == max(Day))
  
  #max.day.subjects <- max.day.subjects %>% filter(Subject %in% c("8626"))
  
  next.day.all <- tibble() #this will be our primary output, we will bind rows onto this as we iterate
  for (i2 in seq_len(nrow(max.day.subjects))){ #iterate through most recent days per animal
    max.day.sub <-  max.day.subjects[i2,] #grabs i'th subject
    max.day <- max.day.sub$Day #this is the last day (also length of Subject column/table)
    
    Subject.Paradigm.sub <- Subject.Paradigm %>% filter(Subject == max.day.sub$Subject) #this is the the 'planned days' and we are getting the subset for the current subject
    
    settings_char.sub <- settings_char %>% filter(Subject == max.day.sub$Subject) #_char and _num are virtually the same but one has descriptions and one has numbers for inputs/outputs (e.g. 'left port' vs 1)
    settings_num.sub <- settings_num %>% filter(Subject == max.day.sub$Subject) #here we are just getting filtering/subsetting by subject
    max.day.num <- settings_num.sub %>% filter(Day == max.day.sub$Day)
    
    update <- FALSE #if we add days we have to update the 'planned days' object to keep all the days consistent on a per-subject basis
    # current.para.on.list <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day) %>% select(!c(Day, Paradigm.Day, nParadigm.Day, macroFile))
    # next.para.on.list <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day +1) %>% select(!c(Day, Paradigm.Day, nParadigm.Day, macroFile))
    # if (!all.equal(next.para.on.list, current.para.on.list)){
    if (max.day.sub$Paradigm == "GenPIT.CRF"){ #compare two values and choose path (FR1 both -> FR1 S)
      if (max.day.sub$Paradigm.Day > 1){
        vals <- settings_char.sub$N_Rewards[(max.day-1):max.day]
        if (all(vals > 4)){
          #go to next day
          next.day.add <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day +1)
        } else {next.day.add <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day) %>% 
          mutate(Day = Day + 1, Paradigm.Day = Paradigm.Day +1)
        update <- TRUE}
      } else {next.day.add <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day) %>% 
        mutate(Day = Day + 1, Paradigm.Day = Paradigm.Day +1)
      update <- TRUE}
    }  else {
      next.day.add <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day +1)
    }
    #} else {next.day.add <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day +1)}
    
    next.day.all <- bind_rows(next.day.all, next.day.add)
    
    if (update){
      Subject.Paradigm <- Subject.Paradigm %>%
        add_row(next.day.add) %>%
        rowwise() %>%
        run.create.macro.cell(.) %>%
        group_by(Subject) %>%
        arrange(Paradigm, Paradigm.Day) %>%
        mutate(Day = row_number()) %>% 
        group_by(Subject, Paradigm) %>%
        mutate(Paradigm.Day = row_number(), nParadigm.Day = max(Paradigm.Day)-(Paradigm.Day-1)) %>%
        ungroup()
      update <- FALSE
    }
  }
  #next.day.all <- run.create.macro.cell(next.day.all)
  next.day.all <- next.day.all %>% 
    rowwise() %>% 
    mutate(macroFile = list(cur_data_all()),  macroFile = create.macro.cell2(macroFile))
  
  
  out <- list("next.day" = next.day.all, "Subject.Paradigm2" = Subject.Paradigm )
}

LBN.GenPIT.simple <- function(settings_char, settings_num, Subject.Paradigm){
  #here we progress down the logic tree in order of paradigm progression
  max.day.subjects <- settings_char %>% #get the last day run
    group_by(Subject) %>%
    filter(Day == max(Day))
  
  #max.day.subjects <- max.day.subjects %>% filter(Subject %in% c("8626"))
  
  next.day.all <- tibble() #this will be our primary output, we will bind rows onto this as we iterate
  for (i2 in seq_len(nrow(max.day.subjects))){ #iterate through most recent days per animal
    max.day.sub <-  max.day.subjects[i2,] #grabs i'th subject
    max.day <- max.day.sub$Day #this is the last day (also length of Subject column/table)
    
    Subject.Paradigm.sub <- Subject.Paradigm %>% filter(Subject == max.day.sub$Subject) #this is the the 'planned days' and we are getting the subset for the current subject
    
    settings_char.sub <- settings_char %>% filter(Subject == max.day.sub$Subject) #_char and _num are virtually the same but one has descriptions and one has numbers for inputs/outputs (e.g. 'left port' vs 1)
    settings_num.sub <- settings_num %>% filter(Subject == max.day.sub$Subject) #here we are just getting filtering/subsetting by subject
    max.day.num <- settings_num.sub %>% filter(Day == max.day.sub$Day)
    
    update <- FALSE #if we add days we have to update the 'planned days' object to keep all the days consistent on a per-subject basis
    # current.para.on.list <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day) %>% select(!c(Day, Paradigm.Day, nParadigm.Day, macroFile))
    # next.para.on.list <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day +1) %>% select(!c(Day, Paradigm.Day, nParadigm.Day, macroFile))
    # if (!all.equal(next.para.on.list, current.para.on.list)){
    if (max.day.sub$Paradigm == "GenPIT.simple.CRF"){ #compare two values and choose path (FR1 both -> FR1 S)
      if (max.day.sub$Paradigm.Day > 1){
        vals <- settings_char.sub$N_Rewards[(max.day-1):max.day]
        if (all(vals > 4) | max.day.sub$Paradigm.Day >= 10){
          #add another of the same day
          next.day.add <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day +1)
        } else {next.day.add <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day) %>% 
          mutate(Day = Day + 1, Paradigm.Day = Paradigm.Day +1)
        update <- TRUE}
      } else {next.day.add <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day) %>% 
        mutate(Day = Day + 1, Paradigm.Day = Paradigm.Day +1)
      update <- TRUE}
    } else if (max.day.sub$Paradigm == "GenPIT.simple.RI60pPIT"){
      DayPriorShock2 <- settings_num.sub %>% filter(Paradigm == "GenPIT.simple.StimAlt") %>% .$Day-1
      NPtoBeat <- settings_num.sub %>% filter(Day == DayPriorShock2) %>% 
        mutate(NPtoBeat = case_when(lever.reward1 > 0 & lever.reward2 > 0 ~ N_Lever_1 + N_Lever_2,
                                    lever.reward1 > 0 ~ N_Lever_1, 
                                    lever.reward2 > 0 ~ N_Lever_2,
                                    TRUE ~ NA_real_)) %>%
        .$NPtoBeat
      CurrNum <- max.day.num %>% 
        mutate(NPtoBeat = case_when(lever.reward1 > 0 & lever.reward2 > 0 ~ N_Lever_1 + N_Lever_2,
                                    lever.reward1 > 0 ~ N_Lever_1, 
                                    lever.reward2 > 0 ~ N_Lever_2,
                                    TRUE ~ NA_real_)) %>%
        .$NPtoBeat
        if (CurrNum < NPtoBeat){
          #move to next day - replace prior day lever values with current day lever value 
          next.day.add <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day) %>% 
            mutate(Day = Day + 1, Paradigm.Day = Paradigm.Day +1)
          update <- TRUE
        } else {
          next.day.add <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day +1)
        }
    } else {
      next.day.add <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day +1)
    }
    #} else {next.day.add <- Subject.Paradigm.sub %>% filter(Day == max.day.sub$Day +1)}
    
    next.day.all <- bind_rows(next.day.all, next.day.add)
    
    #could the following move out of the loop?... not as is... could aggregate the days to add and then yes
    #provided we also move the update <- FALSE outside the loop at the top
    if (update){
      Subject.Paradigm <- Subject.Paradigm %>%
        add_row(next.day.add) %>%
        rowwise() %>%
        mutate(macroFile = list(cur_data_all()),  macroFile = create.macro.cell2(macroFile)) %>% 
        group_by(Subject) %>%
        arrange(Paradigm, Paradigm.Day) %>%
        mutate(Day = row_number()) %>% 
        group_by(Subject, Paradigm) %>%
        mutate(Paradigm.Day = row_number(), nParadigm.Day = max(Paradigm.Day)-(Paradigm.Day-1)) %>%
        ungroup()
      update <- FALSE
    }
  }
  #next.day.all <- run.create.macro.cell(next.day.all)
  next.day.all <- next.day.all %>% 
    rowwise() %>% 
    mutate(macroFile = list(cur_data_all()),  macroFile = create.macro.cell2(macroFile))
  
  
  out <- list("next.day" = next.day.all, "Subject.Paradigm2" = Subject.Paradigm )
}

next.day.fun.list <- list("PITTrial" = next.day.PITTrial,
                          "LBN" = next.day.LBN,
                          "LBN.PIT.simple" = LBN.GenPIT.simple
)
# set random box values ----------------------------------------------------------

rand.set.thing <- function(set.thing, vector.size) {  
  rand.set.thing <- sample(set.thing, length(set.thing))
  while (length(rand.set.thing) < vector.size){
    rand.set.thing <- c(rand.set.thing,0)}
  rand.set.thing
}


# Sample no int -----------------------------------------------------------

sample.no.int = function(x, size, replace = F, prob = NULL) {
  if (length(x) == 1) return(x)
  base::sample(x, size = size, replace = replace, prob = prob)
}

# Read MED file PIT function ----------------------------------------------
ReadNameMED_PIT <- function(MedFiles, i0){
  # import("readtext")
  # import("tidyverse")
  # import("zoo")
  # import("purrr")
  MED.R.Conversion <- read_xlsx("RK_PIT_MEDtoR_variableNames.xlsx") %>%
    select(!c(Instance, rename.R)) %>% unique()
  ###### this can be a module to read, convert to double, rename 'out'... only bother if you want to clean up other files
  #read file (start main loop to read all files if desired), can move files to 'read' folder when done
  contents <- readtext(MedFiles[i0])
  
  #put -987.987 in for empty variables or DIMs, can remove later to make parsing file easier
  #-987.987 in first position of a DIM should not signify anything, can empty it later or ignore.
  contents<-stringr::str_replace_all(contents$text,"(?<=[[:upper:]]):(?=\\n[[:upper:]])",": -987.987")
  StartDate <- stringr::str_replace_all(stringr::str_extract(contents, "(?<=\\nStart Date: )\\S+(?=\\n)"),"/", "-")
  StartTime <- stringr::str_extract(contents, "(?<=\\nStart Time: )\\S+(?=\\n)")
  EndTime <- stringr::str_extract(contents, "(?<=\\nEnd Time: )\\S+(?=\\n)")
  
  #find list of variable names with anything in them
  MedVarNames <- stringr::str_extract_all(contents,"(?<=\\n)[[:upper:]](?=:)") #gives Variable Names
  MedVarNames2 <-  MedVarNames[[1]][1:length(MedVarNames[[1]])]
  
  #get variables in string form
  MedVars <- stringr::str_split(contents, "(?<=\\n)[[:upper:]]:") #split by variable name
  MedVars2 <- MedVars[[1]][2:length(MedVars[[1]])] #eliminate header
  
  
  #get into character list of numbers
  out <- MedVars2
  out <- stringr::str_replace_all(out,"\\n","")  #remove newlines
  out <- stringr::str_replace_all(out,"\\s{1,}\\d{1,}:","") #remove line numbers
  out <- stringr::str_replace_all(out,"\\s{1,}",",")
  out <- stringr::str_replace_all(out,"^,","")
  out <- stringr::str_split(out,",")
  
  #next two lines just for testing
  #MedVarNamesOrg <- MedVarNames2
  #outOrg <- outOrg
  
  #cruise list of variables, convert to numbers (double)
  for (i in 1:length(out)){
    out[[i]] <- as.double(out[[i]]) #converts variable to double
    #uses MED.R.Conversion cut from "RK_PIT_MEDtoR_variableNames.xlsx" to change names
    if (MedVarNames2[[i]] %in% MED.R.Conversion$DIM){
      MedVarNames2[[i]] = MED.R.Conversion$R[which(MED.R.Conversion$DIM == MedVarNames2[[i]])]
    }
    
  }
  
  out <- lapply(out, function(x) replace(x, x==-987.987, NA)) #replacees placeholder numbers with NAs
  out <- lapply(out, function(xi) if(all(xi == 0) & (length(xi) > 10)){ #This removes anY totally zero filled arrays
    xi <- as.double(NA)
  } else{
    xi <- xi
  }) #replaces placeholder numbers with NAs
  out <- lapply(out, function(x) x <- x[x != ""]) #remove empty character vectors
  out <- lapply(out, function(x) if (!all(is.na(x))){x <- x[!is.na(x)]}) #removes NAs that aren't alone
  
  # #this is for replacing bulk zeros, looks for 10 or more in a row and truncates variable just prior to first zero in the run of zeros
  # out <- lapply(out, function(x) if (length(x) > 9 & purrr::detect_index(zoo::rollapply(stringr::str_detect(as.character(x), "^0$"), 10, sum), ~ .x == 10) > 0){
  #   x <- x[1:purrr::detect_index(zoo::rollapply(stringr::str_detect(as.character(x), "^0$"), 10, sum), ~ .x == 10)-1]
  # } else {x <- x})
  
  names(out) <- MedVarNames2 #MAKE NAME CHANGES JUST ABOVE, BREAK OUT ANY COUNTERS YOU LIKE
  
  #cleanup output
  out <- out[which(stringr::str_length(names(out)) > 1)] #removes variables without names
  
  
  #extract data from file naming convention
  fileNameData <- stringr::str_split(MedFiles[i0],"/")[[1]] #parse path
  fileNameData <- stringr::str_replace(fileNameData[length(fileNameData)], ".txt", "") #get filename
  fileNameDataSplt <- stringr::str_split(fileNameData,"_") #split parts of filename
  LfileNameDataSplt <- as.list(fileNameDataSplt[[1]]) #make list
  LfileNameDataSplt[[2]] <- stringr::str_extract(LfileNameDataSplt[[2]],"(?<=Subject).*") #remove Subject label from number
  LfileNameDataSplt[[4]] <- as.numeric(LfileNameDataSplt[[4]])
  LfileNameDataSplt[[5]] <- stringr::str_remove(LfileNameDataSplt[5],"Box")
  names(LfileNameDataSplt) <- c("Experiment", "Subject","Paradigm", "Paradigm.Day", "Box") #label list
  LfileNameDataSplt <- LfileNameDataSplt[1:5]
  
  #extract program title from file and add to other non-numeric data
  ProgramName <- stringr::str_extract_all(contents,"(?<=\\nMSN: ).*(?=\\n)") #gives Variable Names
  LfileNameDataSplt <- c(LfileNameDataSplt, ProgramName= ProgramName, Date = StartDate, StartTime = StartTime, EndTime = EndTime) #add filename data to output variable
  
  
  out2 <- list(out = out, meta = LfileNameDataSplt)
}


# Read MED file Active Avoidance function ----------------------------------------------
ReadNameMED_ActiveAvoidance <- function(MedFiles, i0){
  # import("readtext")
  # import("tidyverse")
  # import("zoo")
  # import("purrr")

  # Read the Excel file that contains a conversion table.
  # This table maps MED variable names (in column “DIM”) to their desired R names (in column “R”).
  # We drop the Instance and rename.R columns and keep only unique rows.
  MED.R.Conversion <- read_xlsx("RK_ActiveAvoidance_MEDtoR_variableNames.xlsx") %>%
    select(!c(Instance, rename.R)) %>% unique()
  ###### This section reads the MED file, converts the text into numbers, and renames the output variables.
  # Read the content of the file (using the i0th file in MedFiles) into a list with a $text element.
  contents <- readtext(MedFiles[i0])
  
  #put -987.987 in for empty variables or DIMs, can remove later to make parsing file easier
  #-987.987 in first position of a DIM should not signify anything, can empty it later or ignore.
  contents<-stringr::str_replace_all(contents$text,"(?<=[[:upper:]]):(?=\\n[[:upper:]])",": -987.987")
  
  # Extract the Start Date from the file.
  # The regex looks for text following "\nStart Date: " until the next newline, then replaces "/" with "-".
  StartDate <- stringr::str_replace_all(stringr::str_extract(contents, "(?<=\\nStart Date: )\\S+(?=\\n)"),"/", "-")
 
  # Extract Start Time and End Time in a similar fashion.
  StartTime <- stringr::str_extract(contents, "(?<=\\nStart Time: )\\S+(?=\\n)")
  EndTime <- stringr::str_extract(contents, "(?<=\\nEnd Time: )\\S+(?=\\n)")
  
  # Find a list of variable names (which are single uppercase letters) that are immediately before a colon.
  # The regex uses a positive lookbehind for a newline and a positive lookahead for ":".
  MedVarNames <- stringr::str_extract_all(contents,"(?<=\\n)[[:upper:]](?=:)") #gives Variable Names
  MedVarNames2 <-  MedVarNames[[1]][1:length(MedVarNames[[1]])]
  
  # Split the contents into pieces by using the pattern of a newline followed by an uppercase letter and a colon.
  # This isolates the block of text corresponding to each variable.
  MedVars <- stringr::str_split(contents, "(?<=\\n)[[:upper:]]:") #split by variable name
  MedVars2 <- MedVars[[1]][2:length(MedVars[[1]])] #eliminate header
  
  
  # Prepare to convert the variable blocks into a list of numeric vectors.  
  out <- MedVars2
  out <- stringr::str_replace_all(out,"\\n","")  #remove newlines
  out <- stringr::str_replace_all(out,"\\s{1,}\\d{1,}:","") #remove line numbers
  out <- stringr::str_replace_all(out,"\\s{1,}",",") #remove spaces with a comma
  out <- stringr::str_replace_all(out,"^,","") #remove leading comma
  out <- stringr::str_split(out,",") #split each block into a character vector of values using commas as separators
  
  #next two lines just for testing
  #MedVarNamesOrg <- MedVarNames2
  #outOrg <- outOrg
  
  # Loop over each variable’s character vector and:  
  for (i in 1:length(out)){
    out[[i]] <- as.double(out[[i]]) #converts variable to double
    
    # If the original variable name (from MedVarNames2) is found in the conversion table's DIM column,
    # replace it with the corresponding R name from the conversion table.    
    if (MedVarNames2[[i]] %in% MED.R.Conversion$DIM){
      MedVarNames2[[i]] = MED.R.Conversion$R[which(MED.R.Conversion$DIM == MedVarNames2[[i]])]
    }
  }
  
  out <- lapply(out, function(x) replace(x, x==-987.987, NA)) #replaces placeholder numbers with NAs
  out <- lapply(out, function(xi) if(all(xi == 0) & (length(xi) > 10)){ #This removes any totally zero filled arrays
    xi <- as.double(NA)
  } else{
    xi <- xi
  }) #replaces placeholder numbers with NAs
  
  out <- lapply(out, function(x) x <- x[x != ""]) #remove empty character vectors
  out <- lapply(out, function(x) if (!all(is.na(x))){x <- x[!is.na(x)]}) #removes NAs that aren't alone
  
  # #this is for replacing bulk zeros, looks for 10 or more in a row and truncates variable just prior to first zero in the run of zeros
  # out <- lapply(out, function(x) if (length(x) > 9 & purrr::detect_index(zoo::rollapply(stringr::str_detect(as.character(x), "^0$"), 10, sum), ~ .x == 10) > 0){
  #   x <- x[1:purrr::detect_index(zoo::rollapply(stringr::str_detect(as.character(x), "^0$"), 10, sum), ~ .x == 10)-1]
  # } else {x <- x})
  
  names(out) <- MedVarNames2 #MAKE NAME CHANGES JUST ABOVE, BREAK OUT ANY COUNTERS YOU LIKE
  
  #cleanup output
  out <- out[which(stringr::str_length(names(out)) > 1)] #removes variables without names
  
  
  #extract data from file naming convention
  fileNameData <- stringr::str_split(MedFiles[i0],"/|\\\\")[[1]] #parse path
  fileNameData <- stringr::str_replace(fileNameData[length(fileNameData)], ".txt", "") #get filename
  fileNameDataSplt <- stringr::str_split(fileNameData,"_") #split parts of filename
  LfileNameDataSplt <- as.list(fileNameDataSplt[[1]]) #make list
  LfileNameDataSplt[[2]] <- stringr::str_extract(LfileNameDataSplt[[2]],"(?<=Subject).*") #remove Subject label from number
  LfileNameDataSplt[[4]] <- as.numeric(LfileNameDataSplt[[4]])
  LfileNameDataSplt[[5]] <- stringr::str_remove(LfileNameDataSplt[5],"Box")
  names(LfileNameDataSplt) <- c("Experiment", "Subject","Paradigm", "Paradigm.Day", "Box") #label list
  LfileNameDataSplt <- LfileNameDataSplt[1:5]
  
  #extract program title from file and add to other non-numeric data
  ProgramName <- stringr::str_extract_all(contents,"(?<=\\nMSN: ).*(?=\\n)") #gives Variable Names
  LfileNameDataSplt <- c(LfileNameDataSplt, ProgramName= ProgramName, Date = StartDate, StartTime = StartTime, EndTime = EndTime) #add filename data to output variable
  
  
  out2 <- list(out = out, meta = LfileNameDataSplt)
}

# Active Avoidance Array Detangler ------------------------------------------

MED.array.detangle <- function(arr){
  # Define the number of columns (dimensions) in the array.
  # In this case, each trial’s data is expected to be arranged in 15 columns.
  dim_row_number <- 15
  
  # Convert the flat (linear) array into a tibble with three columns:
  # 'row' – which trial (row) the value belongs to,
  # 'col' – the position within that trial (column),
  # 'value' – the actual numeric value.
  data_tibble <- tibble(
    row = rep(1:ceiling(length(arr) / dim_row_number), each = dim_row_number)[1:length(arr)],
    col = rep(1:dim_row_number, length.out = length(arr)),
    value = arr
  )
  
  # Reshape the tibble from long format to wide format.
  # Each unique column number becomes a separate column in the resulting tibble,
  # with column names prefixed with "V" (e.g., V1, V2, …, V15).
  reshaped_tibble <- data_tibble %>%
    pivot_wider(names_from = col, values_from = value, names_prefix = "V") %>% 
    select(!row)
  
  # Rename the columns to meaningful variable names.
  # The renaming assumes exactly 15 columns are present.
  names(reshaped_tibble) <- c("Trial", "avoid.tag", "avoid.latency","escape.tag", "escape.latency", 
                              "left.move.activity", "right.move.activity", "crossings.count", "ITI.shocks", "Avoid", 
                              "Escape", "Shock", "Cue", "Cue.End", "unused")
  
  reshaped_tibble <- reshaped_tibble %>% 
    select(!c(avoid.tag, escape.tag, left.move.activity, right.move.activity, ITI.shocks, unused)) %>% #remove unused columns
    pivot_longer(!Trial, names_to = "name", values_to = "value") %>% #convert to long format
    filter((value > 0 | name == "Cue")) %>% group_by(name) %>% #remove zero values, keep Cue, group by name
    arrange(name, Trial) %>%  #arrange in order of Trial
    mutate(Instance = row_number()) %>% #add Instance column 
    arrange(Trial) #arrange in order of Trial

}


# Read MED file Shock.NoEscape function ----------------------------------------------
ReadNameMED_Shock.NoEscape <- function(MedFiles, i0){
  # import("readtext")
  # import("tidyverse")
  # import("zoo")
  # import("purrr")
  
  # Read the Excel file that contains a conversion table.
  # This table maps MED variable names (in column “DIM”) to their desired R names (in column “R”).
  # We drop the Instance and rename.R columns and keep only unique rows.
  MED.R.Conversion <- read_xlsx("RK_Shock.NoEscape_MEDtoR_variableNames.xlsx") %>%
    select(!c(Instance, rename.R)) %>% unique()
  ###### This section reads the MED file, converts the text into numbers, and renames the output variables.
  # Read the content of the file (using the i0th file in MedFiles) into a list with a $text element.
  contents <- readtext(MedFiles[i0])
  
  #put -987.987 in for empty variables or DIMs, can remove later to make parsing file easier
  #-987.987 in first position of a DIM should not signify anything, can empty it later or ignore.
  contents<-stringr::str_replace_all(contents$text,"(?<=[[:upper:]]):(?=\\n[[:upper:]])",": -987.987")
  
  # Extract the Start Date from the file.
  # The regex looks for text following "\nStart Date: " until the next newline, then replaces "/" with "-".
  StartDate <- stringr::str_replace_all(stringr::str_extract(contents, "(?<=\\nStart Date: )\\S+(?=\\n)"),"/", "-")
  
  # Extract Start Time and End Time in a similar fashion.
  StartTime <- stringr::str_extract(contents, "(?<=\\nStart Time: )\\S+(?=\\n)")
  EndTime <- stringr::str_extract(contents, "(?<=\\nEnd Time: )\\S+(?=\\n)")
  
  # Find a list of variable names (which are single uppercase letters) that are immediately before a colon.
  # The regex uses a positive lookbehind for a newline and a positive lookahead for ":".
  MedVarNames <- stringr::str_extract_all(contents,"(?<=\\n)[[:upper:]](?=:)") #gives Variable Names
  MedVarNames2 <-  MedVarNames[[1]][1:length(MedVarNames[[1]])]
  
  # Split the contents into pieces by using the pattern of a newline followed by an uppercase letter and a colon.
  # This isolates the block of text corresponding to each variable.
  MedVars <- stringr::str_split(contents, "(?<=\\n)[[:upper:]]:") #split by variable name
  MedVars2 <- MedVars[[1]][2:length(MedVars[[1]])] #eliminate header
  
  
  # Prepare to convert the variable blocks into a list of numeric vectors.  
  out <- MedVars2
  out <- stringr::str_replace_all(out,"\\n","")  #remove newlines
  out <- stringr::str_replace_all(out,"\\s{1,}\\d{1,}:","") #remove line numbers
  out <- stringr::str_replace_all(out,"\\s{1,}",",") #remove spaces with a comma
  out <- stringr::str_replace_all(out,"^,","") #remove leading comma
  out <- stringr::str_split(out,",") #split each block into a character vector of values using commas as separators
  
  #next two lines just for testing
  #MedVarNamesOrg <- MedVarNames2
  #outOrg <- outOrg
  
  # Loop over each variable’s character vector and:  
  for (i in 1:length(out)){
    out[[i]] <- as.double(out[[i]]) #converts variable to double
    
    # If the original variable name (from MedVarNames2) is found in the conversion table's DIM column,
    # replace it with the corresponding R name from the conversion table.    
    if (MedVarNames2[[i]] %in% MED.R.Conversion$DIM){
      MedVarNames2[[i]] = MED.R.Conversion$R[which(MED.R.Conversion$DIM == MedVarNames2[[i]])]
    }
  }
  
  out <- lapply(out, function(x) replace(x, x==-987.987, NA)) #replaces placeholder numbers with NAs
  out <- lapply(out, function(xi) if(all(xi == 0) & (length(xi) > 10)){ #This removes any totally zero filled arrays
    xi <- as.double(NA)
  } else{
    xi <- xi
  }) #replaces placeholder numbers with NAs
  
  out <- lapply(out, function(x) x <- x[x != ""]) #remove empty character vectors
  out <- lapply(out, function(x) if (!all(is.na(x))){x <- x[!is.na(x)]}) #removes NAs that aren't alone
  
  # #this is for replacing bulk zeros, looks for 10 or more in a row and truncates variable just prior to first zero in the run of zeros
  # out <- lapply(out, function(x) if (length(x) > 9 & purrr::detect_index(zoo::rollapply(stringr::str_detect(as.character(x), "^0$"), 10, sum), ~ .x == 10) > 0){
  #   x <- x[1:purrr::detect_index(zoo::rollapply(stringr::str_detect(as.character(x), "^0$"), 10, sum), ~ .x == 10)-1]
  # } else {x <- x})
  
  names(out) <- MedVarNames2 #MAKE NAME CHANGES JUST ABOVE, BREAK OUT ANY COUNTERS YOU LIKE
  
  #cleanup output
  out <- out[which(stringr::str_length(names(out)) > 1)] #removes variables without names
  
  
  #extract data from file naming convention
  fileNameData <- stringr::str_split(MedFiles[i0],"/|\\\\")[[1]] #parse path
  fileNameData <- stringr::str_replace(fileNameData[length(fileNameData)], ".txt", "") #get filename
  fileNameDataSplt <- stringr::str_split(fileNameData,"_") #split parts of filename
  LfileNameDataSplt <- as.list(fileNameDataSplt[[1]]) #make list
  LfileNameDataSplt[[2]] <- stringr::str_extract(LfileNameDataSplt[[2]],"(?<=Subject).*") #remove Subject label from number
  LfileNameDataSplt[[4]] <- as.numeric(LfileNameDataSplt[[4]])
  LfileNameDataSplt[[5]] <- stringr::str_remove(LfileNameDataSplt[5],"Box")
  names(LfileNameDataSplt) <- c("Experiment", "Subject","Paradigm", "Paradigm.Day", "Box") #label list
  LfileNameDataSplt <- LfileNameDataSplt[1:5]
  
  #extract program title from file and add to other non-numeric data
  ProgramName <- stringr::str_extract_all(contents,"(?<=\\nMSN: ).*(?=\\n)") #gives Variable Names
  LfileNameDataSplt <- c(LfileNameDataSplt, ProgramName= ProgramName, Date = StartDate, StartTime = StartTime, EndTime = EndTime) #add filename data to output variable
  
  
  out2 <- list(out = out, meta = LfileNameDataSplt)
}

# Inescp Shock Array Detangler ------------------------------------------
MED.array.detangle.inesc <- function(arr){
  # Define the number of columns (dimensions) in the array.
  # In this case, each trial’s data is expected to be arranged in 15 columns.
  dim_row_number <- 15
  
  # Convert the flat (linear) array into a tibble with three columns:
  # 'row' – which trial (row) the value belongs to,
  # 'col' – the position within that trial (column),
  # 'value' – the actual numeric value.
  data_tibble <- tibble(
    row = rep(1:ceiling(length(arr) / dim_row_number), each = dim_row_number)[1:length(arr)],
    col = rep(1:dim_row_number, length.out = length(arr)),
    value = arr
  )
  
  # Reshape the tibble from long format to wide format.
  # Each unique column number becomes a separate column in the resulting tibble,
  # with column names prefixed with "V" (e.g., V1, V2, …, V15).
  reshaped_tibble <- data_tibble %>%
    pivot_wider(names_from = col, values_from = value, names_prefix = "V") %>% 
    select(!row)
  
  # Rename the columns to meaningful variable names.
  # The renaming assumes exactly 15 columns are present.
  names(reshaped_tibble) <- c("Trial", "avoid.tag", "avoid.latency","escape.tag", "escape.latency", 
                              "left.move.activity", "right.move.activity", "crossings.count", "ITI.shocks", "Avoid", 
                              "Escape", "Shock", "Cue", "Cue.End", "unused")
  
  reshaped_tibble <- reshaped_tibble %>% 
    select(!c(avoid.tag, escape.tag, left.move.activity, right.move.activity, ITI.shocks, unused)) %>% #remove unused columns
    pivot_longer(!Trial, names_to = "name", values_to = "value") %>% #convert to long format
    filter((value > 0 | name == "Cue")) %>% group_by(name) %>% #remove zero values, keep Cue, group by name
    arrange(name, Trial) %>%  #arrange in order of Trial
    mutate(Instance = row_number()) %>% #add Instance column 
    arrange(Trial) #arrange in order of Trial
  
}

# Rename File -------------------------------------------------------------
my.file.rename <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.rename(from = from,  to = to)
}





# Create Paradigm ---------------------------------------------------------
create.Paradigm <- function(Paradigm = "Default",
                            Program = "RK_PIT_PavOneArray_LeverConv", 
                            Pav.Inst.PIT = NA_real_, #takes values of  0, 1, 2 for each option
                            MaxTotalTime = NA_real_, #minutes
                            max.rewards = NA_real_, 
                            
                            min.ITI = NA_real_, #time before random ITI starts in minutes
                            max.ITI = NA_real_, #max time (s) of random interval following min.ITI
                            time.CS = NA_real_, #time (s) stimulus plays for (sets RI with rew.per.CS)
                            rew.per.CS = NA_real_, #sets random interval to dispense certain number of rewards on average over a time period (sets the RI)
                            RandListOrNot = NA_real_, #are cues picked randomly or sequentially
                            
                            Cue.RIorFixed = NA_real_, #0,1
                            Max.Rew.per.Cue = NA_real_, #if not set MED defaults to 3x prescribed average - can set really high
                            max.CSs = NA_real_, #end condition for number or stimuli given
                            time.pulse.off = NA_real_, #time (s) pulse is off for when pulsed
                            time.pulse.on = NA_real_, #time (s) pulse is off for when pulsed
                            time.pump.on = NA_real_, #time (s) pump is on for (determines volume)
                            
                            LeverPort1 = NA_real_,  #7 = right lever, 3 = right port
                            LeverPort2 = NA_real_, #8 = left lever, 1 = left port
                            time.Fixed.Cue.Reward.Delay = NA_real_,
                            Lever.RI.Schedule = NA_real_, #RI schedule in seconds
                            extinction = NA_real_, #extinction 0 for no, 1 for yes 
                            time.extinction = NA_real_, #time in minutes for extinction (if 'extinction' is 1)
                            time.Lever.Opto.on = NA_real_, #time in seconds opto is on for lever presses that are stimulated
                            Opto.for.UnRew.Rew.Both.None = NA_real_, #which type of response gets opto
                            
                            shock.trigger = NA_real_, #0 = no shock, 1 = Reward... could add more
                            shock.ratio = NA_real_,
                            shock.duration = NA_real_,
                            shock.initial.free.rew = NA_real_,
                            max.shock.inactivity = NA_real_,
                            
                            Omi.Off.On = NA_real_, #0,1
                            time.Omi = NA_real_,
                            time.Opto.ON = NA_real_,
                            Omi.Reward = NA_real_,
                            
                            cue1 = NA_real_, #sets cue relationship to cue - randomized through set.avial.stim
                            cue2 = NA_real_,
                            cue3 = NA_real_,
                            reward1 = NA_real_, #sets reward relationship to cue - randomized through set.avail.outcome
                            reward2 = NA_real_,
                            reward3 = NA_real_,
                            lever.avail1 = NA_real_, #right lever availability ()
                            lever.avail2 = NA_real_,
                            lever.reward1 = NA_real_, #right lever reward ID
                            lever.reward2 = NA_real_,
                            opto.stim1 = NA_real_, #opto related to cue
                            opto.stim2 = NA_real_,
                            opto.stim3 = NA_real_,
                            opto.op1 = NA_real_,
                            opto.op2 = NA_real_,
                            opto.op3 = NA_real_,
                            List.Cue.Pick1 = NA_real_,
                            List.Cue.Pick2 = NA_real_,
                            List.Cue.Pick3 = NA_real_,
                            List.Cue.Pick4 = NA_real_,
                            List.Cue.Pick5 = NA_real_,
                            List.Cue.Pick6 = NA_real_,
                            List.Cue.Pick7 = NA_real_,
                            List.Cue.Pick8 = NA_real_, 
                            opto.rew1 = NA_real_,
                            opto.rew2 = NA_real_,
                            opto.rew3 = NA_real_,
                            
                            path.save = "R:\\Basic_Sciences\\Phys\\Lerner_Lab_tnl2633\\Ryan\\MedPC\\Ryan PIT MEDPC files\\",
                            cues.pulsed = c(12), 
                            cues.pulsed.not = c(0, 3, 10)){
  library(tidyverse)
  
  if (!file_test("-f", "paradigm.library.Rds")) {
    paradigm.library <- list()
    saveRDS(paradigm.library, file = "paradigm.library.Rds")
  } else {
    paradigm.library <- readRDS(file = "paradigm.library.Rds")
  }
  
  paradigm.library[Paradigm] <- list(tibble(
    Paradigm,
    Program ,
    Pav.Inst.PIT,
    MaxTotalTime,
    max.rewards,
    
    min.ITI,
    max.ITI,
    time.CS,
    rew.per.CS,
    RandListOrNot,
    
    Cue.RIorFixed,
    Max.Rew.per.Cue,
    max.CSs,
    time.pulse.off,
    time.pulse.on,
    time.pump.on,
    
    LeverPort1,
    LeverPort2,
    time.Fixed.Cue.Reward.Delay,
    Lever.RI.Schedule,
    extinction,
    time.extinction,
    time.Lever.Opto.on,
    Opto.for.UnRew.Rew.Both.None,
    
    shock.trigger,
    shock.ratio,
    shock.duration,
    shock.initial.free.rew,
    max.shock.inactivity,
    
    Omi.Off.On,
    time.Omi,
    time.Opto.ON,
    Omi.Reward,
    
    cue1,
    cue2,
    cue3,
    reward1,
    reward2,
    reward3,
    lever.avail1,
    lever.avail2,
    lever.reward1,
    lever.reward2,
    opto.stim1,
    opto.stim2,
    opto.stim3,
    opto.op1,
    opto.op2,
    opto.op3,
    List.Cue.Pick1,
    List.Cue.Pick2,
    List.Cue.Pick3,
    List.Cue.Pick4,
    List.Cue.Pick5,
    List.Cue.Pick6,
    List.Cue.Pick7,
    List.Cue.Pick8,
    opto.rew1,
    opto.rew2,
    opto.rew3,
    path.save
  ) %>% mutate(across(everything(), as.list))  #this fixes my merge problem but causes the create.macro.cell function below to fail
  )
  #paradigm.library <- bind_rows(paradigm.library, paradigm.library.add)
  saveRDS(paradigm.library, file = "paradigm.library.Rds")
  paradigm.library
}

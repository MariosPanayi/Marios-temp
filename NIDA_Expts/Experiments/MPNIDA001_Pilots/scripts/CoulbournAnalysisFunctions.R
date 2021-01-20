# Coulbourn Data processing and Analysis FUnctions -------------------------------------------------------------------------



# Necessary Libraries -----------------------------------------------------
# Note that some of these are repeated within certain functions to allow for parallel FOR loops
library(tidyverse)
library(knitr)
library(data.table)
# Package for relative file paths
library(here)
# -------------------------------------------------------------------------


Coulbourn_extractIndividualSubjectFiles <- function (projectdatafolder, listofdatafolders){
  
  # Function searches for all .txt raw COulbourn data files within the data folders provided
  # This function 
  
  
  datafilepaths <-0
  for (i in c(1:length(listofdatafolders))){
    lookup <- paste(c(projectdatafolder, listofdatafolders[i]), collapse = "/")
    
    datafilepaths <- list.files(path = lookup, pattern = ".txt")
    foreach (j = c(1:length(datafilepaths))) %dopar% {
      library(here)
      library(tidyverse)
      library(knitr)
      source(here("scripts", "CoulbournAnalysisFunctions.R"))
      # For each raw.txt file split up the data into individual subjects .csv files for subsequent analysis
      folderpath <- here(paste(c(projectdatafolder, listofdatafolders[i]), collapse = "/"))
      filename <- datafilepaths[j]
      ## Run Function
      coulbourn_rawdatasplit(filename,folderpath) 
    }
    
  }
  
}



# Function: Data splitter -------------------------------------------------

# # Example Inputs:
#
# filename <- "CRF_MagTrain_Sess1.txt"
# folderpath <- here("rawdata", "Marios",'2_ConditionedReinforcement', 'MagTrain_Day1')

coulbourn_rawdatasplit <- function(filename,folderpath) {
  
  filepath <- here(folderpath, filename)
  
  # Coulbourn files are tab separated value .txt files
  # rawdata <- read_tsv(filepath)
  # Faster function for reading in files
  rawdata <- fread(filepath)
  
  # Clean up variable names -------------------------------------------------
  
  # Clean up column naming conventions - Assumes 4 response options on Coulbourn setup
  names(rawdata)[12] <- "A1_On"
  names(rawdata)[13] <- "A2_On"
  names(rawdata)[14] <- "A3_On"
  names(rawdata)[15] <- "A4_On"
  names(rawdata)[16] <- "A1_Off"
  names(rawdata)[17] <- "A2_Off"
  names(rawdata)[18] <- "A3_Off"
  names(rawdata)[19] <- "A4_Off"
  
  
  # Separate subjects and save individual CSV files -------------------------------------------------------
  # Coulbourn will put all the data you have asked for in a long format stack with subjects changing after a variable number of columns
  ## Note that if you try and extract by identifying unique subjects you might accidentally ignore that a subject was run multiple times
  ## Instead, Coulbourn timetsamps always start at t = 0, so this can be used to identify the indices of every new subject
  
  # Start and end indices of each subject
  start = which(rawdata$Time == 0)
  end = start -1
  end = end[-1]
  end = c(end, nrow(rawdata))
  
  # Grab filename for naming convention and combine with subject number
  prefix <- strsplit(filename, ".txt")
  prefix <- prefix[[1]]
  prefix <- str_replace_all(prefix, "_", "")
  
  # for each subject 
  for (i in c(1:length(start))) {
    temp <- rawdata[start[i]:end[i], ]
    
    containingfoldername <- strsplit(folderpath, '/')
    containingfoldername <- containingfoldername[[1]][length(containingfoldername[[1]])]
    # containingfoldername <- str_replace_all(containingfoldername, "_", "")
    
    ## Add a new column with the containing folder name
    ## Add a new column with the containing filename
    temp$folder <- replicate(nrow(temp), containingfoldername)
      temp$file <- replicate(nrow(temp), prefix)
    
    # save temp data with appropriate filename
    # Clean SubjNumber
    SubjectNum = paste(c("Rat", temp$Subject[1]), collapse = "")
    SubjectNum = str_replace_all(SubjectNum, "_", "")
  
    
    # Clean RUn number
    tempfilename <- paste(c(prefix, SubjectNum), collapse = "_")
    tempfilename <- paste(tempfilename, ".csv", sep = "")
    
    filepath <- here(folderpath, tempfilename)
    
    # fwrite(temp, filepath)
    #  Faster file writing function
    fwrite(temp, filepath)
    
  }
  
  
  
  # end function
}



Coulbourn_Joinprocesseddata <- function(projectdatafolder, listofdatafolders, processedfoldername) {
  filestojoin <- "0"
  for (i in c(1:length(listofdatafolders))){
    lookup <- paste(c(projectdatafolder, listofdatafolders[i],processedfoldername), collapse = "/")
    
    datafilepaths <- list.files(path = lookup, pattern = ".csv", full.names = TRUE)
    filestojoin <- c(filestojoin, datafilepaths)
    
    #   # For each processed .csv file, load the data and then join it together
    #   filename <- datafilepaths[j]
    #   ## Load and analyse
  }
  #delete initialising variable
  filestojoin <- filestojoin[-1]
  
  ## Load each table of data and join into a single 
  for (i in c(1:length(filestojoin))){ 
    if (i == 1){
      rawdata <- fread(filestojoin[i])
    } else {
      tempdata <- fread(filestojoin[i])
      rawdata <- rbindlist(list(rawdata,tempdata))
    }
  }
  return(rawdata)
}



coulbourn_actioncleantimestamps <- function(A_on, A_off) {
  # Function takes a vector of Action ON times and a vector of Action OFF times from a session
  # Data are processed to fix any issues with missing/non-matching ON/OFF times
  # Assumptions made are that:
  # 1) If an action OFF signal is the first timestamp, then the first action ON was before the session started (so first A_On set to time = 0)
  # 2) Any ON or OFF signal that is doesn't pair chronologically with the other signals reflects an action ON/OFF signal occuring faster than the temporal resolution of the system
  #   A corresponding On/Off signal is added so that the ON-OFF duration is 0 (i.e. they occurred at the same time)
  # These issues happen surprisingly often for Coulbourn Lever Pressing data in particular
  # N.B. output is a data frame of A_on and A_off, split this output apart for use with other functions.
  
  
  if (length(A_on) > 0 | length(A_off) > 0) {
    
    # Make sure data are in chronological order!!! Otherwise loop will go on forever
    A_on <- sort(A_on)
    A_off <- sort(A_off)
    
    
    if (is_empty(A_on) & !is_empty(A_off)) {
      # Check to see if there are no ON signals
      # Situation - special case where event initiated prior to recording start
      A_on <- c(0,A_on)
    }
    if (!is_empty(A_on) & is_empty(A_off)) {
      # Check to see if there are no OFF signals
      # Situation - special case where OFF signal occurred within temporal resolution of system for first response
      A_off <- c(A_on[1], A_off)
    }
    if (A_on[1] > A_off[1]) {
      # Check to see if the first event is an Off. Assumption is that the on signal happened before recording started.
      # Artificially set the action initiation as time = 0 (i.e. session start)
      A_on <- c(0,A_on)
    }
    
    
    
    
    # Combine all on and off signals and order them by time AND [A_on before A_off] when there is a tie 
    # [we will deal with issues this might cause, but it will be correct most of the time]
    time = c(A_on, A_off)
    event = c(rep("A_on", length(A_on)), rep("A_off", length(A_off)))
    # Event ID is just a numeric version of even -1 = On, 1 = Off
    eventID = c(rep(-1, length(A_on)), rep(1, length(A_off)))
    
    #  Order data
    A = data.frame(time, event, eventID)
    A = setorder(A, time, eventID)
    # Add a numeric order to the list so we can manipulate values later
    A$eventorder = c(1:nrow(A))
    
    
    # Check for sequential times that are tied for more than 1 event
    # N.B. if ties[i] = 0, then that event[i] and event[i+1] happened at the same time
    ties = diff(A$time)
    ties = which(ties == 0)
    
    # If there are ties to consider
    if (length(ties) != 0) {
      
      # Test to ignore first tie if it is also the first event
      if(ties[1] == 1) {j = 2} else {j = 1}
      
      for(i in c(j:length(ties))){
        
        idx = ties[i]
        
        if((A$event[idx-1] == "A_on" & A$event[idx] == "A_on" &  A$event[idx+1] == "A_off")) {
          
          # If the tied A_on and A_off signals are in the wrong order, switch them around
          # Note that this is the only way they can be ordered incorrectly since:
          # 1) We have already ordered the data so any times that are tied will put an A_on before an A_off
          # 2) Coulbourn never records more than 1x A_on and 1x A_off with the same time
          # 3) If the previous A_on had a simultaneous A_off then it would also be recorded as such
          
          A$eventorder[idx] = idx+1
          A$eventorder[idx+1] = idx
          
          # Important to re-order the data after each change in case multiple ties are adjacent to each other
          A = setorder(A, eventorder)
          
        }
      }
      
    }
    
    # clear unused temporary variables for memory efficiency
    rm(ties,event,time, A_on, A_off)
    
    # a value of 0 indicates that this index and the one above it are the same [i.e. two A_on or two A_off in a row] 
    repeats <- diff(A$eventID)
    # Indices in A of first repeat value
    repeatsidx <- which(repeats == 0)
    # Identity of the repeated event name
    repeats_ID <- A$event[repeatsidx]
    # Time of the first repeated event
    extra_time <- A$time[repeatsidx]
    # New position to insert extra event between repeats
    extra_eventorder <- A$eventorder[repeatsidx] + 0.5
    
    # Create datatable and recode the new event identities 
    A_extra <- data.table(repeatsidx, repeats_ID, extra_time, extra_event = repeats_ID, extra_eventorder)
    recodeData <- data.table(
      oldvalues = c("A_on", "A_off"),
      newvalues = c("A_off", "A_on")
    )
    # Recode data to reflect the correct identity of the extra events to insert
    A_extra[recodeData, extra_event := newvalues, on =. (extra_event =  oldvalues)]
    # Drop unnecessary variables
    A_extra <- A_extra[, c("extra_time","extra_event", "extra_eventorder")]
    # Relabel columns to merge with A data
    colnames(A_extra) <- c("time","event", "eventorder")
    
    # Join with original data
    A <- A[, c("time","event", "eventorder")]
    A <- rbindlist(list(A, A_extra), use.names = TRUE)
    # Re organise the data in correct temporal order
    A <- setorder(A, eventorder)
    
    # One last check to make sure final event is an OFF signal
    if (A$event[nrow(A)] == "A_on") {
      # Create extra data and append to A
      A_final <- data.frame(time = A$time[nrow(A)], 
                            event = "A_off", 
                            eventorder = (A$eventorder[nrow(A)] + 0.5))
      A <- rbindlist(list(A, A_final), use.names = TRUE)
      A <- setorder(A, eventorder)
      
    }
    
    ### Extract data and return function output
    A_on = A$time[A$event == "A_on"]
    A_off = A$time[A$event == "A_off"]
    A_data <- data.frame(A_on, A_off)
    return(A_data)
    
    
    # End Data re-ordering   
  } else {
    A_data <- data.frame(A_on, A_off)
    return(A_data)
  }
  
}


coulbourn_actionbin <- function(A_on, A_off, bin_start, bin_end){
  
  if (length(A_on) > 0 | length(A_off) > 0) {
    
    
    # CLean timestamp data
    A_data <- coulbourn_actioncleantimestamps(A_on, A_off)
    # Separate data frame output and replace original (A_on, A_off) variables
    A_on <- A_data$A_on
    A_off <- A_data$A_off
    
    
    
    #### Calculate frequency and duration of actions ####
    # Initialise variable
    bin_A_freq <- replicate(length(bin_start), 0)
    bin_A_dur <- replicate(length(bin_start), 0)
    for (i in c(1:length(bin_start))){
      bin_A_freq[i] = sum(A_on >= bin_start[i] & A_on < bin_end[i])
      
      for (j in c(1:length(A_on))){
        # action starts before timebin and ends within timebin
        if ( A_on[j] < bin_start & A_off[j] >= bin_start[i] & A_off[j] < bin_end[i]) {
          bin_A_dur[i] <- bin_A_dur[i] + (A_off[j] - bin_start[i])
          # action after timebin starts and before timebin ends
        } else if ( A_on[j] >= bin_start[i] & A_off[j] < bin_end[i] ) {
          bin_A_dur[i] <- bin_A_dur[i] + (A_off[j] - A_on[j])
          # action starts within the timebin , and ends after timebin
        } else if ( A_on[j] >= bin_start[i] & A_on[j] < bin_end[i] & A_off[j] >= bin_end[i] ) {
          bin_A_dur[i] <- bin_A_dur[i] + bin_end[i] - A_on[j]
          # action before bin starts and continues until after bin
        } else if ( A_on[j] <= bin_start[i] & A_off[j] > bin_end[i] ) {
          bin_A_dur[i] <- bin_A_dur[i] + (bin_end[i] - bin_start[i])
        } else {bin_A_dur[i] <- bin_A_dur[i]}
      }
    }
    
    
    
    bin_A_data <- cbind(bin_A_freq,bin_A_dur)
    
    return(bin_A_data)
    
  } else {
    bin_A_freq <- replicate(length(bin_start), 0)
    bin_A_dur <- replicate(length(bin_start), 0)
    bin_A_data <- cbind(bin_A_freq,bin_A_dur)
    
    return(bin_A_data)
  }
  
  # End of function
}


#
# # Example Inputs:

# Split Data by Trial into discrete time bins --

# Load Individual File --
# 
# ## Set file path
# filename <- "CRFMagTrainSess1_Rat9_MagTrainDay1.csv"
# folderpath <- here("rawdata", "Marios",'2_ConditionedReinforcement', 'MagTrain_Day1')
# 
# 
# # Set parameters --
# 
# ## List of states
# S = c("ITI" = 1,
#       "Pre" = 2,
#       "Pel" = 3)
# 
# ## Time base the linc was set to, in ms
# timebase = 20
# 
# ## Time bins to analyze each state in, in s
# timebinwidth = 1
# 
# ## States to bin and which not to bin
# nobin = S["ITI"]
# bin = c(S["Pre"], S["Pel"])
# 
# ## Identify state that trials start in, usually the ITI ;)
# trialstartstate = S["ITI"]
# 

coulbourn_processdata_Pavlovian_timebin <- function(filename,folderpath,S,timebase, timebinwidth, nobin, bin, trialstartstate) {
  
  filepath <- here(folderpath, filename)
  
  ## Read in data
  rawdata <- fread(filepath)
  ## Key to convert actions
  
  # key_actions <- c("LLR_On" = "A1_On",
  #   "RLR_On" = "A2_On",
  #   "Mag_On" = "A3_On",
  #   "A4_On" = "A4_On",
  #   "LLR_Off" = "A1_Off",
  #   "RLR_Off" = "A2_Off",
  #   "Mag_Off" = "A3_Off",
  #   "A4_Off" = "A4_Off")
  
  
  # Change time units to seconds --------------------------------------------
  
  rawdata <- rawdata %>% 
    mutate(Time = Time*timebase/1000)
  
  # Calculate Trials --------------------------------------------------------
  
  
  ## Add a column to the data set indicating trial number
  
  ### Identify the start time of each trial, and the final end state (indicated by -1 in Coulbourn)
  trialTimes <- c(which(rawdata$`Transition State` == trialstartstate), which(rawdata$`Transition State` == -1))
  totalTrials <- length(trialTimes) - 1
  
  
  ## Initialise trialnum variable with 0s for every row
  trialNum <- replicate(nrow(rawdata),0)
  
  for (i in c(1:totalTrials)) {
    trialIdx = c(trialTimes[i]:(trialTimes[i+1]-1))
    trialNum[trialIdx] = i
    
  }
  
  ## append trial counter to dataframe
  rawdata <- cbind(rawdata,trialNum)
  
  
  # Create time-binned data - Frequency --------------------------------------
  ## State change info
  statechange_idx = which(rawdata$`Transition State` !=0)
  statechange_ID = rawdata$`Transition State`[statechange_idx]
  statechange_Time = rawdata$Time[statechange_idx]
  statechange_TrialNum = rawdata$trialNum[statechange_idx]
  ## Target bin state info
  binstates_idx =  which(!is.na(match(statechange_ID, bin)))
  binstate_ID = statechange_ID[binstates_idx]
  binstart_Time = statechange_Time[binstates_idx]
  binend_Time = statechange_Time[binstates_idx+1]
  binTrialNum = statechange_TrialNum[binstates_idx]
  
  # Calculate a new variable with interval units for each state (e.g. 1s bins)
  bin_time <- 0
  bin_state <- 0
  bin_trial <- 0
  bin_timewithin <- 0
  for (i in  c(1:length(binstart_Time))) {
    
    # Timebin intervals
    Temp <- seq(from = binstart_Time[i], to = binend_Time[i] - timebinwidth, by = timebinwidth)
    bin_time <- c(bin_time, Temp)
    # State IDS
    Temp <- replicate(length(Temp),binstate_ID[i])
    bin_state <- c(bin_state, Temp)
    # Trial Number
    Temp <- replicate(length(Temp),binTrialNum[i])
    bin_trial <- c(bin_trial, Temp)
    # Time within the State
    Temp <- seq(from = timebinwidth, to = length(Temp), by = timebinwidth)
    bin_timewithin <- c(bin_timewithin, Temp)
    
  }
  # Clear initialised value from position 1 of vars
  bin_time <- bin_time[-1] 
  bin_state <- bin_state[-1] 
  bin_trial <- bin_trial[-1] 
  bin_timewithin <- bin_timewithin[-1]
  ## Create new variable with bin_endtime
  bin_endtime <- bin_time + timebinwidth
  
  
  # Count Frequency of events in bins ---------------------------------------
  # Extract data
  ## repeat for each of the 4 possible actions
  A1_on <- rawdata$Time[rawdata$A1_On == 1]
  A1_off <- rawdata$Time[rawdata$A1_Off == 1]
  A1_bin <- coulbourn_actionbin(A1_on, A1_off, bin_time, bin_endtime)
  
  A2_on <- rawdata$Time[rawdata$A2_On == 1]
  A2_off <- rawdata$Time[rawdata$A2_Off == 1]
  A2_bin <- coulbourn_actionbin(A2_on, A2_off, bin_time, bin_endtime)
  
  A3_on <- rawdata$Time[rawdata$A3_On == 1]
  A3_off <- rawdata$Time[rawdata$A3_Off == 1]
  A3_bin <- coulbourn_actionbin(A3_on, A3_off, bin_time, bin_endtime)
  
  A4_on <- rawdata$Time[rawdata$A4_On == 1]
  A4_off <- rawdata$Time[rawdata$A4_Off == 1]
  A4_bin <- coulbourn_actionbin(A4_on, A4_off, bin_time, bin_endtime)

  
  ## Combine into a dataframe and rename variables appropriately
  data_bin <- cbind(A1_bin,A2_bin,A3_bin,A4_bin)
  colnames(data_bin) <- c("A1_freq", "A1_dur","A2_freq", "A2_dur","A3_freq", "A3_dur","A4_freq", "A4_dur")
  
  
  ## Add session/program/subject information
  folder <- replicate(length(bin_trial), rawdata$folder[1])
  file <- replicate(length(bin_trial), rawdata$file[1])
  savename <- replicate(length(bin_trial), filename)
  subject <- replicate(length(bin_trial), rawdata$Subject[1])
  protocol <- replicate(length(bin_trial), rawdata$Protocol[1])
  station <- replicate(length(bin_trial), rawdata$Station[1])
  run <- replicate(length(bin_trial), rawdata$Run[1])
  project <- replicate(length(bin_trial), rawdata$Project[1])
  userID <- replicate(length(bin_trial), rawdata$UserID[1])
  session <- replicate(length(bin_trial), rawdata$Session[1])
  
  ## Combine all the data together
  data_bin <- data.table(folder, file, savename, subject, protocol, station, run, project, userID, session, bin_trial, bin_state, bin_time, bin_timewithin, data_bin)

  
  
  ## Save as .csv file
  # Create a filenaming system that is identical to the original data but with a prefix that identifies the analysis method
  savefilename <- filename
  # Create a new processed data folder
  savefolderpath <- here(folderpath, "Processed_TimeBin")
  dir.create(savefolderpath)
  
  savefilepath <- here(savefolderpath, savefilename)
  fwrite(data_bin, savefilepath)
  
  # Function end
}



# Operant_BinPrePostEventAnalysis -----------------------------------------

# # Example Inputs:
#
# ########## Set parameters ############
# 
# 
# filename <- "LPLAcquisitionRR2Day1B_Rat43.csv"
# 
# filepath <-here("rawdata", "Marios", "3_LeverPressingForLights", "LPL_Acquisition_RR2_Day1", filename)
# 
# ## List of states
# S = c("IRI" = 1,
#       "Flash" = 2,
#       "Steady" = 3,
#       "End" = 4)
# 
# ## Time base the linc was set to, in ms
# timebase = 20
# 
# ## Time bins to analyze each state in, in s
# timebinwidth = 1
# 
# ## States to bin and which not to bin
# nobin = c( S["End"])
# bin = c( S["Flash"], S["Steady"])
# 
# ## Identify state that trials start in, Here These are states that occur after a 'successful' operant response
# trialstartstate = c(S["IRI"])
# 
# ## Time bin pre and post target states of interest, in seconds
# prebintime <- 5
# postbintime <- 5
# 
# ## Read in data
# rawdata <- fread(filepath)
# ## Key to convert actions

coulbourn_processdata_Instrumental_PrePosttimebin <- function(filename,folderpath,S,timebase, timebinwidth, nobin, bin, trialstartstate, prebintime, postbintime) {
  
  filepath <- here(folderpath, filename)
  
  ## Read in data
  rawdata <- fread(filepath)
## Key to convert actions

# key_actions <- c("LLR_On" = "A1_On",
#   "RLR_On" = "A2_On",
#   "Mag_On" = "A3_On",
#   "A4_On" = "A4_On",
#   "LLR_Off" = "A1_Off",
#   "RLR_Off" = "A2_Off",
#   "Mag_Off" = "A3_Off",
#   "A4_Off" = "A4_Off")


# Change time units to seconds --------------------------------------------

rawdata <- rawdata %>% 
  mutate(Time = Time*timebase/1000)

# Calculate Trials --------------------------------------------------------


## Add a column to the data set indicating trial number

### Identify the start time of each trial, and the final end state (indicated by -1 in Coulbourn)
trialTimes <- c(which(rawdata$`Transition State` == trialstartstate), which(rawdata$`Transition State` == -1))
trialDuration <- diff(trialTimes)
totalTrials <- length(trialTimes) - 1


## Initialise trialnum variable with 0s for every row
trialNum <- replicate(nrow(rawdata),0)

for (i in c(1:totalTrials)) {
  trialIdx = c(trialTimes[i]:(trialTimes[i+1]-1))
  trialNum[trialIdx] = i
  
}

## append trial counter to dataframe
rawdata <- cbind(rawdata,trialNum)


# Create time-binned data - Frequency --------------------------------------
## State change info
statechange_idx = which(rawdata$`Transition State` !=0)
statechange_ID = rawdata$`Transition State`[statechange_idx]
statechange_Time = rawdata$Time[statechange_idx]
statechange_TrialNum = rawdata$trialNum[statechange_idx]
## Target bin state info
binstates_idx =  which(!is.na(match(statechange_ID, bin)))
binstate_ID = statechange_ID[binstates_idx]
binstart_Time = statechange_Time[binstates_idx]
binend_Time = statechange_Time[binstates_idx+1]
binTrialNum = statechange_TrialNum[binstates_idx]
## Add extra artificial "bins" for a pre and post target state period identify
prebin_start <- binstart_Time - prebintime
prebin_end <- binstart_Time
prebin_stateID <- replicate(n = length(prebin_start), 111)
prebin_TrialNum <- binTrialNum
postbin_start <- binend_Time
postbin_end <- binend_Time + postbintime
postbin_stateID <- replicate(n = length(prebin_start), 222)
postbin_TrialNum <- binTrialNum

binend_Time <- c(binend_Time, prebin_end, postbin_end)
binstart_Time <- c(binstart_Time, prebin_start, postbin_start)
binstate_ID <- c(binstate_ID, prebin_stateID, postbin_stateID)
binTrialNum <- c(binTrialNum, prebin_TrialNum, postbin_TrialNum)


# Calculate a new variable with interval units for each state (e.g. 1s bins)
bin_time <- 0
bin_state <- 0
bin_trial <- 0
bin_timewithin <- 0

# If no states are present [i.e. animal didn't do anything] return zeros

if (length(binstart_Time) == 0 ) {} else {
for (i in  c(1:length(binstart_Time))) {
  
  # Timebin intervals
  Temp <- seq(from = binstart_Time[i], to = binend_Time[i] - timebinwidth, by = timebinwidth)
  bin_time <- c(bin_time, Temp)
  # State IDS
  Temp <- replicate(length(Temp),binstate_ID[i])
  bin_state <- c(bin_state, Temp)
  # Trial Number
  Temp <- replicate(length(Temp),binTrialNum[i])
  bin_trial <- c(bin_trial, Temp)
  # Time within the State
  Temp <- seq(from = timebinwidth, to = length(Temp), by = timebinwidth)
  bin_timewithin <- c(bin_timewithin, Temp)
  
}
# Clear initialised value from position 1 of vars
bin_time <- bin_time[-1] 
bin_state <- bin_state[-1] 
bin_trial <- bin_trial[-1] 
bin_timewithin <- bin_timewithin[-1]
## Create new variable with bin_endtime
bin_endtime <- bin_time + timebinwidth
}

# Count Frequency of events in bins ---------------------------------------

# Extract data
## repeat for each of the 4 possible actions
A1_on <- rawdata$Time[rawdata$A1_On == 1]
A1_off <- rawdata$Time[rawdata$A1_Off == 1]
A1_bin <- coulbourn_actionbin(A1_on, A1_off, bin_time, bin_endtime)

A2_on <- rawdata$Time[rawdata$A2_On == 1]
A2_off <- rawdata$Time[rawdata$A2_Off == 1]
A2_bin <- coulbourn_actionbin(A2_on, A2_off, bin_time, bin_endtime)

A3_on <- rawdata$Time[rawdata$A3_On == 1]
A3_off <- rawdata$Time[rawdata$A3_Off == 1]
A3_bin <- coulbourn_actionbin(A3_on, A3_off, bin_time, bin_endtime)

A4_on <- rawdata$Time[rawdata$A4_On == 1]
A4_off <- rawdata$Time[rawdata$A4_Off == 1]
A4_bin <- coulbourn_actionbin(A4_on, A4_off, bin_time, bin_endtime)

## Combine into a dataframe and rename variables appropriately
data_bin <- cbind(A1_bin,A2_bin,A3_bin,A4_bin)
colnames(data_bin) <- c("A1_freq", "A1_dur","A2_freq", "A2_dur","A3_freq", "A3_dur","A4_freq", "A4_dur")


## Add session/program/subject information
folder <- replicate(length(bin_trial), rawdata$folder[1])
file <- replicate(length(bin_trial), rawdata$file[1])
savename <- replicate(length(bin_trial), filename)
subject <- replicate(length(bin_trial), rawdata$Subject[1])
protocol <- replicate(length(bin_trial), rawdata$Protocol[1])
station <- replicate(length(bin_trial), rawdata$Station[1])
run <- replicate(length(bin_trial), rawdata$Run[1])
project <- replicate(length(bin_trial), rawdata$Project[1])
userID <- replicate(length(bin_trial), rawdata$UserID[1])
session <- replicate(length(bin_trial), rawdata$Session[1])

## Combine all the data together
data_bin <- data.table(folder, file, savename, subject, protocol, station, run, project, userID, session, bin_trial, bin_state, bin_time, bin_timewithin, data_bin)



## Save as .csv file
# Create a filenaming system that is identical to the original data but with a prefix that identifies the analysis method
savefilename <- filename
# Create a new processed data folder
savefolderpath <- here(folderpath, "Processed_TimeBin")
dir.create(savefolderpath)

savefilepath <- here(savefolderpath, savefilename)
fwrite(data_bin, savefilepath)

# Function end
}




# Modified FUnctions Start Here -------------------------------------------



# Operant_SessionTimeBinAnalysis -----------------------------------------
# 
# # Example Inputs:
# 
# ########## Set parameters ############
# 
# 
# filename <- "LPLAcquisitionRR2Day1B_Rat43.csv"
# 
# filepath <-here("rawdata", "Marios", "3_LeverPressingForLights", "LPL_Acquisition_RR2_Day1", filename)
# 
# ## List of states
# S = c("IRI" = 1,
#       "Flash" = 2,
#       "Steady" = 3,
#       "End" = 4)
# 
# ## Time base the linc was set to, in ms
# timebase = 20
# 
# ## Time bins to analyze each state in, in s
# timebinwidth = 60
# 
# ## Count the frequency and duration of these states
# bin = c(S["Flash"], S["Steady"])
# 
# ## state that should be considered the end of the session. For the absolute end of recording specify the number -1 [the state_ID of the finished state in Coulbourn]
# sessionendstate <- S["End"]

coulbourn_processdata_Operant_SessionTimeBinAnalysis <- function(filename,folderpath,S,timebase, timebinwidth, bin, sessionendstate) {
  
  filepath <- here(folderpath, filename)
  
  ## Read in data
  rawdata <- fread(filepath)
  ## Key to convert actions
  
  # key_actions <- c("LLR_On" = "A1_On",
  #   "RLR_On" = "A2_On",
  #   "Mag_On" = "A3_On",
  #   "A4_On" = "A4_On",
  #   "LLR_Off" = "A1_Off",
  #   "RLR_Off" = "A2_Off",
  #   "Mag_Off" = "A3_Off",
  #   "A4_Off" = "A4_Off")
  
  
  # Change time units to seconds --------------------------------------------
  
  rawdata <- rawdata %>% 
    mutate(Time = Time*timebase/1000)
  
  # Calculate TIme Bins across session --------------------------------------------------------
  sessionlength <-  rawdata$Time[which(rawdata$`Transition State` == sessionendstate)]
  
  # Timebins 
  totalbins <- sessionlength/timebinwidth
  timebins <- c(1:totalbins)
  timebin_start <- seq(from = 0, to = sessionlength-1, by= timebinwidth)
  timebin_end <- seq(from = timebinwidth, to = sessionlength, by= timebinwidth)
  
  
  # Count Frequency of events in bins ---------------------------------------
  
  # Extract data
  ## repeat for each of the 4 possible actions
  A1_on <- rawdata$Time[rawdata$A1_On == 1]
  A1_off <- rawdata$Time[rawdata$A1_Off == 1]
  A1_bin <- coulbourn_actionbin(A1_on, A1_off, timebin_start, timebin_end)
  
  A2_on <- rawdata$Time[rawdata$A2_On == 1]
  A2_off <- rawdata$Time[rawdata$A2_Off == 1]
  A2_bin <- coulbourn_actionbin(A2_on, A2_off, timebin_start, timebin_end)
  
  A3_on <- rawdata$Time[rawdata$A3_On == 1]
  A3_off <- rawdata$Time[rawdata$A3_Off == 1]
  A3_bin <- coulbourn_actionbin(A3_on, A3_off, timebin_start, timebin_end)
  
  A4_on <- rawdata$Time[rawdata$A4_On == 1]
  A4_off <- rawdata$Time[rawdata$A4_Off == 1]
  A4_bin <- coulbourn_actionbin(A4_on, A4_off, timebin_start, timebin_end)
  
  
  # Loop through all the states you want to bin
  statebins = 0
  statebinnames <- "0"
  for (i in 1:length(bin)) {
    statei_on <- rawdata$Time[rawdata$`Transition State` == bin[i]]
    statei_off <- rawdata$Time[rawdata$`Transition State` > 0 & rawdata$`Current State`== bin[i]]
    
    temp <- coulbourn_actionbin(statei_on, statei_off, timebin_start, timebin_end)
    statebins <- cbind(statebins, temp)
    
    statebinnames <- c(statebinnames, paste(names(bin[i]), "freq", sep = "_"), paste(names(bin[i]), "dur", sep = "_"))
  }
  #Drop redundant initialised column
  statebins <-subset(statebins, select = -1)
  statebinnames <- statebinnames[-1]
  
  ## Combine into a dataframe and rename variables appropriately
  data_bin <- cbind(A1_bin,A2_bin,A3_bin,A4_bin, statebins)
  colnames(data_bin) <- c("A1_freq", "A1_dur","A2_freq", "A2_dur","A3_freq", "A3_dur","A4_freq", "A4_dur", statebinnames)
  
  
  ## Add session/program/subject information
  folder <- replicate(length(timebins), rawdata$folder[1])
  file <- replicate(length(timebins), rawdata$file[1])
  savename <- replicate(length(timebins), filename)
  subject <- replicate(length(timebins), rawdata$Subject[1])
  protocol <- replicate(length(timebins), rawdata$Protocol[1])
  station <- replicate(length(timebins), rawdata$Station[1])
  run <- replicate(length(timebins), rawdata$Run[1])
  project <- replicate(length(timebins), rawdata$Project[1])
  userID <- replicate(length(timebins), rawdata$UserID[1])
  session <- replicate(length(timebins), rawdata$Session[1])
  
  ## Combine all the data together
  data_bin <- data.table(folder, file, savename, subject, protocol, station, run, project, userID, session, timebins,timebin_start, timebin_end, data_bin)

  
  
  ## Save as .csv file
  # Create a filenaming system that is identical to the original data but with a prefix that identifies the analysis method
  savefilename <- filename
  # Create a new processed data folder
  savefolderpath <- here(folderpath, "Processed_TimeBin")
  dir.create(savefolderpath)
  
  savefilepath <- here(savefolderpath, savefilename)
  fwrite(data_bin, savefilepath)
  
  # Function end
}

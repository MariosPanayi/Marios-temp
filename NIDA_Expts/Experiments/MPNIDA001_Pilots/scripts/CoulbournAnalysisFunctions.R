# Coulbourn Data processing and Analysis FUnctions


# Necessary Libraries -----------------------------------------------------


library(tidyverse)
library(knitr)
# Package for relative file paths
library(here)

# Function: Data splitter -------------------------------------------------

# # Example Inputs:
#
# filename <- "CRF_MagTrain_Sess1.txt"
# folderpath <- here("rawdata", "Marios",'2_ConditionedReinforcement', 'MagTrain_Day1')

coulbourn_rawdatasplit <- function(filename,folderpath) {
  
  filepath <- here(folderpath, filename)
  
  # Coulbourn files are tab separated value .txt files
  rawdata <- read_tsv(filepath)
  
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
    write_csv(temp, filepath)
    
  }
  
  
  
  # end function
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
  rawdata <- read_csv(filepath)
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
  
  
  # Count Frequency of events in bins ---------------------------------------
  
  #### Change this into a function so you can pass each action into it and append the output
  
  
  coulbourn_actionbin <- function(A_on, A_off){
    
    
    #### Check data for errors###
    
    ## Is data set empty? If so, just return 0s in databins
    
    if (length(A_on) > 0) {
      
      # First action initiated before recording started
      # Set an action initiation at time = 0. This will artificially inflate the number of actions, but start of session is not interesting
      if (A_on[1] > A_off[1]) {
        A_on <- c(0, A_on)
      }
      
      # If last value of action_start is greater than the last value of action ending, then recording stopped while action still in process
      # Set an action ending at the final session end time
      if (tail(A_on,1) > tail(A_off,1)) {
        A_off <- c(A_off, tail(rawdata$Time,1))
      }
      
      # Check for any actions that don't have an exit recorded. Assumption = action ended faster than time resolution of the system
      # Check for any actions that don't have a start. Assumption = action started after last action ended, but within time resolution of the system
      
      # First check if there is exactly 1 action, if so, this code will break, so skip
      
      if (length(A_on) == 1) {print("only one action performed")} else {
        
       for (i in c(1:(length(A_on)-1))) {
        if (A_on[i+1] < A_off[i]) {
          A_off <- c(A_off[c(1:(i-1))], (A_on[i+1] + timebase/1000) , A_off[c(i:length(A_off))])
          print("Check timestamps: actions happened without an end signal")
        }
      }
      
      for (i in c(1:(length(A_off)-1))) {
        if (A_off[i+1] < A_on[i]) {
          A_on <- c(A_on[c(1:(i-1))], (A_off[i+1] + timebase/1000) , A_on[c(i:length(A_on))])
          print("Check timestamps: actions ended without a corresponding enrty signal")
        }
      }
      
      }
      
      #### Calculate frequency and duration of actions ####
      # Initialise variable
      bin_A_freq <- replicate(length(bin_time), 0)
      bin_A_dur <- replicate(length(bin_time), 0)
      for (i in c(1:length(bin_time))){
        bin_A_freq[i] = sum(A_on >= bin_time[i] & A_on < (bin_time[i] + timebinwidth))
        
        bin_start <- bin_time[i]
        bin_end <- bin_time[i] + timebinwidth
        for (j in c(1:length(A_on))){
          # action starts before timebin and ends within timebin
          if ( A_on[j] < bin_start & A_off[j] >= bin_start & A_off[j] < bin_end) {
            bin_A_dur[i] <- bin_A_dur[i] + (A_off[j] - bin_start)
            # action after timebin starts and before timebin ends
          } else if ( A_on[j] >= bin_start & A_off[j] < bin_end ) {
            bin_A_dur[i] <- bin_A_dur[i] + (A_off[j] - A_on[j])
            # action starts within the timebin , and ends after timebin
          } else if ( A_on[j] >= bin_start & A_on[j] < bin_end & A_off[j] >= bin_end ) {
            bin_A_dur[i] <- bin_A_dur[i] + bin_end - A_on[j]
            # action before bin starts and continues until after bin
          } else if ( A_on[j] <= bin_start & A_off[j] > bin_end ) {
            bin_A_dur[i] <- bin_A_dur[i] + timebinwidth
          } else {bin_A_dur[i] <- bin_A_dur[i]}
        }
      }
      
      
      
      bin_A_data <- cbind(bin_A_freq,bin_A_dur)
      
      return(bin_A_data)
      
    } else {
      bin_A_freq <- replicate(length(bin_time), 0)
      bin_A_dur <- replicate(length(bin_time), 0)
      bin_A_data <- cbind(bin_A_freq,bin_A_dur)
      
      return(bin_A_data)
    }
    
    # End of function
  }
  
  
  # Extract data
  ## repeat for each of the 4 possible actions
  A1_on <- rawdata$Time[rawdata$A1_On == 1]
  A1_off <- rawdata$Time[rawdata$A1_Off == 1]
  A1_bin <- coulbourn_actionbin(A1_on, A1_off)
  
  A2_on <- rawdata$Time[rawdata$A2_On == 1]
  A2_off <- rawdata$Time[rawdata$A2_Off == 1]
  A2_bin <- coulbourn_actionbin(A2_on, A2_off)
  
  A3_on <- rawdata$Time[rawdata$A3_On == 1]
  A3_off <- rawdata$Time[rawdata$A3_Off == 1]
  A3_bin <- coulbourn_actionbin(A3_on, A3_off)
  
  
  A4_on <- rawdata$Time[rawdata$A4_On == 1]
  A4_off <- rawdata$Time[rawdata$A4_Off == 1]
  A4_bin <- coulbourn_actionbin(A4_on, A4_off)
  
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
  data_bin <- cbind(folder, file, savename, subject, protocol, station, run, project, userID, session, bin_trial, bin_state, bin_time, bin_timewithin, data_bin)
  # convert to dataframe for saving
  data_bin <- as.data.frame(data_bin)
  
  
  ## Save as .csv file
  # Create a filenaming system that is identical to the original data but with a prefix that identifies the analysis method
  savefilename <- filename
  # Create a new processed data folder
  savefolderpath <- here(folderpath, "Processed_TimeBin")
  dir.create(savefolderpath)
  
  savefilepath <- here(savefolderpath, savefilename)
  write_csv(data_bin, savefilepath)
  
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
# rawdata <- read_csv(filepath)
# ## Key to convert actions

coulbourn_processdata_Instrumental_PrePosttimebin <- function(filename,folderpath,S,timebase, timebinwidth, nobin, bin, trialstartstate, prebintime, postbintime) {
  
  filepath <- here(folderpath, filename)
  
  ## Read in data
  rawdata <- read_csv(filepath)
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

}

# Count Frequency of events in bins ---------------------------------------

#### Change this into a function so you can pass each action into it and append the output


coulbourn_actionbin <- function(A_on, A_off){
  
  
  #### Check data for errors###
  
  ## Is data set empty? If so, just return 0s in databins
  
  if (length(A_on) > 0) {
    
    # First action initiated before recording started
    # Set an action initiation at time = 0. This will artificially inflate the number of actions, but start of session is not interesting
    if (A_on[1] > A_off[1]) {
      A_on <- c(0, A_on)
    }
    
    # If last value of action_start is greater than the last value of action ending, then recording stopped while action still in process
    # Set an action ending at the final session end time
    if (tail(A_on,1) > tail(A_off,1)) {
      A_off <- c(A_off, tail(rawdata$Time,1))
    }
    
    # Check for any actions that don't have an exit recorded. Assumption = action ended faster than time resolution of the system
    # Check for any actions that don't have a start. Assumption = action started after last action ended, but within time resolution of the system
   
     # First check if there is exactly 1 action, if so, this code will break, so skip
    
    if (length(A_on) == 1) {print("only one action performed")} else {
    
    for (i in c(1:(length(A_on)-1))) {
      if (A_on[i+1] < A_off[i]) {
        A_off <- c(A_off[c(1:(i-1))], (A_on[i+1] + timebase/1000) , A_off[c(i:length(A_off))])
        print("Check timestamps: actions happened without an end signal")
      }
    }
    
    for (i in c(1:(length(A_off)-1))) {
      if (A_off[i+1] < A_on[i]) {
        A_on <- c(A_on[c(1:(i-1))], (A_off[i+1] + timebase/1000) , A_on[c(i:length(A_on))])
        print("Check timestamps: actions ended without a corresponding enrty signal")
      }
    }
    
    }
    
    
    #### Calculate frequency and duration of actions ####
    # Initialise variable
    bin_A_freq <- replicate(length(bin_time), 0)
    bin_A_dur <- replicate(length(bin_time), 0)
    for (i in c(1:length(bin_time))){
      bin_A_freq[i] = sum(A_on >= bin_time[i] & A_on < (bin_time[i] + timebinwidth))
      
      bin_start <- bin_time[i]
      bin_end <- bin_time[i] + timebinwidth
      for (j in c(1:length(A_on))){
        # action starts before timebin and ends within timebin
        if ( A_on[j] < bin_start & A_off[j] >= bin_start & A_off[j] < bin_end) {
          bin_A_dur[i] <- bin_A_dur[i] + (A_off[j] - bin_start)
          # action after timebin starts and before timebin ends
        } else if ( A_on[j] >= bin_start & A_off[j] < bin_end ) {
          bin_A_dur[i] <- bin_A_dur[i] + (A_off[j] - A_on[j])
          # action starts within the timebin , and ends after timebin
        } else if ( A_on[j] >= bin_start & A_on[j] < bin_end & A_off[j] >= bin_end ) {
          bin_A_dur[i] <- bin_A_dur[i] + bin_end - A_on[j]
          # action before bin starts and continues until after bin
        } else if ( A_on[j] <= bin_start & A_off[j] > bin_end ) {
          bin_A_dur[i] <- bin_A_dur[i] + timebinwidth
        } else {bin_A_dur[i] <- bin_A_dur[i]}
      }
    }
    
    
    
    bin_A_data <- cbind(bin_A_freq,bin_A_dur)
    
    return(bin_A_data)
    
  } else {
    bin_A_freq <- replicate(length(bin_time), 0)
    bin_A_dur <- replicate(length(bin_time), 0)
    bin_A_data <- cbind(bin_A_freq,bin_A_dur)
    
    return(bin_A_data)
  }
  
  # End of function
}


# Extract data
## repeat for each of the 4 possible actions
A1_on <- rawdata$Time[rawdata$A1_On == 1]
A1_off <- rawdata$Time[rawdata$A1_Off == 1]
A1_bin <- coulbourn_actionbin(A1_on, A1_off)

A2_on <- rawdata$Time[rawdata$A2_On == 1]
A2_off <- rawdata$Time[rawdata$A2_Off == 1]
A2_bin <- coulbourn_actionbin(A2_on, A2_off)

A3_on <- rawdata$Time[rawdata$A3_On == 1]
A3_off <- rawdata$Time[rawdata$A3_Off == 1]
A3_bin <- coulbourn_actionbin(A3_on, A3_off)


A4_on <- rawdata$Time[rawdata$A4_On == 1]
A4_off <- rawdata$Time[rawdata$A4_Off == 1]
A4_bin <- coulbourn_actionbin(A4_on, A4_off)

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
data_bin <- cbind(folder, file, savename, subject, protocol, station, run, project, userID, session, bin_trial, bin_state, bin_time, bin_timewithin, data_bin)
# convert to dataframe for saving
data_bin <- as.data.frame(data_bin)


## Save as .csv file
# Create a filenaming system that is identical to the original data but with a prefix that identifies the analysis method
savefilename <- filename
# Create a new processed data folder
savefolderpath <- here(folderpath, "Processed_TimeBin")
dir.create(savefolderpath)

savefilepath <- here(savefolderpath, savefilename)
write_csv(data_bin, savefilepath)

# Function end
}





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
  rawdata <- read_csv(filepath)
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
  
  #### Change this into a function so you can pass each action into it and append the output
  
  
  coulbourn_actionbin <- function(A_on, A_off, rawdata, bin_start, bin_end, bin_time, timebase, timebinwidth){
    
    
    #### Check data for errors###
    
    ## Is data set empty? If so, just return 0s in databins
    
    if (length(A_on) > 0 | length(A_off) > 0) {
      

# New code start ----------------------------------------------------------
      
      # Make sure data are in chronological order!!! Otherwise loop will go on forever
      A_on <- sort(A_on)
      A_off <- sort(A_off)
      
      
      # create new empty variable to populate with values
      # Note that this inefficient method is required because Coulbourn timestamps don't obey logic,
      # There is no way to precalculate the number of events because the on and off signals might both contain independent inconsistencies
      ts = double()
      event = character()
      
      if (is_empty(A_on) & !is_empty(A_off)) {
        # Check to see if there are no ON signals
        # Situation - special case where event initiated prior to recording start
        A_on <- c(0,A_on)
      }
      if (!is_empty(A_on) & is_empty(A_off)) {
        # Check to see if there are no OFF signals
        # Situation - special case where OFF signal occurred within temporal resolution of system for first repsonse
        A_off <- c(A_on[1], A_off)
      }
      
      
      if (A_on[1] > A_off[1]) {
        # Check to see if the first event is an Off. Assumption is that the on signal happened before recording started.
        # Artificially set the action initiation as time = 0 (i.e. session start)
        A_on <- c(0,A_on)
      }
      
      # add first onset [which will exist since A_on is not empty AND we have made sure the first on is before the first off]
      
      ts <- c(ts, A_on[1])
      event <- c(event, "A_on")
      A_on <- A_on[-1]
      
      
      # Keep doing the following steps until the on and off signal vectors are empty
      # We will delete things as they are moved from A_on and A_off and transfer them 
      # to the new vector with any extra on and off signals required
      while (length(A_on) > 0 | length(A_off) > 0) {
        
        # First check if the last thing in the event list is an On or Off
        if (tail(event,1) == "A_on"){
          ### Last event is an On ###
          
          if (is_empty(A_on) & !is_empty(A_off)){
            # No more recorded ON signals - deal with remaining OFF signals if any exist
            # Solution -> add next OFF signal to ts and event lists
            ts <- c(ts, A_off[1])
            event <- c(event, "A_off")
            # Remove value from off list
            A_off <- A_off[-1]
            
          } else if (!is_empty(A_on) & is_empty(A_off)){
            # No more recorded OFF signals - Remaining ON signals to be dealt with
            # Situation  -> an On signal occurred without a corresponding OFF signal - happened within temporal resolution of system
            # Solution -> add an extra OFF event to the list
            ts <- c(ts, tail(ts,1))
            event <- c(event, "A_off")
            
          }
          
          else if (A_off[1] >= tail(ts,1)) {
            
            # OFF event comes after previous On event
            if (A_off[1] <= A_on[1] ) {
              # Condition 1: Off event is before Next On event and after previous On event
              # Situation -> OFF signal where it was expected 
              # Solution -> add to ts and event lists
              ts <- c(ts, A_off[1])
              event <- c(event, "A_off")
              # Remove value from off list
              A_off <- A_off[-1]
              
            } else if (A_off[1] > A_on[1]) {
              # Condition 2: Off event is after Next On event AND correctly after previous ON event
              # Situation -> OFF event after previous ON happened faster than system temporal resolutions
              # Solution -> add an extra OFF event to the list
              ts <- c(ts, tail(ts,1))
              event <- c(event, "A_off")
              
            } 
          }
          
        } 
        
        
        else if (tail(event,1) == "A_off") {
          ### Last event is an Off ###
          
          if (is_empty(A_off) & !is_empty(A_on)){
            # No more recorded Off signals - deal with remaining OFF signals if any exist
            # Solution -> add next ON signal to ts and event lists
            ts <- c(ts, A_on[1])
            event <- c(event, "A_on")
            # Remove value from on list
            A_on <- A_on[-1]
            
          } else if (!is_empty(A_off) & is_empty(A_on)){
            # No more recorded ON signals - Remaining OFF signals to be dealt with
            # Situation -> an OFF signal occurred without a corresponding ON signal - happened within temporal resolution of system
            # Solution -> add an extra ON event to the list
            ts <- c(ts, tail(ts,1))
            event <- c(event, "A_on")
            
          } 
          
          else if(A_on[1] >= tail(ts,1)){
            
            if (A_on[1] <= A_off[1]){
              # COndition 3: Next On event comes before the Next Off event AND Next ON event comes after previous OFF event
              # Situation -> On signal where it is expected
              # SOlution -> add to ts and event lists
              ts <- c(ts, A_on[1])
              event <- c(event, "A_on")
              # Remove value from off list
              A_on <- A_on[-1]
              
            } else if(A_on[1] > A_off[1]){
              # Condition 4: Next On event comes AFTER the Next Off event AND Next ON event comes after previous OFF event
              # Situation -> On event after previous OFF happened faster than system temporal resolutions
              # Solution -> add an extra ON event to the list
              ts <- c(ts, tail(ts,1))
              event <- c(event, "A_on")
              
            } 
          }
        }
        
        # While loop end
      }
      
      # Check to see if final event was an ON. If so, then Add a corresponding OFF straight afterwards
      # IMPORTANT - This is a major assumption!!! It is also possible that the final action doesn't have a corresponding OFF signal because the session ended.
      # However, this is really hard to judge without looking at each case individually. 
      # Given that the end of the session usually has little of interest, this is a conservative assumption about the animals' behaviour that is consistent 
      # with the assumptions earlier in this script.
      
      if (tail(event,1) == "A_on") {
        ts <- c(ts, tail(ts,1))
        event <- c(event, "A_off")
        
      }
      #Finally convert the data back into variables A_on and A_off
      
      A_on <- ts[event == "A_on"]
      A_off <- ts[event == "A_off"]
    

# New Code End ------------------------------------------------------------


      #### Calculate frequency and duration of actions ####
      # Initialise variable
      bin_A_freq <- replicate(length(bin_time), 0)
      bin_A_dur <- replicate(length(bin_time), 0)
      for (i in c(1:length(bin_time))){
        bin_A_freq[i] = sum(A_on >= bin_time[i] & A_on < (bin_time[i] + timebinwidth))
        
        bin_start <- bin_time[i]
        bin_end <- bin_time[i] + timebinwidth
        for (j in c(1:length(A_on))){
          # action starts before timebin and ends within timebin
          if ( A_on[j] < bin_start & A_off[j] >= bin_start & A_off[j] < bin_end) {
            bin_A_dur[i] <- bin_A_dur[i] + (A_off[j] - bin_start)
            # action after timebin starts and before timebin ends
          } else if ( A_on[j] >= bin_start & A_off[j] < bin_end ) {
            bin_A_dur[i] <- bin_A_dur[i] + (A_off[j] - A_on[j])
            # action starts within the timebin , and ends after timebin
          } else if ( A_on[j] >= bin_start & A_on[j] < bin_end & A_off[j] >= bin_end ) {
            bin_A_dur[i] <- bin_A_dur[i] + bin_end - A_on[j]
            # action before bin starts and continues until after bin
          } else if ( A_on[j] <= bin_start & A_off[j] > bin_end ) {
            bin_A_dur[i] <- bin_A_dur[i] + timebinwidth
          } else {bin_A_dur[i] <- bin_A_dur[i]}
        }
      }
      
      
      
      bin_A_data <- cbind(bin_A_freq,bin_A_dur)
      
      return(bin_A_data)
      
    } else {
      bin_A_freq <- replicate(length(bin_time), 0)
      bin_A_dur <- replicate(length(bin_time), 0)
      bin_A_data <- cbind(bin_A_freq,bin_A_dur)
      
      return(bin_A_data)
    }
    
    # End of function
  }
  
  
  # Extract data
  ## repeat for each of the 4 possible actions
  A1_on <- rawdata$Time[rawdata$A1_On == 1]
  A1_off <- rawdata$Time[rawdata$A1_Off == 1]
  A1_bin <- coulbourn_actionbin(A1_on, A1_off, rawdata, timebin_start, timebin_end, timebin_start, timebase, timebinwidth)
  
  A2_on <- rawdata$Time[rawdata$A2_On == 1]
  A2_off <- rawdata$Time[rawdata$A2_Off == 1]
  A2_bin <- coulbourn_actionbin(A2_on, A2_off, rawdata, timebin_start, timebin_end, timebin_start, timebase, timebinwidth)
  
  A3_on <- rawdata$Time[rawdata$A3_On == 1]
  A3_off <- rawdata$Time[rawdata$A3_Off == 1]
  A3_bin <- coulbourn_actionbin(A3_on, A3_off, rawdata, timebin_start, timebin_end, timebin_start, timebase, timebinwidth)
  
  
  A4_on <- rawdata$Time[rawdata$A4_On == 1]
  A4_off <- rawdata$Time[rawdata$A4_Off == 1]
  A4_bin <- coulbourn_actionbin(A4_on, A4_off, rawdata, timebin_start, timebin_end, timebin_start, timebase, timebinwidth)
  
  
  # Loop through all the states you want to bin
  statebins = 0
  statebinnames <- "0"
  for (i in 1:length(bin)) {
    statei_on <- rawdata$Time[rawdata$`Transition State` == bin[i]]
    statei_off <- rawdata$Time[rawdata$`Transition State` > 0 & rawdata$`Current State`== bin[i]]
    
    temp <- coulbourn_actionbin(statei_on, statei_off, rawdata, timebin_start, timebin_end, timebin_start, timebase, timebinwidth)
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
  data_bin <- cbind(folder, file, savename, subject, protocol, station, run, project, userID, session, timebins,timebin_start, timebin_end, data_bin)
  # convert to dataframe for saving
  data_bin <- as.data.frame(data_bin)
  
  
  ## Save as .csv file
  # Create a filenaming system that is identical to the original data but with a prefix that identifies the analysis method
  savefilename <- filename
  # Create a new processed data folder
  savefolderpath <- here(folderpath, "Processed_TimeBin")
  dir.create(savefolderpath)
  
  savefilepath <- here(savefolderpath, savefilename)
  write_csv(data_bin, savefilepath)
  
  # Function end
}




# 
# 
# 
# 
# coulbourn_actioncleantimestamps(A_on, A_off) {
#   # Function takes a vector of Action ON times and a vector of Action OFF times from a session
#   # Data are processed to fix any issues with missing/non-mathcing ON/OFF times
#   # Assumptions made are that:
#   # 1) If an action OFF signal is the first timestamp, then the first action ON was before the session started (so first A_On set to time = 0)
#   # 2) Any ON or OFF signal that is doesn't pair chronologically with the other signals reflects an action ON/OFF signal occuring faster than the temporal resolution of the system
#   #   A corresponding On/Off signal is added so that the ON-OFF duration is 0 (i.e. they occured at the same time)
#   # These issues happen surprisingly often for Coulbourn Lever Pressing data in particular 
#   # N.B. output is a data frame of A_on and A_off, split this output apart for use wiht other functions.
#   
#   if (length(A_on) > 0 | length(A_off) > 0) {
#     
#     # Make sure data are in chronological order!!! Otherwise loop will go on forever
#     A_on <- sort(A_on)
#     A_off <- sort(A_off)
#     
#     
#     # create new empty variable to populate with values
#     # Note that this inefficient method is required because Coulbourn timestamps don't obey logic,
#     # There is no way to precalculate the number of events because the on and off signals might both contain independent inconsistencies
#     ts = double()
#     event = character()
#     
#     if (is_empty(A_on) & !is_empty(A_off)) {
#       # Check to see if there are no ON signals
#       # Situation - special case where event initiated prior to recording start
#       A_on <- c(0,A_on)
#     }
#     if (!is_empty(A_on) & is_empty(A_off)) {
#       # Check to see if there are no OFF signals
#       # Situation - special case where OFF signal occurred within temporal resolution of system for first repsonse
#       A_off <- c(A_on[1], A_off)
#     }
#     
#     
#     if (A_on[1] > A_off[1]) {
#       # Check to see if the first event is an Off. Assumption is that the on signal happened before recording started.
#       # Artificially set the action initiation as time = 0 (i.e. session start)
#       A_on <- c(0,A_on)
#     }
#     
#     # add first onset [which will exist since A_on is not empty AND we have made sure the first on is before the first off]
#     
#     ts <- c(ts, A_on[1])
#     event <- c(event, "A_on")
#     A_on <- A_on[-1]
#     
#     
#     # Keep doing the following steps until the on and off signal vectors are empty
#     # We will delete things as they are moved from A_on and A_off and transfer them 
#     # to the new vector with any extra on and off signals required
#     while (length(A_on) > 0 | length(A_off) > 0) {
#       
#       # First check if the last thing in the event list is an On or Off
#       if (tail(event,1) == "A_on"){
#         ### Last event is an On ###
#         
#         if (is_empty(A_on) & !is_empty(A_off)){
#           # No more recorded ON signals - deal with remaining OFF signals if any exist
#           # Solution -> add next OFF signal to ts and event lists
#           ts <- c(ts, A_off[1])
#           event <- c(event, "A_off")
#           # Remove value from off list
#           A_off <- A_off[-1]
#           
#         } else if (!is_empty(A_on) & is_empty(A_off)){
#           # No more recorded OFF signals - Remaining ON signals to be dealt with
#           # Situation  -> an On signal occurred without a corresponding OFF signal - happened within temporal resolution of system
#           # Solution -> add an extra OFF event to the list
#           ts <- c(ts, tail(ts,1))
#           event <- c(event, "A_off")
#           
#         }
#         
#         else if (A_off[1] >= tail(ts,1)) {
#           
#           # OFF event comes after previous On event
#           if (A_off[1] <= A_on[1] ) {
#             # Condition 1: Off event is before Next On event and after previous On event
#             # Situation -> OFF signal where it was expected 
#             # Solution -> add to ts and event lists
#             ts <- c(ts, A_off[1])
#             event <- c(event, "A_off")
#             # Remove value from off list
#             A_off <- A_off[-1]
#             
#           } else if (A_off[1] > A_on[1]) {
#             # Condition 2: Off event is after Next On event AND correctly after previous ON event
#             # Situation -> OFF event after previous ON happened faster than system temporal resolutions
#             # Solution -> add an extra OFF event to the list
#             ts <- c(ts, tail(ts,1))
#             event <- c(event, "A_off")
#             
#           } 
#         }
#         
#       } 
#       
#       
#       else if (tail(event,1) == "A_off") {
#         ### Last event is an Off ###
#         
#         if (is_empty(A_off) & !is_empty(A_on)){
#           # No more recorded Off signals - deal with remaining OFF signals if any exist
#           # Solution -> add next ON signal to ts and event lists
#           ts <- c(ts, A_on[1])
#           event <- c(event, "A_on")
#           # Remove value from on list
#           A_on <- A_on[-1]
#           
#         } else if (!is_empty(A_off) & is_empty(A_on)){
#           # No more recorded ON signals - Remaining OFF signals to be dealt with
#           # Situation -> an OFF signal occurred without a corresponding ON signal - happened within temporal resolution of system
#           # Solution -> add an extra ON event to the list
#           ts <- c(ts, tail(ts,1))
#           event <- c(event, "A_on")
#           
#         } 
#         
#         else if(A_on[1] >= tail(ts,1)){
#           
#           if (A_on[1] <= A_off[1]){
#             # COndition 3: Next On event comes before the Next Off event AND Next ON event comes after previous OFF event
#             # Situation -> On signal where it is expected
#             # SOlution -> add to ts and event lists
#             ts <- c(ts, A_on[1])
#             event <- c(event, "A_on")
#             # Remove value from off list
#             A_on <- A_on[-1]
#             
#           } else if(A_on[1] > A_off[1]){
#             # Condition 4: Next On event comes AFTER the Next Off event AND Next ON event comes after previous OFF event
#             # Situation -> On event after previous OFF happened faster than system temporal resolutions
#             # Solution -> add an extra ON event to the list
#             ts <- c(ts, tail(ts,1))
#             event <- c(event, "A_on")
#             
#           } 
#         }
#       }
#       
#       # While loop end
#     }
#     
#     # Check to see if final event was an ON. If so, then Add a corresponding OFF straight afterwards
#     # IMPORTANT - This is a major assumption!!! It is also possible that the final action doesn't have a corresponding OFF signal because the session ended.
#     # However, this is really hard to judge without looking at each case individually. 
#     # Given that the end of the session usually has little of interest, this is a conservative assumption about the animals' behaviour that is consistent 
#     # with the assumptions earlier in this script.
#     
#     if (tail(event,1) == "A_on") {
#       ts <- c(ts, tail(ts,1))
#       event <- c(event, "A_off")
#       
#     }
#     #Finally convert the data back into variables A_on and A_off
#     
#     A_on <- ts[event == "A_on"]
#     A_off <- ts[event == "A_off"]
#     
#     A_data <- data.frame(A_on, A_off)
#     return(A_data)
#     
#   } else {
#     A_data <- data.frame(A_on, A_off)
#     return(A_data)
#   }
#   
# }

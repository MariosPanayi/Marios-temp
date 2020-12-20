# Coulbourn Data processing and Analysis FUnctions


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
    # save temp data with appropriate filename
    # Clean SubjNumber
    SubjectNum = paste(c("Rat", temp$Subject[1]), collapse = "")
    SubjectNum = str_replace_all(SubjectNum, "_", "")
    
    containingfoldername <- strsplit(folderpath, '/')
    containingfoldername <- containingfoldername[[1]][length(containingfoldername[[1]])]
    containingfoldername <- str_replace_all(containingfoldername, "_", "")
    
    # Clean RUn number
    tempfilename <- paste(c(prefix, SubjectNum, containingfoldername), collapse = "_")
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
  for (i in  c(1:length(binstates_idx))) {
    
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
  savename <- replicate(length(bin_trial), filename)
  subject <- replicate(length(bin_trial), rawdata$Subject[1])
  protocol <- replicate(length(bin_trial), rawdata$Protocol[1])
  station <- replicate(length(bin_trial), rawdata$Station[1])
  run <- replicate(length(bin_trial), rawdata$Run[1])
  project <- replicate(length(bin_trial), rawdata$Project[1])
  userID <- replicate(length(bin_trial), rawdata$UserID[1])
  session <- replicate(length(bin_trial), rawdata$Session[1])
  
  ## Combine all the data together
  data_bin <- cbind(savename, subject, protocol, station, run, project, userID, session, bin_trial, bin_state, bin_time, bin_timewithin, data_bin)
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

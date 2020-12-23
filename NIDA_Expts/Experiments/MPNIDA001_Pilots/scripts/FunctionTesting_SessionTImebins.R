


# Operant_BinPrePostEventAnalysis -----------------------------------------

# Example Inputs:

########## Set parameters ############


filename <- "LPLAcquisitionRR2Day1B_Rat43.csv"

filepath <-here("rawdata", "Marios", "3_LeverPressingForLights", "LPL_Acquisition_RR2_Day1", filename)

## List of states
S = c("IRI" = 1,
      "Flash" = 2,
      "Steady" = 3,
      "End" = 4)

## Time base the linc was set to, in ms
timebase = 20

## Time bins to analyze each state in, in s
timebinwidth = 60

## States to count the occurence of
bin = c(S["Flash"], S["Steady"])

## state that should be considered the end of the session. For the absolute end of recording specify the number -1 [the state_ID of the finished state in Coulbourn]
sessionendstate <- S["End"]


## Read in data
rawdata <- read_csv(filepath)
## Key to convert actions






# coulbourn_processdata_Instrumental_PrePosttimebin <- function(filename,folderpath,S,timebase, timebinwidth, nobin, bin) {
#   
  # filepath <- here(folderpath, filename)
  # 
  # ## Read in data
  # rawdata <- read_csv(filepath)
  # ## Key to convert actions
  
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

  coulbourn_actionbin(A_on, A_off, rawdata, timebin_start, timebin_end, timebin_start, timebase, timebinwidth)
    
  # Count Frequency of events in bins ---------------------------------------
  
  #### Change this into a function so you can pass each action into it and append the output
  
  
  coulbourn_actionbin <- function(A_on, A_off, rawdata, bin_start, bin_end, bin_time, timebase, timebinwidth){
    
    
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


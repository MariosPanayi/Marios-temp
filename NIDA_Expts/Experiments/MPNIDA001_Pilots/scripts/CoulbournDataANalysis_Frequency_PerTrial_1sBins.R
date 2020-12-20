# Title: CoulbournDataANalysis_Frequency_PerTrial_1sBins

## Author: Marios Panayi
## Date: 16-DEC-2020
## Purpose: Analyze Extracted [i.e. individual] data from Coulbourn boxes
## Notes: Data from NIDA Pilots
# Load Packagaes ----------------------------------------------------------
## Packages for data organisation and plotting
library(tidyverse)
library(knitr)
# Package for relative file paths
library(here)
# library(ggpubr)
library(cowplot)
library(ggsignif)
library(patchwork)
library(RColorBrewer)
## Packages for Data analysis
library(afex)
afex_options(emmeans_model = "multivariate")# use multivariate model for all follow-up tests.
library(emmeans)
# install.packages("devtools")
# devtools::install_github("crsh/papaja")
# library(papaja)
# library(knitr)
# remotes::install_github("noamross/redoc")
# library(redoc)


# Load Individual File ----------------------------------------------------

## Set file path
filename <- "CRFMagTrainSess1_Rat11_Run1.csv"
folderpath <- here("rawdata", "Marios",'2_ConditionedReinforcement')
filepath <- here(folderpath, filename)

## Read in data
rawdata <- read_csv(filepath)


# Set parameters ----------------------------------------------------------


## List of states
S = c("ITI" = 1,
      "Pre" = 2,
      "Pel" = 3)

## Time base the linc was set to, in ms
timebase = 20

## Time bins to analyze each state in, in s
timebinwidth = 1

## States not to bin
nobin = S["ITI"]

bin = c(S["Pre"], S["Pel"])

## Key to convert actions

key_actions <- c("LLR_On" = "A1_On",
  "RLR_On" = "A2_On",
  "Mag_On" = "A3_On",
  "A4_On" = "A4_On",
  "LLR_Off" = "A1_Off",
  "RLR_Off" = "A2_Off",
  "Mag_Off" = "A3_Off",
  "A4_Off" = "A4_Off")


# Change time units to seconds --------------------------------------------

rawdata <- rawdata %>% 
  mutate(Time = Time*timebase/1000)

# Calculate Trials --------------------------------------------------------
## Identify state that trials start in, usually the ITI ;)
trialstartstate = S["ITI"]

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
  #
}
# Clear initialised value from position 1 of vars
bin_time <- bin_time[-1] 
bin_state <- bin_state[-1] 
bin_trial <- bin_trial[-1] 



# Count Frequency of events in bins ---------------------------------------


#### Change this into a function so you can pass each action into it and append the output

# Extract data
#Check data for errors
A3_on <- rawdata$Time[rawdata$A3_On == 1]
A3_off <- rawdata$Time[rawdata$A3_Off == 1]

# First action initiated before recording started
# Set an action initiation at time = 0. This will artificially inflate the number of actions, but start of session is not interesting
if (A3_on[1] > A3_off[1]) {
  A3_on <- c(0, A3_on)
}

# If last value of action_start is greater than the last value of action ending, then recording stopped while action still in process
# Set an action ending at the final session end time
if (tail(A3_on,1) > tail(A3_off,1)) {
  A3_off <- c(A3_off, tail(rawdata$Time,1))
}

# Check for any actions that don't have an exit recorded. Assumption = action ended faster than time resolution of the system
# Check for any actions that don't have a start. Assumption = action started after last action ended, but within time resolution of the system
for (i in c(1:(length(A3_on)-1))) {
  if (A3_on[i+1] < A3_off[i]) {
    A3_off <- c(A3_off[c(1:(i-1))], (A3_on[i+1] + timebase/1000) , A3_off[c(i:length(A3_off))])
    print("Check timestamps: actions happened without an end signal")
  }
}

for (i in c(1:(length(A3_off)-1))) {
  if (A3_off[i+1] < A3_on[i]) {
    A3_on <- c(A3_on[c(1:(i-1))], (A3_off[i+1] + timebase/1000) , A3_on[c(i:length(A3_on))])
    print("Check timestamps: actions ended without a corresponding enrty signal")
  }
}



## Calculate frequency and duration of actions
# Initialise variable
bin_A3_freq <- replicate(length(bin_time), 0)
bin_A3_dur <- replicate(length(bin_time), 0)
for (i in c(1:length(bin_time))){
  bin_A3_freq[i] = sum(A3_on >= bin_time[i] & A3_on < (bin_time[i] + timebinwidth))
  
  bin_start <- bin_time[i]
  bin_end <- bin_time[i] + timebinwidth
  for (j in c(1:length(A3_on))){
    # action starts before timebin and ends within timebin
    if ( A3_on[j] < bin_start & A3_off[j] >= bin_start & A3_off[j] < bin_end) {
      bin_A3_dur[i] <- bin_A3_dur[i] + (A3_off[j] - bin_start)
    # action after timebin starts and before timebin ends
    } else if ( A3_on[j] >= bin_start & A3_off[j] < bin_end ) {
      bin_A3_dur[i] <- bin_A3_dur[i] + (A3_off[j] - A3_on[j])
    # action starts within the timebin , and ends after timebin
    } else if ( A3_on[j] >= bin_start & A3_on[j] < bin_end & A3_off[j] >= bin_end ) {
      bin_A3_dur[i] <- bin_A3_dur[i] + bin_end - A3_on[j]
      # action before bin starts and continues until after bin
    } else if ( A3_on[j] <= bin_start & A3_off[j] > bin_end ) {
      bin_A3_dur[i] <- bin_A3_dur[i] + timebinwidth
    } else {bin_A3_dur[i] <- bin_A3_dur[i]}
  }
}





# names(rawdata)[12] <- "A1_On"
# names(rawdata)[13] <- "A2_On"
# names(rawdata)[14] <- "A3_On"
# names(rawdata)[15] <- "A4_On"
# names(rawdata)[16] <- "A1_Off"
# names(rawdata)[17] <- "A2_Off"
# names(rawdata)[18] <- "A3_Off"
# names(rawdata)[19] <- "A4_Off"
# 
# 


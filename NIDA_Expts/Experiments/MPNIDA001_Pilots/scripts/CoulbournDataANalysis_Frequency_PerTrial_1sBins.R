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
filename <- "CRFMagTrainSess1_Rat10_Run1.csv"
folderpath <- here("rawdata", "Marios",'ConditionedReinforcement')
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


# Change time units to seconds --------------------------------------------

rawdata %>% 
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

## Set up new dataframe

A1_times <- 


for (i in c(1:totalTrials)) {
  trialIdx = c(trialTimes[i]:(trialTimes[i+1]-1))
  
  ## Find row values for the tranisition state entries that are in the "bin" list
  
  statechange_start = which(!is.na(match(rawdata$`Transition State`, bin)))
  
  
  statechange_idx = which(rawdata$`Transition State` !=0)
  statechange_ID = rawdata$`Transition State`[statechange_idx]
  statechange_trialNUm = rawdata$trialNum[statechange_idx]
  
  # data.frame(statechange_idx,statechange_ID,statechange_trialNUm)
  
}

rawdata$Time[which(rawdata[,14] == 1)]





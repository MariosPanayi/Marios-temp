# Load Packagaes ----------------------------------------------------------
## Packages for data organisation and plotting
library(tidyverse)
library(knitr)
# Package for relative file paths
library(here)
# Load Analysis Functions
source(here("scripts", "CoulbournAnalysisFunctions.R"))


# Identify files to analyze

# Can't use here() function effectively here, so have to create relative file paths
## COllapses multiple subfolders if needed
datafoldersinproject1 <- c("rawdata")
datafoldersinproject2 <-c("Marios")

## Project specific folder
projectdatafolder <- c("2_ConditionedReinforcement")

## Final level of folders contianing the relevant .txt Coulbourn files
listofdatafolders <- c("Acquisition_Day1",
                       "Acquisition_Day2",
                       "Acquisition_Day3")


#  extract data filenames, only .txt --------------------------------------


datafilepaths <-0
for (i in c(1:length(listofdatafolders))){
lookup <- paste(datafoldersinproject1, datafoldersinproject2, projectdata_folder, listofdatafolders[i], sep = "/")

  datafilepaths <- list.files(path = lookup, pattern = ".txt")
  for (j in c(1:length(datafilepaths))) {
    
    # For each raw.txt file split up the data into indivudal subjects .csv files for subsequent analysis
    folderpath <- here(datafoldersinproject1, datafoldersinproject2,projectdatafolder,listofdatafolders[i])
    filename <- datafilepaths[j]
    ## Run Function
    coulbourn_rawdatasplit(filename,folderpath) 
  }
  
}



#  extract processed data filenames, only .csv and put them into a --------

########## Set parameters ############

## List of states
S = c("ITI" = 1,
      "PreCS" = 2,
      "Click" = 3,
      "Noise" = 4,
      "Tone" = 5,
      "Siren" = 6,
      "Pelletx1" = 7,
      "Pelletx2" = 8,
      "Pelletx0" = 9,
      "End" = 10)

## Time base the linc was set to, in ms
timebase = 20

## Time bins to analyze each state in, in s
timebinwidth = 1

## States to bin and which not to bin
nobin = c(S["ITI"], S["End"])
bin = c(S["PreCS"], S["Click"], S["Noise"], S["Tone"], S["Siren"], S["Pelletx1"], S["Pelletx2"], S["Pelletx0"])

## Identify state that trials start in, usually the ITI ;)
trialstartstate = S["ITI"]

########## Run Analysis ############

for (i in c(1:length(listofdatafolders))){
  lookup <- paste(datafoldersinproject1, datafoldersinproject2, projectdata_folder, listofdatafolders[i], sep = "/")
  
  datafilepaths <- list.files(path = lookup, pattern = ".csv")
  for (j in c(1:length(datafilepaths))) {
    
    # For each raw.txt file split up the data into indivudal subjects .csv files for subsequent analysis
    folderpath <- here(datafoldersinproject1, datafoldersinproject2,projectdatafolder,listofdatafolders[i])
    filename <- datafilepaths[j]
    ## Run Function - N.B. Warnings will appear to tell you that the new directory for data storage already exists. Safe to ignore.
    coulbourn_processdata_Pavlovian_timebin(filename,folderpath,S,timebase, timebinwidth, nobin, bin, trialstartstate)
  }
  
}



# Combine all data --------------------------------------------------------
#create list of all filenames
filestojoin <- "0"
for (i in c(1:length(listofdatafolders))){
  lookup <- paste(datafoldersinproject1, datafoldersinproject2, projectdata_folder, listofdatafolders[i],"Processed_TimeBin",  sep = "/")
  
  datafilepaths <- list.files(path = lookup, pattern = ".csv", full.names = TRUE)
  filestojoin <- c(filestojoin, datafilepaths)
  # for (j in c(1:length(datafilepaths))) {
  #   
  #   # For each processed .csv file, load the data and then join it together
  #   filename <- datafilepaths[j]
  #   ## Load and analyse
}
#delete initialising variable
filestojoin <- filestojoin[-1]

## Load each table of data and join into a single 
for (i in c(1:length(filestojoin))){ 
  if (i == 1){
    rawdata <- read_csv(filestojoin[i])
    } else {
      tempdata <- read_csv(filestojoin[i])
      rawdata <- full_join(rawdata,tempdata)
  }
}



# Recode States to meaningful variables -----------------------------------
## Solution: create a counterbalancing data frame and then inner_join() with the rawdata to match all rlevant rows on Subject[Make sure subject is labelled the same in both tables]
counterbalancing_subjects <- c("1_____" = "A",
                               "2_____" = "B",
                               "3_____" = "C",
                               "4_____" = "D",
                               "5_____" = "A",
                               "6_____" = "B",
                               "26____" = "A",
                               "27____" = "B",
                               "28____" = "C",
                               "29____" = "D",
                               "30____" = "A",
                               "31____" = "B",
                               "7_____" = "C",
                               "8_____" = "D",
                               "32____" = "C",
                               "33____" = "D")


relabelStates <- S = c("PreCS" = 2,
                       "Click" = 3,
                       "Noise" = 4,
                       "Tone" = 5,
                       "Siren" = 6,
                       "Pelletx1" = 7,
                       "Pelletx2" = 8,
                       "Pelletx0" = 9)

relabelPeriods <- S = c("Pre" = 2,
                        "CS" = 3,
                        "CS" = 4,
                        "CS" = 5,
                        "CS" = 6,
                        "Post" = 7,
                        "Post" = 8,
                        "Post" = 9)

counterbalancing_cues <- c("A++--" = "Click",
                           "B+-" = "Noise",
                           "C++" = "Tone",
                           "D+" = "Siren")









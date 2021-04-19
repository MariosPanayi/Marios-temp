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
listofdatafolders <- c("Test1_Day13",
                       "Test2_Day13",
                       "Test3_Day16",
                       "Test4_Day16")


#  extract data filenames, only .txt --------------------------------------


datafilepaths <-0
for (i in c(1:length(listofdatafolders))){
lookup <- paste(datafoldersinproject1, datafoldersinproject2, projectdatafolder, listofdatafolders[i], sep = "/")

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
      "Click" = 2,
      "Noise" = 3,
      "Tone" = 4,
      "Siren" = 5,
      "End" = 6)

## Time base the linc was set to, in ms
timebase = 20

## Time bins to analyze each state in, in s
timebinwidth = 60

## Count the frequency and duration of these states
bin = c(S["Click"], S["Noise"], S["Tone"], S["Siren"])

## state that should be considered the end of the session. For the absolute end of recording specify the number -1 [the state_ID of the finished state in Coulbourn]
sessionendstate <- S["End"]


########## Run Analysis ############

for (i in c(1:length(listofdatafolders))){
  lookup <- paste(datafoldersinproject1, datafoldersinproject2, projectdatafolder, listofdatafolders[i], sep = "/")
  
  datafilepaths <- list.files(path = lookup, pattern = ".csv")
  for (j in c(1:length(datafilepaths))) {
    
    # For each raw.txt file split up the data into individudal subjects .csv files for subsequent analysis
    folderpath <- here(datafoldersinproject1, datafoldersinproject2,projectdatafolder,listofdatafolders[i])
    filename <- datafilepaths[j]
    ## Run Function - N.B. Warnings will appear to tell you that the new directory for data storage already exists. Safe to ignore.
    coulbourn_processdata_Operant_SessionTimeBinAnalysis(filename,folderpath,S,timebase, timebinwidth, bin, sessionendstate)
  }
  
}



# Combine all data --------------------------------------------------------
#create list of all filenames
filestojoin <- "0"
for (i in c(1:length(listofdatafolders))){
  lookup <- paste(datafoldersinproject1, datafoldersinproject2, projectdatafolder, listofdatafolders[i],"Processed_TimeBin",  sep = "/")
  
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



# Recode COunterbalancing information -----------------------------------
## Solution: create a counterbalancing data frame and then left_join() with the rawdata to match all relevant rows on Subject[Make sure subject is labelled the same in both tables]
subject <- c("9_____",
             "10____",
             "11____",
             "12____",
             "13____",
             "14____",
             "34____",
             "35____",
             "36____",
             "37____",
             "38____",
             "39____",
             "15____",
             "16____",
             "40____",
             "41____")


counterbalancing <- c("A",
                      "B",
                      "C",
                      "D",
                      "A",
                      "B",
                      "A",
                      "B",
                      "C",
                      "D",
                      "A",
                      "B",
                      "C",
                      "D",
                      "C",
                      "D")
sex <- c("F",
         "F",
         "F",
         "F",
         "F",
         "F",
         "M",
         "M",
         "M",
         "M",
         "M",
         "M",
         "F",
         "F",
         "M",
         "M")

# Create counterbalancing lookup table
lookup_counterbalancing <- data.frame(subject, counterbalancing, sex)
# Combine with rawdata
rawdata <- left_join(rawdata, lookup_counterbalancing, by = "subject")


# Add counterbalancing associated with lever->light IDs

protocol <- c("36",
              "37",
              "38",
              "39",
              "40",
              "41",
              "42",
              "43",
              "44",
              "45",
              "46",
              "47")

Left_LeverCueID <- c("Click",
                     "Noise",
                     "Click",
                     "Tone",
                     "Click",
                     "Siren",
                     "Noise",
                     "Tone",
                     "Noise",
                     "Siren",
                     "Tone",
                     "Siren")

Right_LeverCueID <- c("Noise",
                      "Click",
                      "Tone",
                      "Click",
                      "Siren",
                      "Click",
                      "Tone",
                      "Noise",
                      "Siren",
                      "Noise",
                      "Siren",
                      "Tone")

# Create counterbalancing lookup table
lookup_counterbalancing <- data.frame(protocol, Left_LeverCueID, Right_LeverCueID)
# Combine with rawdata
## First convert Protocol variable to string for join method to work
rawdata <- rawdata %>% 
  mutate(protocol = as.character(protocol))

rawdata <- left_join(rawdata, lookup_counterbalancing, by = c("protocol"))



# Counterbalancing based on Pavlovian History

CS_LeftLeverName <- c("A++--",
             "B+-",
             "C++",
             "D+",
             "B+-",
             "C++",
             "D+",
             "A++--",
             "C++",
             "D+",
             "A++--",
             "B+-",
             "D+",
             "A++--",
             "B+-",
             "C++")

CS_RightLeverName <- c("A++--",
                      "B+-",
                      "C++",
                      "D+",
                      "B+-",
                      "C++",
                      "D+",
                      "A++--",
                      "C++",
                      "D+",
                      "A++--",
                      "B+-",
                      "D+",
                      "A++--",
                      "B+-",
                      "C++")

Left_LeverCueID <- c("Click",
              "Noise",
              "Tone",
              "Siren",
              "Click",
              "Noise",
              "Tone",
              "Siren",
              "Click",
              "Noise",
              "Tone",
              "Siren",
              "Click",
              "Noise",
              "Tone",
              "Siren")

Right_LeverCueID <- c("Click",
                     "Noise",
                     "Tone",
                     "Siren",
                     "Click",
                     "Noise",
                     "Tone",
                     "Siren",
                     "Click",
                     "Noise",
                     "Tone",
                     "Siren",
                     "Click",
                     "Noise",
                     "Tone",
                     "Siren")

counterbalancing<- c("A",
                     "A",
                     "A",
                     "A",
                     "B",
                     "B",
                     "B",
                     "B",
                     "C",
                     "C",
                     "C",
                     "C",
                     "D",
                     "D",
                     "D",
                     "D")



# Create counterbalancing lookup table
lookup_counterbalancing <- data.frame(counterbalancing, Left_LeverCueID, CS_LeftLeverName)
# Combine with rawdata
rawdata <- left_join(rawdata, lookup_counterbalancing, by = c("counterbalancing", "Left_LeverCueID"))

# Create counterbalancing lookup table
lookup_counterbalancing <- data.frame(counterbalancing, Right_LeverCueID, CS_RightLeverName)
# Combine with rawdata
rawdata <- left_join(rawdata, lookup_counterbalancing, by = c("counterbalancing", "Right_LeverCueID"))

# FInal COunterbalancing to define Test condition
TestCondition <- c("P50_HighVsLow",
                   "High_100Vs50",
                   "P100_HighVsLow",
                   "Low_100Vs50",
                   "P50_HighVsLow",
                   "High_100Vs50",
                   "P100_HighVsLow",
                   "Low_100Vs50")

CS_LeftLeverName <- c("A++--",
                      "A++--",
                      "C++",
                      "B+-",
                      "B+-",
                      "C++",
                      "D+",
                      "D+")

CS_RightLeverName <- c("B+-",
                       "C++",
                       "D+",
                       "D+",
                       "A++--",
                       "A++--",
                       "C++",
                       "B+-")

# Create counterbalancing lookup table
lookup_counterbalancing <- data.frame(TestCondition, CS_LeftLeverName, CS_RightLeverName)
# Combine with rawdata
rawdata <- left_join(rawdata, lookup_counterbalancing, by = c("CS_LeftLeverName", "CS_RightLeverName"))
# Calculate Session/Day number --------------------------------------------

rawdata <- rawdata %>%
  mutate(folder1 = folder) %>% 
  separate(folder1, c("Test","Day"))


# Save as CSV -------------------------------------------------------------

savefolderpath <- here("rawdata","Marios","2_ConditionedReinforcement","CombinedData")
savefilename <- "CRF_ProcessedData_CRFTests_withinSession1minBin.csv"
dir.create(savefolderpath)
write_csv(rawdata,here(savefolderpath,savefilename))

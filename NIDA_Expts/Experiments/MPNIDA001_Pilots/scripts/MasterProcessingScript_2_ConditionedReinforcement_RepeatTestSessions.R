# Load Packagaes ----------------------------------------------------------
## Packages for data organisation and plotting
library(tidyverse)
library(knitr)
library(data.table)
# Package for relative file paths
library(here)
# # Benchmark time of functions and analyze time profile of functions
# library(microbenchmark)
# library(profvis)
# Load Analysis Functions
source(here("scripts", "CoulbournAnalysisFunctions.R"))

# Packages for parallel computing siginificantly speeds things up
library(foreach)
library(doParallel)

numCores = 16
registerDoParallel(numCores)

# Identify files to analyze

## COllapses multiple subfolders if needed
### Project specific folder
projectdatafolder <- c("rawdata","Marios","2_ConditionedReinforcement")

## Final level of folders contianing the relevant .txt Coulbourn files
listofdatafolders <- c("RepeatTest1_Day24",
                       "RepeatTest2_Day24",
                       "RepeatTest3_Day25",
                       "RepeatTest4_Day25",
                       "RepeatTest5_Day26")



#  extract data filenames, only .txt --------------------------------------


Coulbourn_extractIndividualSubjectFiles(projectdatafolder, listofdatafolders)

#  extract processed data filenames, only .csv and put them into a --------



processdata <- function(projectdatafolder, listofdatafolders) {
  

  
  ########## Set parameters ############
  ## List of states
  S = c("ITI1" = 1,
        "BothLeversout1" = 2,
        "LeftReinforced1" = 3,
        "RightReinforced1" = 4,
        "ITI2_LeftOut" = 5,
        "LeftLeverOut" = 6,
        "LeftReinforced2" = 7,
        "ITI2_RightOut" = 8,
        "RightLeverOut" = 9,
        "RightReinforced2" = 10,
        "ITI3_PreChoiceTest" = 11,
        "ChoiceTest_BothLeversOut" = 12,
        "Click" = 13,
        "Noise" = 14,
        "Tone" = 15,
        "Siren" = 16,
        "End" = 17)
  
  ## Time base the linc was set to, in ms
  timebase = 20
  
  ## Time bins to analyze each state in, in s
  timebinwidth = 60
  
  ## Count the frequency and duration of these states
  bin = c(S["Click"], S["Noise"], S["Tone"], S["Siren"])
  
  ## State indicating the session has started - for variable start procedures 
  trialstartstate <- S["ChoiceTest_BothLeversOut"]
  
  ## state that should be considered the end of the session. For the absolute end of recording specify the number -1 [the state_ID of the finished state in Coulbourn]
  # sessionendstate <- S["End"]
  # SOme programs had to be stopped manually, use FIN state as end to avoid errors
  sessionendstate <- c(-1)
  
  
  
  
  for (i in c(1:length(listofdatafolders))){
    lookup <- paste(c(projectdatafolder, listofdatafolders[i]), collapse = "/")
    
    datafilepaths <- list.files(path = lookup, pattern = ".csv")
    foreach (j = c(1:length(datafilepaths))) %dopar% {
      library(here)
      library(tidyverse)
      library(knitr)
      library(data.table)
      source(here("scripts", "CoulbournAnalysisFunctions.R"))
      # For each raw.txt file split up the data into individual subjects .csv files for subsequent analysis
      folderpath <- here(paste(projectdatafolder, collapse = "/"),listofdatafolders[i])
      filename <- datafilepaths[j]
      ## Run Function - N.B. Warnings will appear to tell you that the new directory for data storage already exists. Safe to ignore.
      coulbourn_processdata_Operant_SessionTimeBinAnalysis_variablestart(filename,folderpath,S,timebase, timebinwidth, bin, trialstartstate, sessionendstate)
    }
    
  }
  
  
}

processdata(projectdatafolder, listofdatafolders)


# Combine all data --------------------------------------------------------
processedfoldername = "Processed_TimeBin"
#create list of all filenames
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

protocol <- c("63",
              "64",
              "65",
              "66",
              "67",
              "68",
              "69",
              "70",
              "71",
              "72",
              "73",
              "74")

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
savefilename <- "CRF_ProcessedData_CRFRTests_Repeats_withinSession1minBin.csv"
dir.create(savefolderpath, showWarnings = FALSE)
fwrite(rawdata,here(paste(c(savefolderpath, savefilename), collapse = "/")))


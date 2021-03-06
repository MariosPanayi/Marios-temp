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
projectdatafolder <- c("3_LeverPressingForLights")

## Final level of folders contianing the relevant .txt Coulbourn files
listofdatafolders <- c("LPL_Devaluation1_Test_Day13",
                       "LPL_Devaluation2_Test_Day14")


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
S = c("Test_NonReinforced" = 1,
      "ITI" = 2,
      "Test_Reinforced" = 3,
      "Flash" = 4,
      "Steady" = 5,
      "End" = 6)

## Time base the linc was set to, in ms
timebase = 20

## Time bins to analyze each state in, in s
timebinwidth = 1

## States to bin and which not to bin
nobin = c(S["End"], S["ITI"], S["Test_NonReinforced"])
bin = c( S["Flash"], S["Steady"])

## Identify state that trials start in, Here These are states that occur after a 'successful' operant response
trialstartstate = c(S["Test_Reinforced"])

## Time bin pre and post target states of interest, in seconds
prebintime <- 5
postbintime <- 5



########## Run Analysis ############

for (i in c(1:length(listofdatafolders))){
  lookup <- paste(datafoldersinproject1, datafoldersinproject2, projectdatafolder, listofdatafolders[i], sep = "/")
  
  datafilepaths <- list.files(path = lookup, pattern = ".csv")
  for (j in c(1:length(datafilepaths))) {
    
    # For each raw.txt file split up the data into individudal subjects .csv files for subsequent analysis
    folderpath <- here(datafoldersinproject1, datafoldersinproject2,projectdatafolder,listofdatafolders[i])
    filename <- datafilepaths[j]
    ## Run Function - N.B. Warnings will appear to tell you that the new directory for data storage already exists. Safe to ignore.
    coulbourn_processdata_Instrumental_PrePosttimebin(filename,folderpath,S,timebase, timebinwidth, nobin, bin, trialstartstate, prebintime, postbintime)
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
subject <- c("17____",
             "18____",
             "19____",
             "20____",
             "21____",
             "22____",
             "42____",
             "43____",
             "44____",
             "23____",
             "24____",
             "25____")



  counterbalancing <- c("A",
                        "B",
                        "A",
                        "B",
                        "A",
                        "B",
                        "A",
                        "B",
                        "A",
                        "A",
                        "B",
                        "B")
  
  Pavlovian_cbx <-  c("X",
                        "X",
                        "Y",
                        "Y",
                        "X",
                        "X",
                        "Y",
                        "Y",
                        "X",
                        "X",
                        "Y",
                        "Y")
  
  sex <- c("F",
           "F",
           "F",
           "F",
           "F",
           "F",
           "M",
           "M",
           "M",
           "F",
           "F",
           "F")
  




  
  # Create counterbalancing lookup table
lookup_counterbalancing <- data.frame(subject, counterbalancing, Pavlovian_cbx, sex)
  # Combine with rawdata
rawdata <- left_join(rawdata, lookup_counterbalancing, by = "subject")



# Add counterbalancing associated with lever->light IDs

counterbalancing <-  c("A",
                        "A",
                       "B",
                       "B")
Pavlovian_cbx <-  c("X",
                    "Y",
                    "X",
                    "Y")

FLash_leverCbx <-  c("Left",
                     "Left",
                     "Right",
                     "Right")

Steady_levercbx <-  c("Right",
                      "Right",
                      "Left",
                      "Left")

Flash_OutcomeID <- c("Chocolate",
                     "Banana",
                     "Chocolate",
                     "Banana")


Steady_OutcomeID <- c("Banana",
                     "Chocolate",
                     "Banana",
                     "Chocolate")


# Create counterbalancing lookup table
lookup_counterbalancing <- data.frame(counterbalancing, Pavlovian_cbx, FLash_leverCbx, Steady_levercbx, Flash_OutcomeID, Steady_OutcomeID)
# Combine with rawdata
rawdata <- left_join(rawdata, lookup_counterbalancing, by = c("counterbalancing", "Pavlovian_cbx"))


# Calculate Session/Day number --------------------------------------------

rawdata <- rawdata %>%
  mutate(folder1 = folder) %>% 
  separate(folder1, c(NA,"Stage","Schedule","Day"))


# Add counterbalancing of devalued outcome dependent on which test day it is

DevaluedOutcome <- c("Banana",
                      "Chocolate",
                      "Chocolate",
                      "Banana",
                      "Chocolate",
                      "Banana",
                      "Banana",
                      "Chocolate",
                      "Chocolate",
                      "Chocolate",
                      "Chocolate",
                      "Chocolate",
                     "Chocolate",
                     "Banana",
                     "Banana",
                     "Chocolate",
                     "Banana",
                     "Chocolate",
                     "Chocolate",
                     "Banana",
                     "Banana",
                     "Banana",
                     "Banana",
                     "Banana")

subject <- c("17____",
             "18____",
             "19____",
             "20____",
             "21____",
             "22____",
             "42____",
             "43____",
             "44____",
             "23____",
             "24____",
             "25____",
             "17____",
             "18____",
             "19____",
             "20____",
             "21____",
             "22____",
             "42____",
             "43____",
             "44____",
             "23____",
             "24____",
             "25____")

Day <- c("Day13",
"Day13",
"Day13",
"Day13",
"Day13",
"Day13",
"Day13",
"Day13",
"Day13",
"Day13",
"Day13",
"Day13",
"Day14",
"Day14",
"Day14",
"Day14",
"Day14",
"Day14",
"Day14",
"Day14",
"Day14",
"Day14",
"Day14",
"Day14")

# Create counterbalancing lookup table
lookup_counterbalancing <- data.frame(Day, subject, DevaluedOutcome)
# Combine with rawdata
rawdata <- left_join(rawdata, lookup_counterbalancing, by = c("Day", "subject"))


# Save as CSV -------------------------------------------------------------

savefolderpath <- here("rawdata","Marios","3_LeverPressingForLights","CombinedData")
savefilename <- "LPL_ProcessedData_DevlautionTest_WithinCue1minBins.csv"
dir.create(savefolderpath)
write_csv(rawdata,here(savefolderpath,savefilename))

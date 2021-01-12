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
listofdatafolders <- c("LPL_Acquisition_RR2_Day1",
                       "LPL_Acquisition_RR2_Day2",
                       "LPL_Acquisition_RR2_Day3",
                       "LPL_Acquisition_RR2_Day4")


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
S = c("IRI" = 1,
      "Flash" = 2,
      "Steady" = 3,
      "End" = 4)

## Time base the linc was set to, in ms
timebase = 20

## Time bins to analyze each state in, in s
timebinwidth = 600

## Count the frequency and duration of these states
bin = c(S["Flash"], S["Steady"])

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
lookup_counterbalancing <- data.frame(subject, counterbalancing, sex)
  # Combine with rawdata
rawdata <- left_join(rawdata, lookup_counterbalancing, by = "subject")


## Added later for completeness

subject <- c("17____",
             "18____",
             "19____",
             "20____",
             "21____",
             "22____",
             "23____",
             "24____",
             "25____",
             "42____",
             "43____",
             "44____")

Instrumental_cbx <- c("A",
                      "B",
                      "A",
                      "B",
                      "A",
                      "B",
                      "A",
                      "B",
                      "B",
                      "A",
                      "B",
                      "A")

Pavlovian_cbx <- c("X",
                   "X",
                   "Y",
                   "Y",
                   "X",
                   "X",
                   "X",
                   "Y",
                   "Y",
                   "Y",
                   "Y",
                   "X")

FLash_leverCbx <- c("Left",
                    "Right",
                    "Left",
                    "Right",
                    "Left",
                    "Right",
                    "Left",
                    "Right",
                    "Right",
                    "Left",
                    "Right",
                    "Left")

Steady_levercbx <- c("Right",
                     "Right",
                     "Left",
                     "Left",
                     "Right",
                     "Right",
                     "Right",
                     "Left",
                     "Left",
                     "Left",
                     "Left",
                     "Right")

Flash_OutcomeID <- c("Chocolate",
                     "Chocolate",
                     "Banana",
                     "Banana",
                     "Chocolate",
                     "Chocolate",
                     "Chocolate",
                     "Banana",
                     "Banana",
                     "Banana",
                     "Banana",
                     "Chocolate")

Steady_OutcomeID <- c("Banana",
                      "Banana",
                      "Chocolate",
                      "Chocolate",
                      "Banana",
                      "Banana",
                      "Banana",
                      "Chocolate",
                      "Chocolate",
                      "Chocolate",
                      "Chocolate",
                      "Banana")

DevaluedOutcome1 <- c("Banana",
                      "Chocolate",
                      "Chocolate",
                      "Banana",
                      "Chocolate",
                      "Banana",
                      "Chocolate",
                      "Chocolate",
                      "Chocolate",
                      "Banana",
                      "Chocolate",
                      "Chocolate")

# Create counterbalancing lookup table
lookup_counterbalancing <- data.frame(subject, Instrumental_cbx, Pavlovian_cbx, FLash_leverCbx, Steady_levercbx, Flash_OutcomeID, Steady_OutcomeID, DevaluedOutcome1)
# Combine with rawdata
rawdata <- left_join(rawdata, lookup_counterbalancing, by = "subject")

# Calculate Session/Day number --------------------------------------------

rawdata <- rawdata %>%
  mutate(folder1 = folder) %>% 
  separate(folder1, c(NA,"Stage","Schedule","Day"))


# Save as CSV -------------------------------------------------------------

savefolderpath <- here("rawdata","Marios","3_LeverPressingForLights","CombinedData")
savefilename <- "LPL_ProcessedData_WithinSession10minBins.csv"
dir.create(savefolderpath)
write_csv(rawdata,here(savefolderpath,savefilename))

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
projectdatafolder <- c("1_SpecificCI")

## Final level of folders contianing the relevant .txt Coulbourn files
listofdatafolders <- c("FeatureNegative_Stage2_Day5",
                       "FeatureNegative_Stage2_Day6",
                       "FeatureNegative_Stage2_Day7",
                       "FeatureNegative_Stage2_Day8",
                       "FeatureNegative_Stage2_Day9",
                       "FeatureNegative_Stage2_Day10",
                       "FeatureNegative_Stage2_Day11",
                       "FeatureNegative_Stage2_Day12")


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
      "PreCS" = 2,
      "Click" = 6,
      "Noise" = 7,
      "Tone" = 8,
      "Siren" = 9,
      "Pel_Banana" = 10,
      "Pel_Chocolate" = 11,
      "Click_Flash" = 12,
      "Noise_Steady" = 13,
      "End" = 14,
      "NoReward" = 15,
      "Click_Steady" = 16,
      "Noise_Flash" = 17)

## Time base the linc was set to, in ms
timebase = 20

## Time bins to analyze each state in, in s
timebinwidth = 1

## States to bin and which not to bin
nobin = c(S["ITI"], S["End"])
bin = c(S["PreCS"], S["Click"], S["Noise"], S["Tone"], S["Siren"], S["Pel_Banana"], S["Pel_Chocolate"], S["Click_Flash"], S["Noise_Steady"], S["NoReward"], S["Click_Steady"], S["Noise_Flash"])

## Identify state that trials start in, usually the ITI ;)
trialstartstate = S["ITI"]

########## Run Analysis ############

for (i in c(1:length(listofdatafolders))){
  lookup <- paste(datafoldersinproject1, datafoldersinproject2, projectdatafolder, listofdatafolders[i], sep = "/")
  
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
subject <- c("1_____",
             "2_____",
             "3_____",
             "4_____",
             "5_____",
             "6_____",
             "26____",
             "27____",
             "28____",
             "29____",
             "30____",
             "31____",
             "7_____",
             "8_____",
             "32____",
             "33____")


  counterbalancing <- c("AX",
                        "BX",
                        "CX",
                        "DX",
                        "AY",
                        "BY",
                        "AX",
                        "BX",
                        "CX",
                        "DX",
                        "AY",
                        "BY",
                        "CY",
                        "DY",
                        "CY",
                        "DY")
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


###

state_ID <- c("PreCS",
             "Click",
             "Noise",
             "Tone",
             "Siren",
             "Click_Flash",
             "Noise_Steady",
             "Click_Steady",
             "Noise_Flash",
             "Pel_Banana",
             "Pel_Chocolate",
             "NoReward")

Period <- c("Pre",
           "CS",
           "CS",
           "CS",
           "CS",
           "CS",
           "CS",
           "CS",
           "CS",
           "Post",
           "Post",
           "Post")

bin_state <- c(2,
              6,
              7,
              8,
              9,
              12,
              13,
              16,
              17,
              10,
              11,
              15)

# Create counterbalancing lookup table
lookup_stateIDs <- data.frame(bin_state, Period, state_ID)
# COmbine with rawdata
rawdata <- left_join(rawdata, lookup_stateIDs, by =c("bin_state"))

### 
CS_name <- c("A_O1",
             "B_O2",
             "C_O1",
             "D_O2",
             "AX_",
             "BY_",
             "A_O1",
             "B_O2",
             "C_O1",
             "D_O2",
             "AX_",
             "BY_",
             "A_O1",
             "B_O2",
             "D_O2",
             "C_O1",
             "AX_",
             "BY_",
             "A_O1",
             "B_O2",
             "D_O2",
             "C_O1",
             "AX_",
             "BY_",
             "B_O2",
             "A_O1",
             "C_O1",
             "D_O2",
             "BY_",
             "AX_",
             "B_O2",
             "A_O1",
             "C_O1",
             "D_O2",
             "BY_",
             "AX_",
             "B_O2",
             "A_O1",
             "D_O2",
             "C_O1",
             "BY_",
             "AX_",
             "B_O2",
             "A_O1",
             "D_O2",
             "C_O1",
             "BY_",
             "AX_")

state_ID <- c("Click",
              "Noise",
              "Tone",
              "Siren",
              "Click_Flash",
              "Noise_Steady",
              "Click",
              "Noise",
              "Tone",
              "Siren",
              "Click_Steady",
              "Noise_Flash",
              "Click",
              "Noise",
              "Tone",
              "Siren",
              "Click_Flash",
              "Noise_Steady",
              "Click",
              "Noise",
              "Tone",
              "Siren",
              "Click_Steady",
              "Noise_Flash",
              "Click",
              "Noise",
              "Tone",
              "Siren",
              "Click_Flash",
              "Noise_Steady",
              "Click",
              "Noise",
              "Tone",
              "Siren",
              "Click_Steady",
              "Noise_Flash",
              "Click",
              "Noise",
              "Tone",
              "Siren",
              "Click_Flash",
              "Noise_Steady",
              "Click",
              "Noise",
              "Tone",
              "Siren",
              "Click_Steady",
              "Noise_Flash")

  counterbalancing<- c("AX",
                       "AX",
                       "AX",
                       "AX",
                       "AX",
                       "AX",
                       "AY",
                       "AY",
                       "AY",
                       "AY",
                       "AY",
                       "AY",
                       "BX",
                       "BX",
                       "BX",
                       "BX",
                       "BX",
                       "BX",
                       "BY",
                       "BY",
                       "BY",
                       "BY",
                       "BY",
                       "BY",
                       "CX",
                       "CX",
                       "CX",
                       "CX",
                       "CX",
                       "CX",
                       "CY",
                       "CY",
                       "CY",
                       "CY",
                       "CY",
                       "CY",
                       "DX",
                       "DX",
                       "DX",
                       "DX",
                       "DX",
                       "DX",
                       "DY",
                       "DY",
                       "DY",
                       "DY",
                       "DY",
                       "DY")

# Create counterbalancing lookup table
lookup_CSname <- data.frame(counterbalancing, state_ID, CS_name)
# Combine with rawdata
rawdata <- left_join(rawdata, lookup_CSname, by = c("counterbalancing","state_ID"))


# Not the most satisfying solution, but will have to do since other methods don't appear to be working very well
rawdata <- rawdata %>% 
  group_by(subject, session, bin_trial) %>% 
  mutate(CS_name = unique(CS_name[Period == "CS"])[1]) %>% 
  ungroup()


# Calculate Session/Day number --------------------------------------------

rawdata <- rawdata %>%
  mutate(folder1 = folder) %>% 
  separate(folder1, c(NA,"Stage","Day"))



# Save as CSV -------------------------------------------------------------

savefolderpath <- here("rawdata","Marios","1_SpecificCI","CombinedData")
savefilename <- "CI_Stage2_ProcessedData_pertrial_1sbins.csv"
dir.create(savefolderpath)
write_csv(rawdata,here(savefolderpath,savefilename))

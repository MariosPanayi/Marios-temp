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
listofdatafolders <- c("RetardationTest_Stage5_Day16",
                       "RetardationTest_Stage5_Day17")


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
      "Flash" = 3,
      "Steady" = 4,
      "Pel_Banana" = 5,
      "Pel_Chocolate" = 7,
      "End" = 6)

## Time base the linc was set to, in ms
timebase = 20

## Time bins to analyze each state in, in s
timebinwidth = 1

## States to bin and which not to bin
nobin = c(S["ITI"], S["End"])
bin = c(S["PreCS"], S["Flash"], S["Steady"],S["Pel_Banana"], S["Pel_Chocolate"])

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
             "Flash",
             "Steady",
             "Pel_Banana",
             "Pel_Chocolate")

Period <- c("Pre",
           "CS",
           "CS",
           "Post",
           "Post")

bin_state <- c(2,
              3,
              4,
              5,
              7)

# Create counterbalancing lookup table
lookup_stateIDs <- data.frame(bin_state, Period, state_ID)
# COmbine with rawdata
rawdata <- left_join(rawdata, lookup_stateIDs, by =c("bin_state"))

# Not the most satisfying solution, but will have to do since other methods don't appear to be working very well
rawdata <- rawdata %>% 
  group_by(subject, session, bin_trial) %>% 
  mutate(CS_name = unique(CS_name[Period == "CS"])[1]) %>% 
  ungroup()


### 
CS <- c("X",
             "Y",
             "Y",
             "X",
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
             "Y",
             "X")

state_ID <- c("Flash",
              "Steady",
              "Flash",
              "Steady",
              "Flash",
              "Steady",
              "Flash",
              "Steady",
              "Flash",
              "Steady",
              "Flash",
              "Steady",
              "Flash",
              "Steady",
              "Flash",
              "Steady")

  counterbalancing<- c("AX",
                       "AX",
                       "AY",
                       "AY",
                       "BX",
                       "BX",
                       "BY",
                       "BY",
                       "CX",
                       "CX",
                       "CY",
                       "CY",
                       "DX",
                       "DX",
                       "DY",
                       "DY")

# Create counterbalancing lookup table
lookup_CSname <- data.frame(counterbalancing, state_ID, CS)
# Combine with rawdata
rawdata <- left_join(rawdata, lookup_CSname, by = c("counterbalancing","state_ID"))

US <- c("O1",
               "O2")
state_ID <- c("Pel_Banana",
              "Pel_Chocolate")

# Create counterbalancing lookup table
lookup_USname <- data.frame(US, state_ID)
# Combine with rawdata
rawdata <- left_join(rawdata, lookup_USname, by = c("state_ID"))


# Not the most satisfying solution, but will have to do since other methods don't appear to be working very well
rawdata <- rawdata %>% 
  group_by(subject, session, bin_trial) %>% 
  mutate(CS_name = paste(unique(CS[Period == "CS"])[1], unique(US[Period == "Post"])[1], sep = "_")) %>% 
  ungroup() %>% 
  select(-CS,-US)


# Calculate Session/Day number --------------------------------------------

rawdata <- rawdata %>%
  mutate(folder1 = folder) %>% 
  separate(folder1, c(NA,"Stage","Day"))



# Save as CSV -------------------------------------------------------------

savefolderpath <- here("rawdata","Marios","1_SpecificCI","CombinedData")
savefilename <- "CI_Stage5_ProcessedData_pertrial_1sbins.csv"
dir.create(savefolderpath)
write_csv(rawdata,here(savefolderpath,savefilename))

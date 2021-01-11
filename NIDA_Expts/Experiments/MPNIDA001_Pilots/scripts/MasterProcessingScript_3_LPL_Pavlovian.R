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
listofdatafolders <- c("LPL_Pavlovian_Acquisition_Day7",
                       "LPL_Pavlovian_Acquisition_Day8",
                       "LPL_Pavlovian_Acquisition_Day9",
                       "LPL_Pavlovian_Acquisition_Day10",
                       "LPL_Pavlovian_Acquisition_Day11",
                       "LPL_Pavlovian_Acquisition_Day12")


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
      "PreCS1" = 2,
      "PreCS2" = 3,
      "Flash" = 4,
      "Steady" = 5,
      "BananaPel" = 6,
      "ChocolatePel" = 7,
      "PostCS2" = 8,
      "End" = 9)

## Time base the linc was set to, in ms
timebase = 20

## Time bins to analyze each state in, in s
timebinwidth = 1

## States to bin and which not to bin
nobin = c(S["ITI"], S["End"])
bin = c(S["PreCS1"], S["PreCS2"], S["Flash"], S["Steady"], S["BananaPel"], S["ChocolatePel"], S["PostCS2"])

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


  counterbalancing <- c("X",
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
  
  sex <- c("Female",
             "Female",
             "Female",
             "Female",
             "Female",
             "Female",
             "Male",
             "Male",
             "Male",
             "Female",
             "Female",
             "Female")
  
  instrumentalcbx <- c("A",
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
  
  # Create counterbalancing lookup table
lookup_counterbalancing <- data.frame(subject, counterbalancing, sex, instrumentalcbx)
  # Combine with rawdata
rawdata <- left_join(rawdata, lookup_counterbalancing, by = "subject")


###
state_ID <- c("PreCS1",
              "PreCS2",
             "Flash",
             "Steady",
             "BananaPel",
             "ChocolatePel",
             "PostCS2")

Period <- c("Pre1",
           "Pre2",
           "CS",
           "CS",
           "Post1",
           "Post1",
           "Post2")

bin_state <- c(2,
              3,
              4,
              5,
              6,
              7,
              8)





# Create counterbalancing lookup table
lookup_stateIDs <- data.frame(bin_state, Period, state_ID)
# COmbine with rawdata
rawdata <- left_join(rawdata, lookup_stateIDs, by =c("bin_state"))

###
CS_name <- c("Flash",
             "Steady",
             "Flash",
             "Steady",
             "Flash",
             "Steady",
             "Flash",
             "Steady")

Lever_name <- c("Left",
             "Right",
             "Left",
             "Right",
             "Right",
             "Left",
             "Right",
             "Left")

state_ID <- c("Flash",
              "Steady",
              "Flash",
              "Steady",
              "Flash",
              "Steady",
              "Flash",
              "Steady")

outcome_ID <- c("Banana",
              "Chocolate",
              "Chocolate",
              "Banana",
              "Banana",
              "Chocolate",
              "Chocolate",
              "Banana")

instrumentalcbx <- c("A",
                     "A",
                     "A",
                     "A",
                     "B",
                     "B",
                     "B",
                     "B")

counterbalancing<- c("X",
                     "X",
                     "Y",
                     "Y",
                     "X",
                     "X",
                     "Y",
                     "Y")

# Create counterbalancing lookup table
lookup_CSname <- data.frame(counterbalancing, state_ID, CS_name, instrumentalcbx,Lever_name, outcome_ID)
# Combine with rawdata
rawdata <- left_join(rawdata, lookup_CSname, by = c("counterbalancing","instrumentalcbx","state_ID"))


# Not the most satisfying solution, but will have to do since other methods don't appear to be working very well
rawdata <- rawdata %>%
  group_by(subject, session, bin_trial) %>%
  mutate(CS_name = unique(CS_name[Period == "CS"])[1],
         outcome_ID = unique(outcome_ID[Period == "CS"])[1],
         Lever_name = unique(Lever_name[Period == "CS"])[1]) %>%
  ungroup()


# Calculate Session/Day number --------------------------------------------

rawdata <- rawdata %>%
  mutate(folder1 = folder) %>% 
  separate(folder1, c(NA, "Stage",NA, "Day"))



# Save as CSV -------------------------------------------------------------

savefolderpath <- here("rawdata","Marios","3_LeverPressingForLights","CombinedData")
savefilename <- "LPL_Pavlovian_ProcessedData_pertrial_1sbins.csv"
dir.create(savefolderpath)
write_csv(rawdata,here(savefolderpath,savefilename))

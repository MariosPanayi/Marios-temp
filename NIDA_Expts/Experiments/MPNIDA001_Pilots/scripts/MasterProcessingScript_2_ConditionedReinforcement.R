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
                       "Acquisition_Day3",
                       "Acquisition_Day4",
                       "Acquisition_Day5",
                       "Acquisition_Day6",
                       "Acquisition_Day7")


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

  # Create counterbalancing lookup table
lookup_counterbalancing <- data.frame(subject, counterbalancing)
  # Combine with rawdata
rawdata <- left_join(rawdata, lookup_counterbalancing, by = "subject")


###
state_ID <- c("PreCS",
             "Click",
             "Noise",
             "Tone",
             "Siren",
             "Pelletx1",
             "Pelletx2",
             "Pelletx0")

Period <- c("Pre",
           "CS",
           "CS",
           "CS",
           "CS",
           "Post",
           "Post",
           "Post")

bin_state <- c(2,
              3,
              4,
              5,
              6,
              7,
              8,
              9)
# Create counterbalancing lookup table
lookup_stateIDs <- data.frame(bin_state, Period, state_ID)
# COmbine with rawdata
rawdata <- left_join(rawdata, lookup_stateIDs, by =c("bin_state"))

### 
CS_name <- c("A++--",
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

state_ID <- c("Click",
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
  separate(folder1, c("Stage","Day"))



# Save as CSV -------------------------------------------------------------

savefolderpath <- here("rawdata","Marios","2_ConditionedReinforcement","CombinedData")
savefilename <- "CRF_ProcessedData_pertrial_1sbins.csv"
dir.create(savefolderpath)
write_csv(rawdata,here(savefolderpath,savefilename))

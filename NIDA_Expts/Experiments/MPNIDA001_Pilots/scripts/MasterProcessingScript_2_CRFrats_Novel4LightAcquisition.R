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
listofdatafolders <- c("NovelLightCues_Acquisition_Day1",
                       "NovelLightCues_Acquisition_Day2")


#  extract data filenames, only .txt --------------------------------------


Coulbourn_extractIndividualSubjectFiles(projectdatafolder, listofdatafolders)

#  extract processed data filenames, only .csv and put them into a --------

########## Set parameters ############

processdata <- function(projectdatafolder, listofdatafolders) {
  
  ## List of states
  S = c("ITI" = 1,
        "PreCS" = 2,
        "Flash" = 3,
        "Steady" = 4,
        "Pulse" = 5,
        "Oscillate" = 6,
        "Pelletx2" = 8,
        "Pelletx0" = 9,
        "End" = 10)
  
  ## Time base the linc was set to, in ms
  timebase = 20
  
  ## Time bins to analyze each state in, in s
  timebinwidth = 1
  
  ## States to bin and which not to bin
  nobin = c(S["ITI"], S["End"])
  bin = c(S["PreCS"], S["Flash"], S["Steady"],S["Pulse"], S["Oscillate"], S["Pelletx2"], S["Pelletx0"])
  
  ## Identify state that trials start in, usually the ITI ;)
  trialstartstate = S["ITI"]
  
  ########## Run Analysis ############
  
  
  
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
      coulbourn_processdata_Pavlovian_timebin(filename,folderpath,S,timebase, timebinwidth, nobin, bin, trialstartstate)
    }
    
  }
  
  
}

processdata(projectdatafolder, listofdatafolders)

# Combine all data --------------------------------------------------------
#create list of all filenames

processedfoldername = "Processed_TimeBin"
rawdata <- Coulbourn_Joinprocesseddata(projectdatafolder, listofdatafolders, processedfoldername)

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

###

state_ID <- c("PreCS",
             "Flash",
             "Steady",
             "Pulse",
             "Oscillate",
             "Pelletx2",
             "Pelletx0")


Period <- c("Pre",
           "CS",
           "CS",
           "CS",
           "CS",
           "Post",
           "Post")

bin_state <- c(2,
              3,
              4,
              5,
              6,
              8,
              9)

# Create counterbalancing lookup table
lookup_stateIDs <- data.table(bin_state, Period, state_ID)
# COmbine with rawdata
rawdata <- left_join(rawdata, lookup_stateIDs, by =c("bin_state"))

# Not the most satisfying solution, but will have to do since other methods don't appear to be working very well
rawdata <- rawdata %>% 
  group_by(subject, session, bin_trial) %>% 
  mutate(CS_name = unique(state_ID[Period == "CS"])[1]) %>% 
  ungroup()


###
CS <- c("A",
        "B",
        "B",
        "A",
        "A",
        "B",
        "B",
        "A",
        "A",
        "B",
        "B",
        "A",
        "A",
        "B",
        "B",
        "A")

 
state_ID <- c("Flash",
              "Steady",
              "Pulse",
              "Oscillate",
              "Flash",
              "Steady",
              "Pulse",
              "Oscillate",
              "Flash",
              "Steady",
              "Pulse",
              "Oscillate",
              "Flash",
              "Steady",
              "Pulse",
              "Oscillate")

  counterbalancing<-  c("A",
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
lookup_CSname <- data.table(counterbalancing, state_ID, CS)
# Combine with rawdata
rawdata <- left_join(rawdata, lookup_CSname, by = c("counterbalancing","state_ID"))

US <- c("+",
        "-")
state_ID <- c("Pelletx2",
              "Pelletx0")


# Create counterbalancing lookup table
lookup_USname <- data.table(US, state_ID)
# Combine with rawdata
rawdata <- left_join(rawdata, lookup_USname, by = c("state_ID"))


# Not the most satisfying solution, but will have to do since other methods don't appear to be working very well
rawdata <- rawdata %>%
  group_by(subject, session, bin_trial) %>%
  mutate(CS_name = paste(unique(CS[Period == "CS"])[1], unique(US[Period == "Post"])[1], sep = "")) %>%
  ungroup() %>%
  select(-CS,-US)


# Calculate Session/Day number --------------------------------------------

rawdata <- rawdata %>%
  mutate(folder1 = folder) %>% 
  separate(folder1, c(NA,"Stage","Day"))



# Save as CSV -------------------------------------------------------------

savefolderpath <- here("rawdata","Marios","2_ConditionedReinforcement","CombinedData")
savefilename <- "CRF_NovelLightAcquisition_ProcessedData_pertrial_1sbins.csv"
dir.create(savefolderpath, showWarnings = FALSE)
fwrite(rawdata,here(paste(c(savefolderpath, savefilename), collapse = "/")))

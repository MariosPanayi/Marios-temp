# Load Packagaes ----------------------------------------------------------
## Packages for data organisation and plotting
library(tidyverse)
library(knitr)
library(data.table)
# Package for relative file paths
library(here)
# Benchmark time of functions
library(microbenchmark)
# Load Analysis Functions
source(here("scripts", "CoulbournAnalysisFunctions.R"))

# Packages for parallel computing
library(foreach)
library(doParallel)

numCores = 16
registerDoParallel(numCores)

# Identify files to analyze

## COllapses multiple subfolders if needed
### Project specific folder
projectdatafolder <- c("rawdata","CRF")

## Final level of folders containing the relevant .txt Coulbourn files
listofdatafolders <- c("CRF_Acq_Stage1_Day1",
                       "CRF_Acq_Stage1_Day2")


#  extract data filenames, only .txt --------------------------------------



Coulbourn_extractIndividualSubjectFiles(projectdatafolder, listofdatafolders)

#  extract processed data filenames, only .csv and put them into a --------

processdata <- function(projectdatafolder, listofdatafolders) {
########## Set parameters ############

## List of states
S = c("ITI" = 1,
      "PreCS" = 2,
      "Click" = 3,
      "Noise" = 4,
      "Tone" = 5,
      "Siren" = 6,
      "Pelletx1" = 7,
      "Pelletx3" = 8,
      "Pelletx0" = 9,
      "End" = 10)

## Time base the linc was set to, in ms
timebase = 20

## Time bins to analyze each state in, in s
timebinwidth = 1

## States to bin and which not to bin
nobin = c(S["ITI"], S["End"])
bin = c(S["PreCS"], S["Click"], S["Noise"], S["Tone"], S["Siren"], S["Pelletx1"], S["Pelletx3"], S["Pelletx0"])

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
    data
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
subject <- c("25____",
             "26____",
             "27____",
             "28____",
             "29____",
             "30____",
             "31____",
             "32____",
             "33____",
             "34____",
             "35____",
             "36____",
             "37____",
             "38____",
             "39____",
             "40____",
             "41____",
             "42____",
             "43____",
             "44____",
             "45____",
             "46____",
             "47____",
             "48____")


  counterbalancing <- c("A",
                        "B",
                        "C",
                        "D",
                        "A",
                        "B",
                        "C",
                        "D",
                        "A",
                        "B",
                        "C",
                        "D",
                        "D",
                        "C",
                        "B",
                        "A",
                        "D",
                        "C",
                        "B",
                        "A",
                        "D",
                        "C",
                        "B",
                        "A")

  # Create counterbalancing lookup table
lookup_counterbalancing <- data.table(subject, counterbalancing)
  # Combine with rawdata
rawdata <- left_join(rawdata, lookup_counterbalancing, by = "subject")


###
state_ID <- c("PreCS",
             "Click",
             "Noise",
             "Tone",
             "Siren",
             "Pelletx1",
             "Pelletx3",
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
lookup_stateIDs <- data.table(bin_state, Period, state_ID)
# COmbine with rawdata
rawdata <- left_join(rawdata, lookup_stateIDs, by =c("bin_state"))

### 
CS_name <- c("A+++/---",
             "B+/-",
             "D+",
             "C+++",
             "B+/-",
             "C+++",
             "A+++/---",
             "D+",
             "C+++",
             "D+",
             "B+/-",
             "A+++/---",
             "D+",
             "A+++/---",
             "C+++",
             "B+/-")

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
lookup_CSname <- data.table(counterbalancing, state_ID, CS_name)
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
  separate(folder1, c(NA, NA, "Stage","Day"))



# Save as CSV -------------------------------------------------------------

savefolderpath <- here("rawdata","CRF","CombinedData")
savefilename <- "CRF_ProcessedData_pertrial_1sbins.csv"
dir.create(savefolderpath, showWarnings = FALSE)
fwrite(rawdata,here(paste(c(savefolderpath, savefilename), collapse = "/")))

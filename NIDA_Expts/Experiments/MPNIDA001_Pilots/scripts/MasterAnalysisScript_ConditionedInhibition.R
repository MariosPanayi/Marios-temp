library(here)

# Load Analysis Functions
source(here("scripts", "CoulbournAnalysisFunctions.R"))


# Identify files to analyse

# Can't use here() function effectively here, so have to create relative file paths
## COllapses multiple subfolders if needed
datafoldersinproject <- c("rawdata", "Marios")
datafoldersinproject <- paste(datafoldersinproject, collapse = "/")
## Project specific folder
projectdatafolder <- c("2_ConditionedReinforcement")
projectdatafolder <- paste(projectdatafolder, collapse = "/")
## Final level of folders contianing the relevant .txt Coulbourn files
listofdatafolders <- c("Acquisition_Day1",
                       "Acquisition_Day2",
                       "Acquisition_Day3")


# extract data filenames, only .txt
for (i in c(1:length(listofdatafolders))){
lookup <- paste(datafoldersinproject, projectdatafolder, listofdatafolders[i], sep = "/")

  list.files(path = lookup, pattern = ".txt")
  
}

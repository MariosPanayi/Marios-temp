# Title: CoulbournDataExtraction
## Author: Marios Panayi
## Date: 16-DEC-2020
## Purpose: Extract Data from Raw COUlbourn Files
## Notes: Data from NIDA Pilots
# Load Packagaes ----------------------------------------------------------
## Packages for data organisation and plotting
library(tidyverse)
library(knitr)
# Package for relative file paths
library(here)
# library(ggpubr)
library(cowplot)
library(ggsignif)
library(patchwork)
library(RColorBrewer)
## Packages for Data analysis
library(afex)
afex_options(emmeans_model = "multivariate")# use multivariate model for all follow-up tests.
library(emmeans)
# install.packages("devtools")
# devtools::install_github("crsh/papaja")
# library(papaja)
# library(knitr)
# remotes::install_github("noamross/redoc")
# library(redoc)


# Read in data file -------------------------------------------------------

filename <- "CRF_MagTrain_Sess1.txt"
folderpath <- here("rawdata", "Marios",'2_ConditionedReinforcement')
filepath <- here(folderpath, filename)
# Coulbourn files are tab separated value .txt files
rawdata <- read_tsv(filepath)

# Clean up column naming conventions - Assumes 4 response options on Coulbourn setup
names(rawdata)[12] <- "A1_On"
names(rawdata)[13] <- "A2_On"
names(rawdata)[14] <- "A3_On"
names(rawdata)[15] <- "A4_On"
names(rawdata)[16] <- "A1_Off"
names(rawdata)[17] <- "A2_Off"
names(rawdata)[18] <- "A3_Off"
names(rawdata)[19] <- "A4_Off"


# Separate subjects -------------------------------------------------------
# Coulbourn will put all the data you have asked for in a long format stack with subjects changing after a variable number of columns
## Note that if you try and extract by identifying unique subjects you might accidentally ignore that a subject was run multiple times
## Instead, Coulbourn timetsamps always start at t = 0, so this can be used to identify the indices of every new subject



# Start and end indices of each subject
start = which(rawdata$Time == 0)
end = start -1
end = end[-1]
end = c(end, nrow(rawdata))

# Grab filename for naming convention and combine with subject number
prefix <- strsplit(filename, ".txt")
prefix <- prefix[[1]]
prefix <- str_replace_all(prefix, "_", "")

# for each subject 
for (i in c(1:length(start))) {
 temp <- rawdata[start[i]:end[i], ]
  # save temp data with appropriate filename
  # Clean SubjNumber
  SubjectNum = paste(c("Rat", temp$Subject[1]), collapse = "")
  SubjectNum = str_replace_all(SubjectNum, "_", "")
  
  RunNumber = paste(c("Run", temp$Run[1]), collapse = "")
  # Clean RUn number
  tempfilename <- paste(c(prefix, SubjectNum,RunNumber), collapse = "_")
  tempfilename <- paste(tempfilename, ".csv", sep = "")
  
  filepath <- here(folderpath, tempfilename)
 write_csv(temp, filepath)
  
}


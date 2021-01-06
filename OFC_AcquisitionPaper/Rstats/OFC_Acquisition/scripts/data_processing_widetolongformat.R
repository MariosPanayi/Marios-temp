## Packages for data organisation and plotting
library(tidyverse)
#library(ggpubr)
library(cowplot)
################################################################################
## Packages for Data analysis
library(afex)
afex_options(emmeans_model = "multivariate") # use multivariate model for all follow-up tests.
library(emmeans)
# install.packages("devtools")
# devtools::install_github("crsh/papaja")
library(papaja)
library(knitr)
library(here)

#####
# Script gets wide format raw data and converts it into long format.
# Data resaved in long format, .csv file
# only need to run once
#####


# Get all wide format files to process
# list.files(here('data','wideformat'))

#### "Expt1_Acquisition_GeneralSatiety_Data.csv" ----
full_data_wide <- read_csv(here('data','wideformat',"Expt1_Acquisition_GeneralSatiety_Data.csv"))

# make long format and split up the coding variable into unique variables
full_data_long <- full_data_wide %>% 
  pivot_longer(3:11, names_to = "IV", values_to = "MagEntries") %>% 
  mutate(IV = str_replace(IV, "CS_Pre", "CSPre"),
         IV = str_replace(IV, "Day21", "Day21_Hungry"),
         IV = str_replace(IV, "Day23", "Day23_Hungry")) %>% 
  separate(IV, c("Period", "Day", "Hunger")) %>% 
             mutate(Day = str_replace(Day, "y", "y "))

#Save long format data
write_csv(full_data_long,here('data',"Expt1_Acquisition_GeneralSatiety_Data.csv"))



#### "Expt1_Combined_Acquisition_Block3Days_Data.csv"       ----
full_data_wide <- read_csv(here('data','wideformat',"Expt1_Combined_Acquisition_Block3Days_Data.csv"))

# make long format and split up the coding variable into unique variables
full_data_long <- full_data_wide %>% 
  pivot_longer(4:24, names_to = "IV", values_to = "MagEntries") %>% 
  mutate(IV = str_replace(IV, "CS_Pre", "CSPre")) %>% 
  separate(IV, c("Stage", "Period", "Day")) %>% 
  mutate(Day = str_replace(Day, "y", "y "))

#Save long format data
write_csv(full_data_long,here('data',"Expt1_Combined_Acquisition_Block3Days_Data.csv"))


#### "Expt1_Combined_Acquisition_Data.csv"   ----

full_data_wide <- read_csv(here('data','wideformat',"Expt1_Combined_Acquisition_Data.csv"))

# make long format and split up the coding variable into unique variables
full_data_long <- full_data_wide %>% 
  pivot_longer(4:45, names_to = "IV", values_to = "MagEntries") %>% 
  mutate(IV = str_replace(IV, "CS_Pre", "CSPre")) %>% 
  separate(IV, c("Stage", "Period", "Day")) %>% 
  mutate(Day = str_replace(Day, "y", "y "))

#Save long format data
write_csv(full_data_long,here('data',"Expt1_Combined_Acquisition_Data.csv"))


#### "Expt1_Combined_LocomotorActivity.csv"  ----      
full_data_wide <- read_csv(here('data','wideformat',"Expt1_Combined_LocomotorActivity.csv"))

# make long format and split up the coding variable into unique variables
full_data_long <- full_data_wide %>% 
  pivot_longer(4:5, names_to = "Time", values_to = "Beam breaks") %>% 
  mutate(Time = str_replace(Time, "FirstHalf", "1"),
         Time = str_replace(Time, "SecondHalf", "2"))

#Save long format data
write_csv(full_data_long,here('data',"Expt1_Combined_Locomotor_Data.csv"))


#### "Expt1_DevaluationTest_data.csv"                       ----
full_data_wide <- read_csv(here('data','wideformat',"Expt1_DevaluationTest_data.csv"))

# make long format and split up the coding variable into unique variables
full_data_long <- full_data_wide %>% 
  pivot_longer(3:36, names_to = "IV", values_to = "MagEntries") %>% 
  mutate(IV = str_replace(IV, "TasteAversion_Saline1", "TasteAversion_Saline_Pairing1"),
         IV = str_replace(IV, "TasteAversion_Saline2", "TasteAversion_Saline_Pairing2"),
         IV = str_replace(IV, "TasteAversion_LiCl1", "TasteAversion_LiCl_Pairing1"),
         IV = str_replace(IV, "TasteAversion_LiCl2", "TasteAversion_LiCl_Pairing2")) %>% 
  separate(IV, c("Stage", "Condition", "Day")) %>% 
  mutate(Day = str_replace(Day, "Day", "Day "))


#Save long format data
write_csv(full_data_long,here('data',"Expt1_DevaluationTest_data.csv"))


#### "Expt1_Satiety_Extinction_PerTrial.csv"                ----
full_data_wide <- read_csv(here('data','wideformat',"Expt1_Satiety_Extinction_PerTrial.csv"))

# make long format and split up the coding variable into unique variables
full_data_long <- full_data_wide %>% 
  pivot_longer(3:50, names_to = "IV", values_to = "MagEntries") %>% 
  separate(IV, c("Stage", "Period", "Trial")) 

#Save long format data
write_csv(full_data_long,here('data',"Expt1_Satiety_Extinction_PerTrial.csv"))



#### "Expt1_Satiety_PerTrial.csv"    ----
full_data_wide <- read_csv(here('data','wideformat',"Expt1_Satiety_PerTrial.csv"))

# make long format and split up the coding variable into unique variables
full_data_long <- full_data_wide %>% 
  pivot_longer(3:34, names_to = "IV", values_to = "MagEntries") %>% 
  separate(IV, c("Day", "Period", "Trial")) %>% 
  mutate(Trial = str_replace(Trial, "Trial", ""))

#Save long format data
write_csv(full_data_long,here('data',"Expt1_Satiety_PerTrial.csv"))


##################

#### "Expt2_PostTrainingInfusion_Acquisition_Data.csv"      ----
full_data_wide <- read_csv(here('data','wideformat',"Expt2_PostTrainingInfusion_Acquisition_Data.csv"))

# make long format and split up the coding variable into unique variables
full_data_long <- full_data_wide %>% 
  pivot_longer(3:53, names_to = "IV", values_to = "MagEntries") %>% 
  separate(IV, c("Period", "Stage", "Day")) %>% 
  mutate(Period = str_replace(Period, "CSminusPreCS", "CSPre"))

#Save long format data
write_csv(full_data_long,here('data',"Expt2_PostTrainingInfusion_Acquisition_Data.csv"))



#### "Expt3_PostTrainingLesion_Acquisition_Data.csv"        ----
full_data_wide <- read_csv(here('data','wideformat',"Expt3_PostTrainingLesion_Acquisition_Data.csv"))

# make long format and split up the coding variable into unique variables
full_data_long <- full_data_wide %>% 
  pivot_longer(3:20, names_to = "IV", values_to = "MagEntries") %>% 
  mutate(IV = str_replace(IV, "CS_PreCS", "CSPre")) %>% 
  separate(IV, c("Stage", "Period", "DayBlock"))

#Save long format data
write_csv(full_data_long,here('data',"Expt3_PostTrainingLesion_Acquisition_Data.csv"))


#### "Expt4_PostTrainingInfusion_Early_Acquisition_Data.csv"----
full_data_wide <- read_csv(here('data','wideformat',"Expt4_PostTrainingInfusion_Early_Acquisition_Data.csv"))

# make long format and split up the coding variable into unique variables
full_data_long <- full_data_wide %>% 
  pivot_longer(4:33, names_to = "IV", values_to = "MagEntries") %>% 
  mutate(IV = str_replace(IV, "CS_Pre", "CSPre")) %>% 
  separate(IV, c("Period", "Day"))

#Save long format data
write_csv(full_data_long,here('data',"Expt4_PostTrainingInfusion_Early_Acquisition_Data.csv"))

#### "Expt5_Blocking_Stage1_Data.csv"                       ----
full_data_wide <- read_csv(here('data','wideformat',"Expt5_Blocking_Stage1_Data.csv"))

# make long format and split up the coding variable into unique variables
full_data_long <- full_data_wide %>% 
  pivot_longer(3:32, names_to = "IV", values_to = "MagEntries") %>% 
  mutate(IV = str_replace(IV, "Stage1_Day1", "Stage1_NoInfusion_Day1"),
         IV = str_replace(IV, "Stage1_Day2", "Stage1_NoInfusion_Day2"),
         IV = str_replace(IV, "Stage1_Day3", "Stage1_NoInfusion_Day3"),
         IV = str_replace(IV, "Stage1_Day4", "Stage1_NoInfusion_Day4"),
         IV = str_replace(IV, "CS_PreCS", "CSPre")) %>% 
  separate(IV, c("Period","Cue","Stage", "Infusion", "Day"))

#Save long format data
write_csv(full_data_long,here('data',"Expt5_Blocking_Stage1_Data.csv"))


#### "Expt5_Blocking_Stage2_Test_ReAcquisition_Data.csv"    ----
full_data_wide <- read_csv(here('data','wideformat',"Expt5_Blocking_Stage2_Test_ReAcquisition_Data.csv"))

# make long format and split up the coding variable into unique variables
full_data_long <- full_data_wide %>% 
  pivot_longer(3:15, names_to = "IV", values_to = "MagEntries") %>% 
  mutate(IV = str_replace(IV, "Test_CueB", "Test_CueB_Day1516"),
         IV = str_replace(IV, "Test_CueD", "Test_CueD_Day1516"),
         IV = str_replace(IV, "Cue", "")) %>% 
  separate(IV, c("Period","Cue", "Day"))


#Save long format data
write_csv(full_data_long,here('data',"Expt5_Blocking_Stage2_Test_ReAcquisition_Data.csv"))



#### "Expt6_RelativeValue_Acquisition_Data.csv"             ----
full_data_wide <- read_csv(here('data','wideformat',"Expt6_RelativeValue_Acquisition_Data.csv"))

# make long format and split up the coding variable into unique variables
full_data_long <- full_data_wide %>% 
  pivot_longer(2:73, names_to = "IV", values_to = "MagEntries") %>% 
  separate(IV, c("Period","Stage", "Magazine", "Probability"))


#Save long format data
write_csv(full_data_long,here('data',"Expt6_RelativeValue_Acquisition_Data.csv"))


#### "Expt6_RelativeValue_Infusion_Data.csv"  ----
full_data_wide <- read_csv(here('data','wideformat',"Expt6_RelativeValue_Infusion_Data.csv"))

# make long format and split up the coding variable into unique variables
full_data_long <- full_data_wide %>% 
  pivot_longer(2:25, names_to = "IV", values_to = "MagEntries") %>% 
  mutate(IV = str_replace(IV, "Muscimol", "Musscimol_")) %>% 
  separate(IV, c("Drug", "Period", "Probability", "Magazine"))


#Save long format data
write_csv(full_data_long,here('data',"Expt6_RelativeValue_Infusion_Data.csv"))





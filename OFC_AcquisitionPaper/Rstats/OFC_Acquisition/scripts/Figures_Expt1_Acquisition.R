##### Load relevant packages ----
## Packages for data organisation and plotting
library(tidyverse)
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

#### Plot functions ====
# modified from from:
# https://stackoverflow.com/questions/39071002/moving-x-or-y-axis-together-with-tick-labels-to-the-middle-of-a-single-ggplot-n
# Learn more here
# https://www.stat.auckland.ac.nz/~paul/useR2015-grid/grid-slides.html
# https://bookdown.org/rdpeng/RProgDA/the-grid-package.html#grobs

shift_xaxis_facet <- function(p, y=0){
  g <- ggplotGrob(p)
  dummy <- data.frame(y=y)
  # Identify axis elements - use this to find and move other elements
  ax <- g[["grobs"]][g$layout$name == "axis-b-1-1"][[1]]
  # plot grid as an annotation
  # grobTree combines all the elements of the axis into a single grob (vp = vertical position)
  # Viewport defines a rectangular window  
  p + annotation_custom(grid::grobTree(ax, vp = grid::viewport(y=1, height=sum(ax$height))), 
                        ymax=y, ymin=y) +
    geom_hline(aes(yintercept=y), data = dummy) +
    theme(axis.text.x = element_blank(), 
          axis.ticks.x = element_blank())
  
}

shift_xaxis <- function(p, y=0){
  g <- ggplotGrob(p)
  dummy <- data.frame(y=y)
  # Identify axis elements - use this to find and move other elements
  ax <- g[["grobs"]][g$layout$name == "axis-b"][[1]]
  # plot grid as an annotation
  # grobTree combines all the elements of the axis into a single grob (vp = vertical position)
  # Viewport defines a rectangular window  
  p + annotation_custom(grid::grobTree(ax, vp = grid::viewport(y=1, height=sum(ax$height))), 
                        ymax=y, ymin=y) +
    geom_hline(aes(yintercept=y), data = dummy) +
    theme(axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank())
  
}




#### Expt 1 Acquisition Pre Lesion: Acquisition Block 3 ----
full_data <- read_csv(here("data","Expt1_Combined_Acquisition_Block3Days_Data.csv"))

full_data <- full_data %>% 
  mutate(Group = str_replace(Group, "OFC", "Lesion"),
         Day = str_replace(Day, "Day ", "")) %>% 
         rename(Block = Day)

levelorder <- c("Sham", "Lesion")
full_data$Group <- fct_relevel(as.factor(full_data$Group), levelorder) 


data_Expt1_PreLesion_AcquisitionBlock3 <- full_data

## Optional Acquisition data per day isntead of blocks of 3 days
# read_csv(here("data","Expt1_Combined_Acquisition_Data.csv"))

#### Expt 1 Acquisition Pre Lesion: Locomotor  ----
full_data <- read_csv(here('data',"Expt1_Combined_Locomotor_Data.csv"))

levelorder <- c("Sham", "Lesion")
full_data$Group <- fct_relevel(as.factor(full_data$Group), levelorder)

data_Expt1_PreLesion_Locomotor <- full_data


#### Expt 1 Acquisition Pre Lesion: General Satiety  ----
full_data <- read_csv(here("data","Expt1_Acquisition_GeneralSatiety_Data.csv"))

full_data <- full_data %>% 
  mutate(Group = str_replace(Group, "OFC", "Lesion"))%>% 
  unite(GroupHungerID, Group, Hunger, sep = ": ", remove = FALSE) 

levelorder <- c("Sham", "Lesion")
full_data$Group <- fct_relevel(as.factor(full_data$Group), levelorder)

levelorder <- c("Satiety", "Hungry")
full_data$Hunger <- fct_relevel(as.factor(full_data$Hunger), levelorder)

levelorder <- c("Sham: Satiety", "Sham: Hungry", "Lesion: Satiety", "Lesion: Hungry")
full_data$GroupHungerID <- fct_relevel(as.factor(full_data$GroupHungerID), levelorder)

data_Expt1_PreLesion_Satiety <- full_data


#### Expt 1 Acquisition Pre Lesion: General Satiety  Per trial  ----
full_data <- read_csv(here("data","Expt1_Satiety_PerTrial.csv"))

#Add in CS-PreCS measure that wasn't calculated earlier
full_data <- full_data %>% 
  pivot_wider(names_from = Period, values_from = MagEntries) %>% 
  mutate(CSPre = CS - PreCS) %>% 
  pivot_longer(cols = c(PreCS,CS,CSPre), names_to = "Period", values_to = "MagEntries")


full_data <- full_data %>% 
  mutate(Group = str_replace(Group, "OFC", "Lesion"),
         Day = str_replace(Day, "Day", ""))

levelorder <- c("Sham", "Lesion")
full_data$Group <- fct_relevel(as.factor(full_data$Group), levelorder)

data_Expt1_PreLesion_Satiety_PerTrial <- full_data



#### Expt 1 Acquisition Pre Lesion: General Satiety Extinction Per trial  ----
full_data <- read_csv(here("data","Expt1_Satiety_Extinction_PerTrial.csv"))


full_data <- full_data %>% 
  mutate(Group = str_replace(Group, "OFC", "Lesion"),
         Stage = str_replace(Stage, "Hunger", "Hungry")) %>% 
  rename(Hunger = Stage)

levelorder <- c("Sham", "Lesion")
full_data$Group <- fct_relevel(as.factor(full_data$Group), levelorder)

levelorder <- c("Satiety", "Hungry")
full_data$Hunger <- fct_relevel(as.factor(full_data$Hunger), levelorder)


data_Expt1_PreLesion_Satiety_Extinction_PerTrial <- full_data



#### Expt 1 Acquisition Pre Lesion: Devaluation Post Test  ----
# Acquisition
full_data <-  read_csv(here("data","Expt1_DevaluationTest_data.csv"))

full_data <- full_data %>% 
  filter(Stage == "Acquisition") %>% 
  mutate(Group = str_replace(Group, "SHAM", "Sham"),
         Group = str_replace(Group, "LESION", "Lesion"),
         Condition = str_replace(Condition, "NonDevalued", "Non-Devalued"),
         Day = str_replace(Day, "Day", ""),
         Day = as.numeric(Day),
         DayBlock = ceiling(Day/2)) %>% 
  rename(Cue = Condition)

# Summarise into blocks of 2 days for simplicity
full_data <- full_data %>% 
  group_by(Group, Subject, Stage, Cue, DayBlock) %>% 
  summarise(MagEntries = mean(MagEntries)) %>% 
  unite(GroupCueID, Group, Cue, sep = ": ", remove = FALSE) %>% 
  ungroup()

  
levelorder <- c("Sham", "Lesion")
full_data$Group <- fct_relevel(as.factor(full_data$Group), levelorder)

levelorder <- c("Non-Devalued", "Devalued")
full_data$Cue <- fct_relevel(as.factor(full_data$Cue), levelorder)

levelorder <- c("Sham: Non-Devalued", "Sham: Devalued", "Lesion: Non-Devalued", "Lesion: Devalued")
full_data$GroupCueID <- fct_relevel(as.factor(full_data$GroupCueID), levelorder)

data_Expt1_PreLesion_PostDeval_Acquisition <- full_data 

# Taste Aversion
full_data <-  read_csv(here("data","Expt1_DevaluationTest_data.csv"))

full_data <- full_data %>% 
  filter(Stage == "TasteAversion") %>% 
  mutate(Group = str_replace(Group, "SHAM", "Sham"),
         Group = str_replace(Group, "LESION", "Lesion"),
         Day = str_replace(Day, "Pairing", "")) %>% 
  rename(Injection = Condition) %>% 
    rename(Pairing = Day) %>% 
    rename(Consumption = MagEntries)%>% 
  unite(GroupInjectionID, Group, Injection, sep = ": ", remove = FALSE)

levelorder <- c("Sham", "Lesion")
full_data$Group <- fct_relevel(as.factor(full_data$Group), levelorder)

levelorder <- c("Saline", "LiCl")
full_data$Injection <- fct_relevel(as.factor(full_data$Injection), levelorder)

levelorder <- c("Sham: Saline", "Sham: LiCl", "Lesion: Saline", "Lesion: LiCl")
full_data$GroupInjectionID <- fct_relevel(as.factor(full_data$GroupInjectionID), levelorder)

data_Expt1_PreLesion_PostDeval_TasteAversion <- full_data

# Devaluation test
# data_Expt1_PreLesion_PostDeval_DevalTest <- full_data %>% 
#   filter(Stage == "DevalTest")

full_data <-  read_csv(here("data","Expt1_DevaluationTest_data.csv"))

full_data <- full_data %>% 
  filter(Stage == "DevalTest") %>% 
  mutate(Group = str_replace(Group, "SHAM", "Sham"),
         Group = str_replace(Group, "LESION", "Lesion"),
         Condition = str_replace(Condition, "NonDevalued", "Non-Devalued")) %>% 
  rename(Cue = Condition) %>% 
  unite(GroupCueID, Group, Cue, sep = ": ", remove = FALSE)

levelorder <- c("Sham", "Lesion")
full_data$Group <- fct_relevel(as.factor(full_data$Group), levelorder)

levelorder <- c("Non-Devalued", "Devalued")
full_data$Cue <- fct_relevel(as.factor(full_data$Cue), levelorder)

levelorder <- c("Sham: Non-Devalued", "Sham: Devalued", "Lesion: Non-Devalued", "Lesion: Devalued")
full_data$GroupCueID <- fct_relevel(as.factor(full_data$GroupCueID), levelorder)


data_Expt1_PreLesion_PostDeval_DevalTest <- full_data
  

#### Expt 2 Acquisition PostInfusion  ----
full_data <- read_csv(here("data","Expt2_PostTrainingInfusion_Acquisition_Data.csv"))

full_data <- full_data %>% 
  mutate(ExptStage = Stage,
         Day = str_replace(Day, "Day", ""),
         Stage  = str_replace(Stage, "NoInfusion", "No Infusion"),
         Stage  = str_replace(Stage, "PostAcquisition", "Post"),
         DayPeriod = Day)

full_data$DayPeriod  = recode(full_data$DayPeriod, "10" = "Post", "11" = "Post") 

full_data <- full_data %>% 
  group_by(Group, Subj, Period, Stage, ExptStage, DayPeriod) %>% 
  summarise(MagEntries = mean(MagEntries)) %>% 
  ungroup()

levelorder <- c("Saline", "Muscimol")
full_data$Group <- fct_relevel(as.factor(full_data$Group), levelorder)

levelorder <- c("Acquisition", "Post", "Infusion", "No Infusion")
full_data$Stage <- fct_relevel(as.factor(full_data$Stage), levelorder)

levelorder <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "Post", "12", "13", "14", "15", "16", "17")
full_data$DayPeriod <- fct_relevel(as.factor(full_data$DayPeriod), levelorder)

data_Expt2_PostInfusion_Acquisition <- full_data

#### Expt 3 Acquisition PostLesion  ----
full_data <- read_csv(here("data","Expt3_PostTrainingLesion_Acquisition_Data.csv"))

full_data <- full_data %>% 
  mutate(Stage = str_replace(Stage, "Surgery", "-Surgery"),
         DayBlock = str_replace(DayBlock, "Block", ""),
         DayBlock = str_replace(DayBlock, "block", ""),)

levelorder <- c("Sham", "Lesion")
full_data$Group <- fct_relevel(as.factor(full_data$Group), levelorder)

data_Expt3_PostLesion_Acquisition <- full_data

#### Expt 4 Acquisition PostInfusion Early  ----
full_data <- read_csv(here("data","Expt4_PostTrainingInfusion_Early_Acquisition_Data.csv"))

full_data <- full_data %>% 
  mutate(Day = str_replace(Day, "Day", ""),
         Stage = Day)

full_data$Stage <- recode(full_data$Stage, "1" = "Acquisition", 
         "2" = "Acquisition", 
         "3" = "Acquisition", 
         "4" = "Acquisition", 
         "5" = "Infusion", 
         "6" = "Infusion", 
         "7" = "Infusion", 
         "8" = "Infusion", 
         "9" = "Infusion", 
         "10" = "No Infusion")

levelorder <- c("Saline", "Muscimol")
full_data$Group <- fct_relevel(as.factor(full_data$Group), levelorder)

levelorder <- c("Acquisition", "Infusion", "No Infusion")
full_data$Stage <- fct_relevel(as.factor(full_data$Stage), levelorder)


data_Expt4_PostInfusion_Early_Acquisition <- full_data

#### Expt 5 Blocking Infusion ----
# Stage 1
full_data <- read_csv(here("data","Expt5_Blocking_Stage1_Data.csv"))

full_data <- full_data %>% 
  mutate(Day = str_replace(Day, "Day", ""),
         Infusion  = str_replace(Infusion, "NoInfusion", "No Infusion")) %>% 
  unite(GroupCueID, Group, Cue, sep = ": ", remove = FALSE)


levelorder <- c("Saline", "Muscimol")
full_data$Group <- fct_relevel(as.factor(full_data$Group), levelorder)

levelorder <- c("Infusion", "No Infusion")
full_data$Infusion <- fct_relevel(as.factor(full_data$Infusion), levelorder)

levelorder <- c("Saline: A", "Muscimol: A")
full_data$GroupCueID <- fct_relevel(as.factor(full_data$GroupCueID), levelorder)


data_Expt5_Blocking_Infusion_Stage1 <- full_data


# Stage 2
full_data <- read_csv(here("data","Expt5_Blocking_Stage2_Test_ReAcquisition_Data.csv"))

full_data <- full_data %>% 
  filter(Period == "Stage2") %>% 
  mutate(Day = str_replace(Day, "Day", "")) %>% 
           unite(GroupCueID, Group, Cue, sep = ": ", remove = FALSE)

levelorder <- c("Saline", "Muscimol")
full_data$Group <- fct_relevel(as.factor(full_data$Group), levelorder)

levelorder <- c("Saline: AB", "Saline: CD", "Muscimol: AB", "Muscimol: CD")
full_data$GroupCueID <- fct_relevel(as.factor(full_data$GroupCueID), levelorder)


data_Expt5_Blocking_Infusion_Stage2 <- full_data

# Stage 2 - Block 2 Trials

full_data <- read_csv(here("data","Expt5_Blocking_Stage2_Block2Trials.csv"))

full_data <- full_data %>% 
  unite(GroupCueID, Group, Cue, sep = ": ", remove = FALSE)

levelorder <- c("Saline", "Muscimol")
full_data$Group <- fct_relevel(as.factor(full_data$Group), levelorder)

levelorder <- c("Saline: AB", "Saline: CD", "Muscimol: AB", "Muscimol: CD")
full_data$GroupCueID <- fct_relevel(as.factor(full_data$GroupCueID), levelorder)

data_Expt5_Blocking_Infusion_Stage2_Block2Trials <- full_data

# Test

full_data <- read_csv(here("data","Expt5_Blocking_Stage2_Test_ReAcquisition_Data.csv"))

full_data <- full_data %>% 
  filter(Period == "Test") %>% 
  mutate(Day = str_replace(Day, "Day", "")) %>% 
  unite(GroupCueID, Group, Cue, sep = ": ", remove = FALSE)

levelorder <- c("Saline", "Muscimol")
full_data$Group <- fct_relevel(as.factor(full_data$Group), levelorder)

levelorder <- c("Saline: B", "Saline: D", "Muscimol: B", "Muscimol: D")
full_data$GroupCueID <- fct_relevel(as.factor(full_data$GroupCueID), levelorder)

data_Expt5_Blocking_Infusion_Test <- full_data

# Reacquisition
full_data <- read_csv(here("data","Expt5_Blocking_Stage2_Test_ReAcquisition_Data.csv"))

full_data <- full_data %>% 
  filter(Period == "ReAcquisition") %>% 
  mutate(Day = str_replace(Day, "Day", "")) %>% 
  unite(GroupCueID, Group, Cue, sep = ": ", remove = FALSE)

levelorder <- c("Saline", "Muscimol")
full_data$Group <- fct_relevel(as.factor(full_data$Group), levelorder)

levelorder <- c("Saline: A", "Saline: B", "Muscimol: A", "Muscimol: B")
full_data$GroupCueID <- fct_relevel(as.factor(full_data$GroupCueID), levelorder)

data_Expt5_Blocking_Infusion_Reacquisition <- full_data

# # Correlate performance during Stage 1 with Test
# tempdata1 <- data_Expt5_Blocking_Infusion_Stage1 %>% 
#   filter (Period == "CSPre")
# tempdata2 <- data_Expt5_Blocking_Infusion_Test %>% 
#   mutate(Stage = "Test",
#          Infusion = "No Infusion",
#          Period = "CSPre")
# joined_data <-full_join(tempdata1, tempdata2)
# joined_data <- joined_data %>% 
#   unite(DayCueID, Cue, Day,sep = "_", remove = TRUE) %>% 
#   select(Group, DayCueID, Subject, MagEntries)
# joined_data <- spread(joined_data, DayCueID, MagEntries)
# 
# joined_data <- joined_data %>% 
#   mutate(Test = D_1516 - B_1516,
#           SumAcq = A_1 + A_2 + A_3 + A_4 + A_5 + A_6 + A_7 + A_8 + A_9 + A_10,
#          SumAcq_Infusion = A_5 + A_6 + A_7 + A_8 + A_9 + A_10)
# 
# Expt5_Correlation <- joined_data %>% 
#   ggplot(mapping = aes(x = SumAcq_Infusion, y = Test, group = Group, colour = Group, fill = Group, shape = Group,linetype = Group)) +
#   stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2)

#### Expt 6 Relative Value Acquisition ----

# Probability distribution
full_data <- read_csv(here("data","Expt6_RelativeValue_ProbabilityDistribution_Data.csv"))

data_Expt6_RewardProbabilityDistributions <- full_data

# Raw - Separate Magazines
full_data <- read_csv(here("data","Expt6_RelativeValue_Acquisition_Data.csv"))

full_data <- full_data %>% 
  filter(Magazine %in%  c("Pellet", "Dipper")) %>% 
  mutate(Magazine = str_replace(Magazine, "Dipper", "Sucrose"))
  
full_data$Probability <- recode(full_data$Probability, "p2" = "Low", 
                                  "p4" = "Medium", 
                                  "p8" = "High")

full_data$Stage <- recode(full_data$Stage, "WaterDep" = "Thirst", 
                                "Dip4xUp" = "4x Sucrose")
  
levelorder <- c("PreCS", "CS")
full_data$Period <- fct_relevel(as.factor(full_data$Period), levelorder)

levelorder <- c("Acquisition", "Thirst", "4x Sucrose")
full_data$Stage <- fct_relevel(as.factor(full_data$Stage), levelorder)

levelorder <- c("Sucrose", "Pellet")
full_data$Magazine <- fct_relevel(as.factor(full_data$Magazine), levelorder)

levelorder <- c("Low", "Medium", "High")
full_data$Probability <- fct_relevel(as.factor(full_data$Probability), levelorder)


data_Expt6_RelativeValue_Acquisition_MagSep <- full_data


# Mag Differences - Separate Magazines
full_data <- read_csv(here("data","Expt6_RelativeValue_Acquisition_Data.csv"))

full_data <- full_data %>% 
  filter(Magazine %in%  c("MagDipDiff")) %>% 
  mutate(Magazine = str_replace(Magazine, "MagDipDiff", "Pellet - Sucrose"))

full_data$Probability <- recode(full_data$Probability, "p2" = "Low", 
                                "p4" = "Medium", 
                                "p8" = "High")

full_data$Stage <- recode(full_data$Stage, "WaterDep" = "Thirst", 
                          "Dip4xUp" = "4x Sucrose")

levelorder <- c("PreCS", "CS")
full_data$Period <- fct_relevel(as.factor(full_data$Period), levelorder)

levelorder <- c("Acquisition", "Thirst", "4x Sucrose")
full_data$Stage <- fct_relevel(as.factor(full_data$Stage), levelorder)

levelorder <- c("Low", "Medium", "High")
full_data$Probability <- fct_relevel(as.factor(full_data$Probability), levelorder)


data_Expt6_RelativeValue_Acquisition_MagDiff <- full_data

#### Expt 6B Relative Value Infusion ----
# Raw - Separate Magazines
full_data <- read_csv(here("data","Expt6_RelativeValue_Infusion_Data.csv"))

full_data <- full_data %>% 
  filter(Magazine %in%  c("Pellet", "Dipper")) %>% 
  mutate(Magazine = str_replace(Magazine, "Dipper", "Sucrose"),
         Drug = str_replace(Drug, "Musscimol", "Muscimol")) %>% 
  unite(GroupDrugMag, Drug, Magazine, sep = ": ", remove = FALSE)

full_data$Probability <- recode(full_data$Probability, "prob2" = "Low", 
                                "prob8" = "High")

levelorder <- c("Saline", "Muscimol")
full_data$Drug <- fct_relevel(as.factor(full_data$Drug), levelorder)

levelorder <- c("PreCS", "CS")
full_data$Period <- fct_relevel(as.factor(full_data$Period), levelorder)

levelorder <- c("Sucrose", "Pellet")
full_data$Magazine <- fct_relevel(as.factor(full_data$Magazine), levelorder)

levelorder <- c("Low", "High")
full_data$Probability <- fct_relevel(as.factor(full_data$Probability), levelorder)


levelorder <- c("Saline: Sucrose", "Saline: Pellet", "Muscimol: Sucrose", "Muscimol: Pellet")
full_data$GroupDrugMag <- fct_relevel(as.factor(full_data$GroupDrugMag), levelorder)

data_Expt6B_RelativeValue_Infusion_MagSep <- full_data


# Mag Differences - Separate Magazines
full_data <- read_csv(here("data","Expt6_RelativeValue_Infusion_Data.csv"))

full_data <- full_data %>% 
  filter(Magazine %in%  c("Diff")) %>% 
  mutate(Magazine = str_replace(Magazine, "Diff", "Pellet - Sucrose"),
         Drug = str_replace(Drug, "Musscimol", "Muscimol")) %>% 
  unite(GroupDrugPeriod, Drug, Period, sep = ": ", remove = FALSE)

full_data$Probability <- recode(full_data$Probability, "prob2" = "Low", 
                                "prob8" = "High")

levelorder <- c("Saline", "Muscimol")
full_data$Drug <- fct_relevel(as.factor(full_data$Drug), levelorder)

levelorder <- c("PreCS", "CS")
full_data$Period <- fct_relevel(as.factor(full_data$Period), levelorder)

levelorder <- c("Low", "High")
full_data$Probability <- fct_relevel(as.factor(full_data$Probability), levelorder)

levelorder <- c("Saline: PreCS", "Saline: CS", "Muscimol: PreCS", "Muscimol: CS")
full_data$GroupDrugPeriod <- fct_relevel(as.factor(full_data$GroupDrugPeriod), levelorder)

data_Expt6B_RelativeValue_Infusion_MagDiff <- full_data


noquote(ls())
# List all variables
## Remove global variables from boiler plate code
rm(full_data, levelorder)
# print a list of all data variables without quotation marks




#### Figure plotting Parameter ----
## Define Colours to be used
DarkRed     = "#67001F" 
MediumRed   = "#B2182B"
LightRed    = "#D6604D"
DarkBlue    = "#053061"
MediumBlue  = "#2166AC"
LightBlue   = "#4393C3"  
Black       = "#000000"
White       = "#ffffff"
LightGrey   = "#F0F0F0"
MediumGrey  = "#BDBDBD"
DarkGrey    = "#252525"

## Shapes for Geom_point
circle            = 21
square            = 22
diamond           = 23
triangleUp        = 24
triangleDown      = 25


fillcolours <- c("Sham"                   = White,
                 "Sham: Satiety"          = DarkGrey,
                 "Sham: Hungry"           = White,
                 "Sham: Non-Devalued"     = White,
                 "Sham: Devalued"         = DarkGrey,
                 "Sham: Saline"           = White,
                 "Sham: LiCl"             = DarkGrey,
                 "Saline"                 = White,
                 "Saline: A"              = White,
                 "Saline: B"              = DarkGrey,
                 "Saline: D"              = White,
                 "Saline: AB"             = White,
                 "Saline: CD"             = White,
                 "Lesion"                 = MediumRed,
                 "Lesion: Satiety"        = MediumRed,
                 "Lesion: Hungry"         = White,
                 "Lesion: Non-Devalued"   = White,
                 "Lesion: Devalued"       = MediumRed,
                 "Lesion: Saline"         = White,
                 "Lesion: LiCl"           = MediumRed,
                 "Muscimol"               = MediumRed,
                 "Muscimol: A"            = MediumRed,
                 "Muscimol: B"            = MediumRed,
                 "Muscimol: D"            = White,
                 "Muscimol: AB"           = MediumRed,
                 "Muscimol: CD"           = MediumRed,
                 "PreCS"                  = White,
                 "CS"                     = DarkGrey,
                 "Sucrose"                = White,
                 "Pellet"                 = DarkGrey,
                 "Saline: Sucrose"        = White,
                 "Saline: Pellet"         = DarkGrey,
                 "Saline: PreCS"          = White,
                 "Saline: CS"             = DarkGrey,
                 "Muscimol: Sucrose"      = White,
                 "Muscimol: Pellet"       = MediumRed,
                 "Muscimol: PreCS"        = White,
                 "Muscimol: CS"           = MediumRed)

linecolours <- c("Sham"                   = Black,
                 "Sham: Satiety"          = Black,
                 "Sham: Hungry"           = Black,
                 "Sham: Non-Devalued"     = Black,
                 "Sham: Devalued"         = Black,
                 "Sham: Saline"           = Black,
                 "Sham: LiCl"             = Black,
                 "Saline"                 = Black,
                 "Saline: A"              = Black,
                 "Saline: B"              = Black,
                 "Saline: D"              = Black,
                 "Saline: AB"             = Black,
                 "Saline: CD"             = Black,
                 "Lesion"                 = Black,
                 "Lesion: Satiety"        = DarkRed,
                 "Lesion: Hungry"         = DarkRed,
                 "Lesion: Non-Devalued"   = DarkRed,
                 "Lesion: Devalued"       = DarkRed,
                 "Lesion: Saline"         = DarkRed,
                 "Lesion: LiCl"           = DarkRed,
                 "Muscimol"               = DarkRed,
                 "Muscimol: A"            = DarkRed,
                 "Muscimol: B"            = DarkRed,
                 "Muscimol: D"            = DarkRed,
                 "Muscimol: AB"           = DarkRed,
                 "Muscimol: CD"           = DarkRed,
                 "PreCS"                  = Black,
                 "CS"                     = Black,
                 "Sucrose"                = Black,
                 "Pellet"                 = Black,
                 "Saline: Sucrose"        = Black,
                 "Saline: Pellet"         = Black,
                 "Saline: PreCS"          = Black,
                 "Saline: CS"             = Black,
                 "Muscimol: Sucrose"      = DarkRed,
                 "Muscimol: Pellet"       = DarkRed,
                 "Muscimol: PreCS"        = DarkRed,
                 "Muscimol: CS"           = DarkRed)


linetypes <- c("Sham"                   = "dotted",
               "Sham: Satiety"          = "dotted",
               "Sham: Hungry"           = "dotted",
               "Sham: Non-Devalued"     = "dotted",
               "Sham: Devalued"         = "dotted",
               "Sham: Saline"           = "dotted",
               "Sham: LiCl"             = "dotted",
               "Saline"                 = "dotted",
               "Saline: A"              = "dotted",
               "Saline: B"              = "dotted",
               "Saline: D"              = "dotted",
               "Saline: AB"             = "dotted",
               "Saline: CD"             = "dotted",
               "Lesion"                 = "solid",
               "Lesion: Satiety"        = "solid",
               "Lesion: Hungry"         = "solid",
               "Lesion: Non-Devalued"   = "solid",
               "Lesion: Devalued"       = "solid",
               "Lesion: Saline"         = "solid",
               "Lesion: LiCl"           = "solid",
               "Muscimol"               = "solid",
               "Muscimol: A"            = "solid",
               "Muscimol: B"            = "solid",
               "Muscimol: D"            = "solid",
               "Muscimol: AB"           = "solid",
               "Muscimol: CD"           = "solid",
               "PreCS"                  = "solid",
               "CS"                     = "solid",
               "Sucrose"                = "solid",
               "Pellet"                 = "solid",
               "Saline: Sucrose"        = "solid",
               "Saline: Pellet"         = "solid",
               "Saline: PreCS"          = "solid",
               "Saline: CS"             = "solid",
               "Muscimol: Sucrose"      = "solid",
               "Muscimol: Pellet"       = "solid",
               "Muscimol: PreCS"        = "solid",
               "Muscimol: CS"           = "solid")


pointshapes <- c("Sham"                   = circle,
                 "Sham: Satiety"          = circle,
                 "Sham: Hungry"           = circle,
                 "Sham: Non-Devalued"     = circle,
                 "Sham: Devalued"         = circle,
                 "Sham: Saline"           = circle,
                 "Sham: LiCl"             = circle,
                 "Saline"                 = circle,
                 "Saline: A"              = square,
                 "Saline: B"              = circle,
                 "Saline: D"              = circle,
                 "Saline: AB"             = circle,
                 "Saline: CD"             = triangleUp,
                 "Lesion"                 = square,
                 "Lesion: Satiety"        = square,
                 "Lesion: Hungry"         = square,
                 "Lesion: Non-Devalued"   = square,
                 "Lesion: Devalued"       = square,
                 "Lesion: Saline"         = square,
                 "Lesion: LiCl"           = square,
                 "Muscimol"               = square,
                 "Muscimol: A"            = square,
                 "Muscimol: B"            = circle,
                 "Muscimol: D"            = square,
                 "Muscimol: AB"           = circle,
                 "Muscimol: CD"           = triangleUp,
                 "PreCS"                  = square,
                 "CS"                     = square,
                 "Sucrose"                = circle,
                 "Pellet"                 = square,
                 "Saline: Sucrose"        = circle,
                 "Saline: Pellet"         = square,
                 "Saline: PreCS"          = square,
                 "Saline: CS"             = square,
                 "Muscimol: Sucrose"      = circle,
                 "Muscimol: Pellet"       = square,
                 "Muscimol: PreCS"        = square,
                 "Muscimol: CS"           = square)


##### Plot Figures ----
##### Expt 1  ====
Expt1_PreLesion_Acquisition <- data_Expt1_PreLesion_AcquisitionBlock3 %>% 
  filter(Period == "CSPre") %>% 
  ggplot(mapping = aes(x = as.factor(Block), y = MagEntries, group = Group, colour = Group, fill = Group, shape = Group,linetype = Group)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,5)) +
  ggtitle("Acquisition") + xlab("Block (3 days)") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,15.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))  


Expt1_PreLesion_Acquisition_SKR064 <- data_Expt1_PreLesion_AcquisitionBlock3 %>% 
  filter(Period == "CSPre",
        Experiment == "SKR064" ) %>% 
  ggplot(mapping = aes(x = as.factor(Block), y = MagEntries, group = Group, colour = Group, fill = Group, shape = Group,linetype = Group)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,5)) +
  ggtitle("Acquisition") + xlab("Block (3 days)") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,15.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))  

Expt1_PreLesion_Acquisition_SKR073 <- data_Expt1_PreLesion_AcquisitionBlock3 %>% 
  filter(Period == "CSPre",
         Experiment == "SKR073" ) %>% 
  ggplot(mapping = aes(x = as.factor(Block), y = MagEntries, group = Group, colour = Group, fill = Group, shape = Group,linetype = Group)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,5)) +
  ggtitle("Acquisition") + xlab("Block (3 days)") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,15.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))  

Expt1_PreLesion_Loco <- data_Expt1_PreLesion_Locomotor %>% 
  ggplot(mapping = aes(x = as.factor(Time), y = `Beam breaks`, group = Group, colour = Group, fill = Group, shape = Group,linetype = Group)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,100)) +
  ggtitle("Locomotor") + xlab("Bin (30 mins)") + ylab("Beam Breaks") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,300.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))  


Expt1_PreLesion_Acquisition_Satiety <- data_Expt1_PreLesion_Satiety %>% 
  filter(Period == "CSPre") %>% 
  ggplot(mapping = aes(x = Group, y = MagEntries, group = GroupHungerID, colour = GroupHungerID, fill = GroupHungerID, shape = GroupHungerID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,5)) +
  ggtitle("Satiety") + xlab("Group") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,15.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(.5,"line"))  


Expt1_PreLesion_Acquisition_Satiety_PerTrial <- data_Expt1_PreLesion_Satiety_PerTrial %>% 
  filter(Period == "CSPre") %>% 
  ggplot(mapping = aes(x = as.factor(Trial), y = MagEntries, group = Group, colour = Group, fill = Group, shape = Group,linetype = Group)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,5)) +
  ggtitle("Satiety") + xlab("Trial") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,20.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))  


Expt1_PreLesion_PostDeval_Acquisition <- data_Expt1_PreLesion_PostDeval_Acquisition %>% 
  ggplot(mapping = aes(x = as.factor(DayBlock), y = MagEntries, group = GroupCueID, colour = GroupCueID, fill = GroupCueID, shape = GroupCueID, linetype = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,5)) +
  ggtitle("Novel Acquisition") + xlab("Block (2 days)") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,20.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))  

Expt1_PreLesion_PostDeval_TasteAversion <- data_Expt1_PreLesion_PostDeval_TasteAversion %>% 
  ggplot(mapping = aes(x = as.factor(Pairing), y = Consumption, group = GroupInjectionID, colour = GroupInjectionID, fill = GroupInjectionID, shape = GroupInjectionID, linetype = GroupInjectionID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,5)) +
  ggtitle("Taste Aversion") + xlab("Pairing Number") + ylab("Consumption (g)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,20.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line")) 

Expt1_PreLesion_Post_Deval_Test <- data_Expt1_PreLesion_PostDeval_DevalTest %>% 
  ggplot(mapping = aes(x = Group, y = MagEntries, group = GroupCueID, colour = GroupCueID, fill = GroupCueID, shape = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,5)) +
  ggtitle("Devaluation") + xlab("Group") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,20.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(.5,"line"))  

##### Expt 2  ====
Expt2_PostInfusion_Acquisition_a <- data_Expt2_PostInfusion_Acquisition %>% 
  filter(Period == "CSPre" &
         Stage == "Acquisition") %>% 
  ggplot(mapping = aes(x = as.factor(DayPeriod), y = MagEntries, group = Group, colour = Group, fill = Group, shape = Group,linetype = Group)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,2)) +
  ggtitle("") + xlab("") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,12.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line")) 

Expt2_PostInfusion_Acquisition_b <- data_Expt2_PostInfusion_Acquisition %>% 
  filter(Period == "CSPre" &
           Stage == "Post") %>% 
  ggplot(mapping = aes(x = as.factor(DayPeriod), y = MagEntries, group = Group, colour = Group, fill = Group, shape = Group,linetype = Group)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,2)) +
  ggtitle("") + xlab("") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,12.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line")) 

Expt2_PostInfusion_Acquisition_c <- data_Expt2_PostInfusion_Acquisition %>% 
  filter(Period == "CSPre" &
           Stage == "Infusion") %>% 
  ggplot(mapping = aes(x = as.factor(DayPeriod), y = MagEntries, group = Group, colour = Group, fill = Group, shape = Group,linetype = Group)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,2)) +
  ggtitle("") + xlab("Infusion") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,12.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))  

Expt2_PostInfusion_Acquisition_d <- data_Expt2_PostInfusion_Acquisition %>% 
  filter(Period == "CSPre" &
           Stage == "No Infusion") %>% 
  ggplot(mapping = aes(x = as.factor(DayPeriod), y = MagEntries, group = Group, colour = Group, fill = Group, shape = Group,linetype = Group)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,2)) +
  ggtitle("") + xlab("") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,12.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line")) 


##### Expt 3  ====
Expt3_PostLesion_Acquisition_a <- data_Expt3_PostLesion_Acquisition %>% 
  filter(Period == "CSPre" &
           Stage == "Pre-Surgery") %>% 
  ggplot(mapping = aes(x = as.factor(DayBlock), y = MagEntries, group = Group, colour = Group, fill = Group, shape = Group,linetype = Group)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,2)) +
  ggtitle("") + xlab("Pre-Surgery") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,12.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line")) 

Expt3_PostLesion_Acquisition_b <- data_Expt3_PostLesion_Acquisition %>% 
  filter(Period == "CSPre" &
           Stage == "Post-Surgery") %>% 
  ggplot(mapping = aes(x = as.factor(DayBlock), y = MagEntries, group = Group, colour = Group, fill = Group, shape = Group,linetype = Group)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,2)) +
  ggtitle("") + xlab("Post-Surgery") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,12.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line")) 

##### Expt 4  ====
Expt4_PostInfusion_Early_Acquisition_a <- data_Expt4_PostInfusion_Early_Acquisition %>% 
  filter(Period == "CSPre" &
           Stage == "Acquisition") %>% 
  ggplot(mapping = aes(x = as.factor(Day), y = MagEntries, group = Group, colour = Group, fill = Group, shape = Group,linetype = Group)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("") + xlab("") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,6.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line")) 

Expt4_PostInfusion_Early_Acquisition_b <- data_Expt4_PostInfusion_Early_Acquisition %>% 
  filter(Period == "CSPre" &
           Stage == "Infusion") %>% 
  ggplot(mapping = aes(x = as.factor(Day), y = MagEntries, group = Group, colour = Group, fill = Group, shape = Group,linetype = Group)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("") + xlab("Infusion") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,6.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line")) 

Expt4_PostInfusion_Early_Acquisition_c <- data_Expt4_PostInfusion_Early_Acquisition %>% 
  filter(Period == "CSPre" &
           Stage == "No Infusion") %>% 
  ggplot(mapping = aes(x = as.factor(Day), y = MagEntries, group = Group, colour = Group, fill = Group, shape = Group,linetype = Group)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("") + xlab("") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,6.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line")) 

##### Expt 5  ====

Expt5_Blocking_Infusion_Stage1_a <- data_Expt5_Blocking_Infusion_Stage1 %>% 
  filter(Period == "CSPre",
         Infusion == "No Infusion") %>% 
  ggplot(mapping = aes(x = as.factor(as.numeric(Day)), y = MagEntries, group = GroupCueID, colour = GroupCueID, fill = GroupCueID, shape = GroupCueID, linetype = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("") + xlab("Day") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,6.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line")) 

Expt5_Blocking_Infusion_Stage1_b <- data_Expt5_Blocking_Infusion_Stage1 %>% 
  filter(Period == "CSPre",
         Infusion == "Infusion") %>% 
  ggplot(mapping = aes(x = as.factor(as.numeric(Day)), y = MagEntries, group = GroupCueID, colour = GroupCueID, fill = GroupCueID, shape = GroupCueID, linetype = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("") + xlab("Infusion") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,6.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line")) 

Expt5_Blocking_Infusion_Stage2_a <- data_Expt5_Blocking_Infusion_Stage2 %>% 
  filter(Group == "Saline") %>% 
  ggplot(mapping = aes(x = as.factor(as.numeric(Day)), y = MagEntries, group = GroupCueID, colour = GroupCueID, fill = GroupCueID, shape = GroupCueID, linetype = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("") + xlab("Day") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,6.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line")) 

Expt5_Blocking_Infusion_Stage2_b <- data_Expt5_Blocking_Infusion_Stage2 %>% 
  filter(Group == "Muscimol") %>% 
  ggplot(mapping = aes(x = as.factor(as.numeric(Day)), y = MagEntries, group = GroupCueID, colour = GroupCueID, fill = GroupCueID, shape = GroupCueID, linetype = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("") + xlab("Day") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,6.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line")) 

Expt5_Blocking_Infusion_Stage2_Trial <- data_Expt5_Blocking_Infusion_Stage2_Block2Trials %>% 
  filter(Day == "12",
         Period == "CSPre") %>% 
  ggplot(mapping = aes(x = as.factor(TrialBlock2), y = MagEntries, group = GroupCueID, colour = GroupCueID, fill = GroupCueID, shape = GroupCueID, linetype = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Stage 2: Day 12") + xlab("Block (2 trials)") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,6.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line")) 


Expt5_Blocking_Infusion_Test <- data_Expt5_Blocking_Infusion_Test %>% 
  ggplot(mapping = aes(x = Group, y = MagEntries, group = GroupCueID, colour = GroupCueID, fill = GroupCueID, shape = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("") + xlab("Group") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,6.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(.5,"line"))

Expt5_Blocking_Infusion_ReAcquisition_CueB <- data_Expt5_Blocking_Infusion_Reacquisition %>% 
  filter(Cue == "B") %>% 
  drop_na() %>% 
  ggplot(mapping = aes(x = as.factor(Day), y = MagEntries, group = GroupCueID, colour = GroupCueID, fill = GroupCueID, shape = GroupCueID, linetype = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("") + xlab("Day") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,6.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line")) 

Expt5_Blocking_Infusion_ReAcquisition_CueA <- data_Expt5_Blocking_Infusion_Reacquisition %>% 
  filter(Cue == "A") %>% 
  drop_na() %>% 
  ggplot(mapping = aes(x = as.factor(Day), y = MagEntries, group = GroupCueID, colour = GroupCueID, fill = GroupCueID, shape = GroupCueID, linetype = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("") + xlab("Day") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,6.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line")) 

##### Expt 6  ====
Expt6_ProbabilityDistributions <- data_Expt6_RewardProbabilityDistributions %>% 
  ggplot(mapping = aes(x = as.factor(SucroseRewardAvailabilityInterval_5s) , y = RelativeFrequency , group = Probability , colour = Probability , fill = Probability , shape = Probability)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = position_dodge(width=0),  size = 1, alpha = 0.4)+
  stat_summary_bin(fun.data = "mean_se", geom = "line",  size = .5, alpha = .6) +
# Make Pretty
scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,5)) +
  ggtitle("") + xlab("Interval between sucrose availability (s)") + ylab("%Relative frequency") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,30)) +
  theme(axis.title.x=element_text(face = "bold")) 

Expt6_RelativeValue_4xDipper_MagDiff <- data_Expt6_RelativeValue_Acquisition_MagDiff %>% 
  filter(Stage == "4x Sucrose") %>% 
  ggplot(mapping = aes(x = Probability, y = MagEntries, group = Period , colour = Period , fill = Period , shape = Period )) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Response Bias") + xlab("Probability") + ylab("Pellet - Sucrose") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-3,3.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(.5,"line"))

Expt6_RelativeValue_4xDipper_PreCS <- data_Expt6_RelativeValue_Acquisition_MagSep %>% 
  filter(Stage == "4x Sucrose",
         Period == "PreCS") %>% 
  ggplot(mapping = aes(x = Probability, y = MagEntries, group = Magazine, colour = Magazine , fill = Magazine , shape = Magazine )) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("PreCS") + xlab("Probability") + ylab("Magazine Entries (5s)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,4.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(.5,"line"))

Expt6_RelativeValue_4xDipper_CS <- data_Expt6_RelativeValue_Acquisition_MagSep %>% 
  filter(Stage == "4x Sucrose",
         Period == "CS") %>% 
  ggplot(mapping = aes(x = Probability, y = MagEntries, group = Magazine, colour = Magazine , fill = Magazine , shape = Magazine )) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("CS") + xlab("Probability") + ylab("Magazine Entries (5s)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,4.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(.5,"line"))

Expt6_RelativeValue_Acquisition_MagDiff <- data_Expt6_RelativeValue_Acquisition_MagDiff %>% 
  filter(Stage == "Acquisition") %>% 
  ggplot(mapping = aes(x = Probability, y = MagEntries, group = Period , colour = Period , fill = Period , shape = Period )) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Response Bias") + xlab("Probability") + ylab("Pellet - Sucrose") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-3,3.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(.5,"line"))

Expt6_RelativeValue_Acquisition_PreCS <- data_Expt6_RelativeValue_Acquisition_MagSep %>% 
  filter(Stage == "Acquisition",
         Period == "PreCS") %>% 
  ggplot(mapping = aes(x = Probability, y = MagEntries, group = Magazine, colour = Magazine , fill = Magazine , shape = Magazine )) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("PreCS") + xlab("Probability") + ylab("Magazine Entries (5s)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,4.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(.5,"line"))

Expt6_RelativeValue_Acquisition_CS <- data_Expt6_RelativeValue_Acquisition_MagSep %>% 
  filter(Stage == "Acquisition",
         Period == "CS") %>% 
  ggplot(mapping = aes(x = Probability, y = MagEntries, group = Magazine, colour = Magazine , fill = Magazine , shape = Magazine )) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("CS") + xlab("Probability") + ylab("Magazine Entries (5s)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,4.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(.5,"line"))


Expt6_RelativeValue_Thirst_MagDiff <- data_Expt6_RelativeValue_Acquisition_MagDiff %>% 
  filter(Stage == "Thirst") %>% 
  ggplot(mapping = aes(x = Probability, y = MagEntries, group = Period , colour = Period , fill = Period , shape = Period )) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Response Bias") + xlab("Probability") + ylab("Pellet - Sucrose") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-3,3.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(.5,"line"))

Expt6_RelativeValue_Thirst_PreCS <- data_Expt6_RelativeValue_Acquisition_MagSep %>% 
  filter(Stage == "Thirst",
         Period == "PreCS") %>% 
  ggplot(mapping = aes(x = Probability, y = MagEntries, group = Magazine, colour = Magazine , fill = Magazine , shape = Magazine )) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("PreCS") + xlab("Probability") + ylab("Magazine Entries (5s)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,4.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(.5,"line"))

Expt6_RelativeValue_Thirst_CS <- data_Expt6_RelativeValue_Acquisition_MagSep %>% 
  filter(Stage == "Thirst",
         Period == "CS") %>% 
  ggplot(mapping = aes(x = Probability, y = MagEntries, group = Magazine, colour = Magazine , fill = Magazine , shape = Magazine )) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("CS") + xlab("Probability") + ylab("Magazine Entries (5s)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,4.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(.5,"line"))

##### Expt 6B  ====

Expt6B_RelativeValue_Saline_MagDiff <- data_Expt6B_RelativeValue_Infusion_MagDiff %>% 
  filter(Drug == "Saline") %>% 
  ggplot(mapping = aes(x = Probability, y = MagEntries, group = GroupDrugPeriod  , colour = GroupDrugPeriod  , fill = GroupDrugPeriod  , shape = GroupDrugPeriod  )) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Saline") + xlab("Probability") + ylab("Pellet - Sucrose") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-3,3.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(.5,"line"))

Expt6B_RelativeValue_Saline_PreCS <- data_Expt6B_RelativeValue_Infusion_MagSep %>% 
  filter(Drug == "Saline",
         Period == "PreCS") %>% 
  ggplot(mapping = aes(x = Probability, y = MagEntries, group = GroupDrugMag, colour = GroupDrugMag , fill = GroupDrugMag , shape = GroupDrugMag )) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("PreCS") + xlab("Probability") + ylab("Magazine Entries (5s)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,4.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(.5,"line"))

Expt6B_RelativeValue_Saline_CS <- data_Expt6B_RelativeValue_Infusion_MagSep %>% 
  filter(Drug == "Saline",
         Period == "CS") %>% 
  ggplot(mapping = aes(x = Probability, y = MagEntries, group = GroupDrugMag, colour = GroupDrugMag , fill = GroupDrugMag , shape = GroupDrugMag )) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("CS") + xlab("Probability") + ylab("Magazine Entries (5s)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,4.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(.5,"line"))

Expt6B_RelativeValue_Muscimol_MagDiff <- data_Expt6B_RelativeValue_Infusion_MagDiff %>% 
  filter(Drug == "Muscimol") %>% 
  ggplot(mapping = aes(x = Probability, y = MagEntries, group = GroupDrugPeriod  , colour = GroupDrugPeriod  , fill = GroupDrugPeriod  , shape = GroupDrugPeriod  )) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Muscimol") + xlab("Probability") + ylab("Pellet - Sucrose") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-3,3.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(.5,"line"))

Expt6B_RelativeValue_Muscimol_PreCS <- data_Expt6B_RelativeValue_Infusion_MagSep %>% 
  filter(Drug == "Muscimol",
         Period == "PreCS") %>% 
  ggplot(mapping = aes(x = Probability, y = MagEntries, group = GroupDrugMag, colour = GroupDrugMag , fill = GroupDrugMag , shape = GroupDrugMag )) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("PreCS") + xlab("Probability") + ylab("Magazine Entries (5s)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,4.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(.5,"line"))

Expt6B_RelativeValue_Muscimol_CS <- data_Expt6B_RelativeValue_Infusion_MagSep %>% 
  filter(Drug == "Muscimol",
         Period == "CS") %>% 
  ggplot(mapping = aes(x = Probability, y = MagEntries, group = GroupDrugMag, colour = GroupDrugMag , fill = GroupDrugMag , shape = GroupDrugMag )) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("CS") + xlab("Probability") + ylab("Magazine Entries (5s)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,4.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(.5,"line"))

##### Plot Layouts ----
##### Figure 1 ====
A1 <- Expt1_PreLesion_Acquisition + theme(legend.position= c(0.05,.95), 
                                          legend.justification='left',
                                          legend.direction='vertical')

A2 <- Expt1_PreLesion_Loco + theme(legend.position= c(0.05,.95), 
                                   legend.justification='left',
                                   legend.direction='vertical')

A3 <- Expt1_PreLesion_Acquisition_Satiety + theme(legend.position= c(0.05,.95), 
                                                  legend.justification='left',
                                                  legend.direction='vertical')

A4 <- Expt1_PreLesion_Acquisition_Satiety_PerTrial + theme(legend.position= c(0.05,.95), 
                                                           legend.justification='left',
                                                           legend.direction='vertical')

A5 <- Expt1_PreLesion_PostDeval_Acquisition + theme(legend.position= c(0.05,.95), 
                                                    legend.justification='left',
                                                    legend.direction='vertical')

A6 <- Expt1_PreLesion_PostDeval_TasteAversion + theme(legend.position= c(0.05,.95), 
                                                      legend.justification='left',
                                                      legend.direction='vertical')

A7 <- Expt1_PreLesion_Post_Deval_Test + theme(legend.position= c(0.05,.95), 
                                              legend.justification='left',
                                              legend.direction='vertical')



layout <- "
AAAABBBCCDDD
"

Fig1 <- A1 + A3 + A2 + A7 + plot_annotation(tag_levels = 'A') + plot_layout(design = layout) 

filename = here("figures", "Fig1.pdf")
ggsave(filename, Fig1, width =  260, height = 100, units = "mm")


Fig1_S2 <- A4

filename = here("figures", "Fig1_S2.pdf")
ggsave(filename, Fig1_S2, width =  80, height = 100, units = "mm")


layout <- "
AAABB
"

Fig1_S3 <- A5 + A6 + plot_annotation(tag_levels = 'A') + plot_layout(design = layout) 

filename = here("figures", "Fig1_S3.pdf")
ggsave(filename, Fig1_S3, width =  120, height = 100, units = "mm")

##### Figure 2A ====
B1 <- Expt2_PostInfusion_Acquisition_a + theme(legend.position= c(0.05,.95), 
                                               legend.justification='left',
                                               legend.direction='vertical')

B2 <- Expt2_PostInfusion_Acquisition_b + theme(axis.line.y =  element_blank(),
                                               axis.title.y = element_blank(),
                                               axis.text.y = element_blank(),
                                               axis.ticks.y = element_blank(),
                                               legend.position="none",
                                               axis.text.x = element_text(angle = 45, hjust = 1))

B3 <- Expt2_PostInfusion_Acquisition_c + theme(axis.line.y =  element_blank(),
                                                axis.title.y = element_blank(),
                                                axis.text.y = element_blank(),
                                                axis.ticks.y = element_blank(),
                                                legend.position="none")

B4 <- Expt2_PostInfusion_Acquisition_d + theme(axis.line.y =  element_blank(),
                                               axis.title.y = element_blank(),
                                               axis.text.y = element_blank(),
                                               axis.ticks.y = element_blank(),
                                               legend.position="none")

layout <- "
AAAAAAAABCCCCDD
"
Fig2A <- B1 + B2 + B3 + B4  + plot_layout(design = layout) 

filename = here("figures", "Fig2A.pdf")
ggsave(filename, Fig2A, width =  120, height = 80, units = "mm")

##### Figure 2B ====

C1 <- Expt3_PostLesion_Acquisition_a + theme(legend.position= c(0.05,.95), 
                                             legend.justification='left',
                                             legend.direction='vertical')

C2 <- Expt3_PostLesion_Acquisition_b + theme(axis.line.y =  element_blank(),
                                             axis.title.y = element_blank(),
                                             axis.text.y = element_blank(),
                                             axis.ticks.y = element_blank(),
                                             legend.position="none")


layout <- "
AAAABBBB
"


Fig2B <- C1 + C2  + plot_layout(design = layout) 

filename = here("figures", "Fig2B.pdf")
ggsave(filename, Fig2B, width =  80, height = 80, units = "mm")


##### Figure 2C ====

D1 <- Expt4_PostInfusion_Early_Acquisition_a + theme(legend.position= c(0.05,.95), 
                                             legend.justification='left',
                                             legend.direction='vertical')
D1 <- shift_xaxis(D1)

D2 <- Expt4_PostInfusion_Early_Acquisition_b + theme(axis.line.y =  element_blank(),
                                             axis.title.y = element_blank(),
                                             axis.text.y = element_blank(),
                                             axis.ticks.y = element_blank(),
                                             legend.position="none")

D2 <- shift_xaxis(D2)

D3 <- Expt4_PostInfusion_Early_Acquisition_c + theme(axis.line.y =  element_blank(),
                                                     axis.title.y = element_blank(),
                                                     axis.text.y = element_blank(),
                                                     axis.ticks.y = element_blank(),
                                                     legend.position="none")

D3 <- shift_xaxis(D3)


layout <- "
AAAABBBBBC
"


Fig2C <- D1 + D2 + D3  + plot_layout(design = layout) 

filename = here("figures", "Fig2C.pdf")
ggsave(filename, Fig2C, width =  80, height = 80, units = "mm")


##### Figure 3 ====
E1 <- Expt5_Blocking_Infusion_Stage1_a + theme(legend.position= c(0.05,.95), 
                                               legend.justification='left',
                                               legend.direction='vertical') 
E1 <- shift_xaxis(E1)

E2 <- Expt5_Blocking_Infusion_Stage1_b + theme(axis.line.y =  element_blank(),
                                               axis.title.y = element_blank(),
                                               axis.text.y = element_blank(),
                                               axis.ticks.y = element_blank(),
                                               legend.position="none")

E2 <- shift_xaxis(E2)

E3 <- Expt5_Blocking_Infusion_Stage2_a + theme(legend.position= c(0.05,.3), 
                                               legend.justification='left',
                                               legend.direction='vertical',
                                               axis.title.y = element_blank(),
                                               axis.text.y = element_blank())

E3 <- shift_xaxis(E3)

E4 <- Expt5_Blocking_Infusion_Stage2_b + theme(legend.position= c(0.05,.3), 
                                               legend.justification='left',
                                               legend.direction='vertical',
                                               axis.title.y = element_blank(),
                                               axis.text.y = element_blank())
E4 <- shift_xaxis(E4)

E5 <- Expt5_Blocking_Infusion_Stage2_Trial + theme(legend.position= c(0.6,.35), 
                                                   legend.justification='left',
                                                   legend.direction='vertical')

E5 <- shift_xaxis(E5)

E6 <- Expt5_Blocking_Infusion_Test + theme(legend.position= c(0.05,.90), 
                                           legend.justification='left',
                                           legend.direction='vertical',
                                           axis.title.y = element_blank(),
                                           axis.text.y = element_blank())

E6 <- shift_xaxis(E6)

E7 <- Expt5_Blocking_Infusion_ReAcquisition_CueB + theme(legend.position= c(0.05,.95), 
                                                         legend.justification='left',
                                                         legend.direction='vertical')
E7 <- shift_xaxis(E7)

E8 <- Expt5_Blocking_Infusion_ReAcquisition_CueA + theme(legend.position= c(0.05,.95), 
                                                         legend.justification='left',
                                                         legend.direction='vertical',
                                                         axis.title.y = element_blank(),
                                                         axis.text.y = element_blank())
E8 <- shift_xaxis(E8)



layout <- "
AAABBBBCCCDDDEEE
"

Fig3 <- E1 + E2 + E3 + E4 + E6 + plot_layout(design = layout)

filename = here("figures", "Fig3.pdf")
ggsave(filename, Fig3, width =  200, height = 80, units = "mm")


Fig3_S2 <- E5

filename = here("figures", "Fig3_S2.pdf")
ggsave(filename, Fig3_S2, width =  80, height = 80, units = "mm")

layout <- "
AAABB
"
Fig3_S3 <- E7 + E8 + plot_layout(design = layout) + plot_annotation(tag_levels = 'A')

filename = here("figures", "Fig3_S3.pdf")
ggsave(filename, Fig3_S3, width =  90, height = 80, units = "mm")


##### Figure 4 ====
F1 <- Expt6_RelativeValue_Acquisition_MagDiff + theme(legend.position= c(0.05, .20), 
                                                      legend.justification='left',
                                                      legend.direction='vertical',
                                                      axis.title.x = element_blank()) 
F1 <- shift_xaxis(F1)

F2 <- Expt6_RelativeValue_Acquisition_PreCS + theme(legend.position= c(0.05,.95), 
                                                    legend.justification='left',
                                                    legend.direction='vertical',
                                                    axis.title.x = element_blank())

F3 <- Expt6_RelativeValue_Acquisition_CS + theme(legend.position= c(0.70,.95), 
                                                 legend.justification='left',
                                                 legend.direction='vertical',
                                                 axis.title.y = element_blank(),
                                                 axis.text.y = element_blank())

F4 <- Expt6_RelativeValue_Thirst_MagDiff  + theme(legend.position= c(0.05,.20), 
                                                  legend.justification='left',
                                                  legend.direction='vertical',
                                                  axis.title.x = element_blank()) 
F4 <- shift_xaxis(F4)

F5 <- Expt6_RelativeValue_Thirst_PreCS + theme(legend.position= c(0.05,.95), 
                                               legend.justification='left',
                                               legend.direction='vertical',
                                               axis.title.x = element_blank())

F6 <- Expt6_RelativeValue_Thirst_CS + theme(legend.position= c(0.70,.95), 
                                          legend.justification='left',
                                          legend.direction='vertical',
                                          axis.title.y = element_blank(),
                                          axis.text.y = element_blank())

F7 <- Expt6_RelativeValue_4xDipper_MagDiff  + theme(legend.position= c(0.05,.20), 
                                                    legend.justification='left',
                                                    legend.direction='vertical',
                                                    axis.title.x = element_blank()) 
F7 <- shift_xaxis(F7)

F8 <- Expt6_RelativeValue_4xDipper_PreCS + theme(legend.position= c(0.05,.95), 
                                                 legend.justification='left',
                                                 legend.direction='vertical',
                                                 axis.title.x = element_blank())

F9 <- Expt6_RelativeValue_4xDipper_CS + theme(legend.position= c(0.70,.95), 
                                              legend.justification='left',
                                              legend.direction='vertical',
                                              axis.title.y = element_blank(),
                                              axis.text.y = element_blank())

blank <- ggplot() + geom_blank()

layout <- "
AB
CD
"

Fig4_option <- blank+ F7 + F8 + F9 + plot_annotation(tag_levels = 'A') + plot_layout(design = layout)

filename = here("figures", "Fig4_option.pdf")
ggsave(filename, Fig4_option, width =  120, height = 160, units = "mm")


layout <- "
ABC
DEF
GHI
"


Fig4 <- F2 + F3 + F1 + F5 + F6 + F4 + F8 + F9 + F7 + plot_layout(design = layout) + plot_annotation(tag_levels = 'A')
# Fig4 <- F1 + F2 + F3 + F4 + F5 + F6 + F7 + F8 + F9 + plot_layout(design = layout) + plot_annotation(tag_levels = 'A')
# Fig4 <-  F2 + F5 + F8 + F3 + F6 + F9 + F1 + F4 + F7 + plot_layout(design = layout) + plot_annotation(tag_levels = 'A')
filename = here("figures", "Fig4.pdf")
ggsave(filename, Fig4, width =  160, height = 210, units = "mm")

##### Figure 5 ====
G1 <- Expt6B_RelativeValue_Saline_MagDiff + theme(legend.position= c(0.05,.95), 
                                                  legend.justification='left',
                                                  legend.direction='vertical',
                                                  axis.title.x = element_blank()) 
G1 <- shift_xaxis(G1)

G2 <- Expt6B_RelativeValue_Saline_PreCS + theme(legend.position= c(0.05,.95), 
                                                legend.justification='left',
                                                legend.direction='vertical') 


G3 <- Expt6B_RelativeValue_Saline_CS + theme(legend.position= c(0.05,.95), 
                                             legend.justification='left',
                                             legend.direction='vertical') 

G4 <- Expt6B_RelativeValue_Muscimol_MagDiff + theme(legend.position= c(0.05,.95), 
                                                    legend.justification='left',
                                                    legend.direction='vertical',
                                                    axis.title.x = element_blank()) 
G4 <- shift_xaxis(G4)

G5 <- Expt6B_RelativeValue_Muscimol_PreCS + theme(legend.position= c(0.05,.95), 
                                                  legend.justification='left',
                                                  legend.direction='vertical',
                                                  axis.title.y = element_blank(),
                                                  axis.text.y = element_blank()) 


G6 <- Expt6B_RelativeValue_Muscimol_CS + theme(legend.position= c(0.05,.95), 
                                               legend.justification='left',
                                               legend.direction='vertical',
                                               axis.title.y = element_blank(),
                                               axis.text.y = element_blank()) 


layout <- "
AB
CD
"

Fig5 <- G2 + G5 + G3 + G6 + plot_annotation(tag_levels = 'A') + plot_layout(design = layout)

filename = here("figures", "Fig5.pdf")
ggsave(filename, Fig5, width =  100, height = 160, units = "mm")


Fig5_S2 <- G1 + G4 + plot_annotation(tag_levels = 'A') +  plot_layout(design = layout)


filename = here("figures", "Fig5_S2.pdf")
ggsave(filename, Fig5_S2, width =  100, height = 160, units = "mm")




##### List of Figures ----
# Expt1_PreLesion_Acquisition
# Expt1_PreLesion_Loco
# Expt1_PreLesion_Acquisition_Satiety
# Expt1_PreLesion_Acquisition_Satiety_PerTrial
# Expt1_PreLesion_PostDeval_Acquisition
# Expt1_PreLesion_PostDeval_TasteAversion
# Expt1_PreLesion_Post_Deval_Test
# Expt2_PostInfusion_Acquisition_a
# Expt2_PostInfusion_Acquisition_b
# Expt2_PostInfusion_Acquisition_c
# Expt2_PostInfusion_Acquisition_d
# Expt3_PostLesion_Acquisition_a
# Expt3_PostLesion_Acquisition_b
# Expt4_PostInfusion_Early_Acquisition_a
# Expt4_PostInfusion_Early_Acquisition_b
# Expt4_PostInfusion_Early_Acquisition_c
# Expt5_Blocking_Infusion_Stage1_a
# Expt5_Blocking_Infusion_Stage1_b
# Expt5_Blocking_Infusion_Stage2_a
# Expt5_Blocking_Infusion_Stage2_b
# Expt5_Blocking_Infusion_Stage2_Trial
# Expt5_Blocking_Infusion_Test
# Expt5_Blocking_Infusion_ReAcquisition_CueB
# Expt5_Blocking_Infusion_ReAcquisition_CueA
# Expt6_RelativeValue_Acquisition_MagDiff
# Expt6_RelativeValue_Acquisition_PreCS
# Expt6_RelativeValue_Acquisition_CS
# Expt6_RelativeValue_4xDipper_MagDiff
# Expt6_RelativeValue_4xDipper_PreCS
# Expt6_RelativeValue_4xDipper_CS
# Expt6_RelativeValue_Thirst_MagDiff
# Expt6_RelativeValue_Thirst_PreCS
# Expt6_RelativeValue_Thirst_CS
# Expt6B_RelativeValue_Saline_MagDiff
# Expt6B_RelativeValue_Saline_PreCS
# Expt6B_RelativeValue_Saline_CS
# Expt6B_RelativeValue_Muscimol_MagDiff
# Expt6B_RelativeValue_Muscimol_PreCS
# Expt6B_RelativeValue_Muscimol_CS

##### Save Plot Data as CSV for publication ----

dataset_name <-  c("data_Expt1_PreLesion_AcquisitionBlock3",
                    "data_Expt1_PreLesion_Locomotor",
                    "data_Expt1_PreLesion_PostDeval_Acquisition",
                    "data_Expt1_PreLesion_PostDeval_DevalTest",
                    "data_Expt1_PreLesion_PostDeval_TasteAversion",
                    "data_Expt1_PreLesion_Satiety",
                    "data_Expt1_PreLesion_Satiety_Extinction_PerTrial",
                    "data_Expt1_PreLesion_Satiety_PerTrial",
                    "data_Expt2_PostInfusion_Acquisition",
                    "data_Expt3_PostLesion_Acquisition",
                    "data_Expt4_PostInfusion_Early_Acquisition",
                    "data_Expt5_Blocking_Infusion_Reacquisition",
                    "data_Expt5_Blocking_Infusion_Stage1",
                    "data_Expt5_Blocking_Infusion_Stage2",
                    "data_Expt5_Blocking_Infusion_Stage2_Block2Trials",
                    "data_Expt5_Blocking_Infusion_Test",
                    "data_Expt6_RelativeValue_Acquisition_MagDiff",
                    "data_Expt6_RelativeValue_Acquisition_MagSep",
                    "data_Expt6B_RelativeValue_Infusion_MagDiff",
                    "data_Expt6B_RelativeValue_Infusion_MagSep")


for (i in dataset_name) { 
  filename = here("figures", "figure_data", str_c(i,".csv"))
  temp_data = as.name(i)
  write_csv(eval(temp_data), filename)
}
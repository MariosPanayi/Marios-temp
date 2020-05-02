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

#####
# Script gets wide format raw data and converts it into long format. Also adds Time Bin variable.
# Data resaved in long format, .csv file
# only need to run once
#####
#Expt 3.Locomotor Activity LY vs Amph - Food Dep
# Load Data baseline

full_data_wide <- read_csv("rawdata/WideFormatData/LY354740_Expt3_Locomotor_FoodDep_Habituation_Wide.csv")

full_data_long <- full_data_wide %>% 
  #gather("time_mins", "activity", 3:62)
  pivot_longer(3:62, names_to = "time_mins", values_to = "activity") %>% 
  mutate(
    time_mins = as.numeric(time_mins),
    bin5mins = ceiling(time_mins/5), bin5mins = bin5mins - max(bin5mins),
    bin10mins = ceiling(time_mins/10), bin10mins = bin10mins - max(bin10mins),
    bin15mins = ceiling(time_mins/15), bin15mins = bin15mins - max(bin15mins),
    bin20mins = ceiling(time_mins/20), bin20mins = bin20mins - max(bin20mins),
    bin30mins = ceiling(time_mins/30), bin30mins = bin30mins - max(bin30mins),
    bin60mins = ceiling(time_mins/60), bin60mins = bin60mins - max(bin60mins),
    )


full_data_long %>%   ggplot(mapping = aes(x = as.factor(bin10mins), y = activity, group = Drug, colour = Drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "point") +
  stat_summary_bin(fun.data = "mean_se", geom = "line") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.01) +
  scale_y_continuous( expand = expansion(mult = c(0, 0.1))) +
  theme_classic() 



write_csv(full_data_long, "rawdata/LY354740_Expt3_Locomotor_FoodDep_Habituation.csv")

#####
# Load Data Test

full_data_wide <- read_csv("rawdata/WideFormatData/LY354740_Expt3_Locomotor_FoodDep_Test_Wide.csv")

full_data_long <- full_data_wide %>% 
  #gather("time_mins", "activity", 3:182)
  pivot_longer(3:182, names_to = "time_mins", values_to = "activity") %>% 
  mutate(
    time_mins = as.numeric(time_mins),
    bin5mins = ceiling(time_mins/5),
    bin10mins = ceiling(time_mins/10),
    bin15mins = ceiling(time_mins/15),
    bin20mins = ceiling(time_mins/20),
    bin30mins = ceiling(time_mins/30),
    bin60mins = ceiling(time_mins/60)
  )


full_data_long %>%   ggplot(mapping = aes(x = as.factor(bin30mins), y = activity, group = Drug, colour = Drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "point") +
  stat_summary_bin(fun.data = "mean_se", geom = "line") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.01) +
  scale_y_continuous( expand = expansion(mult = c(0, 0.1))) +
  theme_classic() 

write_csv(full_data_long, "rawdata/LY354740_Expt3_Locomotor_FoodDep_Test.csv")


#####
#Expt 4.Locomotor Activity LY vs Amph - AdLib
# Load Data baseline

full_data_wide <- read_csv("rawdata/WideFormatData/LY354740_Expt4_Locomotor_AdLib_Habituation_Wide.csv")

full_data_long <- full_data_wide %>% 
  #gather("time_mins", "activity", 3:62)
  pivot_longer(3:62, names_to = "time_mins", values_to = "activity") %>% 
  mutate(
    time_mins = as.numeric(time_mins),
    bin5mins = ceiling(time_mins/5), bin5mins = bin5mins - max(bin5mins),
    bin10mins = ceiling(time_mins/10), bin10mins = bin10mins - max(bin10mins),
    bin15mins = ceiling(time_mins/15), bin15mins = bin15mins - max(bin15mins),
    bin20mins = ceiling(time_mins/20), bin20mins = bin20mins - max(bin20mins),
    bin30mins = ceiling(time_mins/30), bin30mins = bin30mins - max(bin30mins),
    bin60mins = ceiling(time_mins/60), bin60mins = bin60mins - max(bin60mins),
  )


full_data_long %>%   ggplot(mapping = aes(x = as.factor(bin10mins), y = activity, group = Drug, colour = Drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "point") +
  stat_summary_bin(fun.data = "mean_se", geom = "line") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.01) +
  scale_y_continuous( expand = expansion(mult = c(0, 0.1))) +
  theme_classic() 



write_csv(full_data_long, "rawdata/LY354740_Expt4_Locomotor_AdLib_Habituation.csv")

#####
# Load Data Test

full_data_wide <- read_csv("rawdata/WideFormatData/LY354740_Expt4_Locomotor_AdLib_Test_Wide.csv")

full_data_long <- full_data_wide %>% 
  #gather("time_mins", "activity", 3:182)
  pivot_longer(3:182, names_to = "time_mins", values_to = "activity") %>% 
  mutate(
    time_mins = as.numeric(time_mins),
    bin5mins = ceiling(time_mins/5),
    bin10mins = ceiling(time_mins/10),
    bin15mins = ceiling(time_mins/15),
    bin20mins = ceiling(time_mins/20),
    bin30mins = ceiling(time_mins/30),
    bin60mins = ceiling(time_mins/60)
  )


full_data_long %>%   ggplot(mapping = aes(x = as.factor(bin30mins), y = activity, group = Drug, colour = Drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "point") +
  stat_summary_bin(fun.data = "mean_se", geom = "line") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.01) +
  scale_y_continuous( expand = expansion(mult = c(0, 0.1))) +
  theme_classic() 

write_csv(full_data_long, "rawdata/LY354740_Expt4_Locomotor_AdLib_Test.csv")

#####

# Load data
baseline_data <- read_csv("rawdata/LY354740_Expt3_Locomotor_FoodDep_Habituation.csv")
baseline_data <- baseline_data %>% 
  mutate(Period = "habituation")
test_data <- read_csv("rawdata/LY354740_Expt3_Locomotor_FoodDep_Test.csv")
test_data <- test_data %>% 
  mutate(Period = "Test")

full_data <- full_join(baseline_data, test_data)

drugcoding <- tribble(
  ~Drug, ~Amph, ~LY,
  "Veh_Amph","Amph", "Veh",
  "LY_Amph","Amph", "LY",
  "Veh_Veh","Veh","Veh",
  "Veh_LY", "Veh","LY",
)

full_data <- inner_join(full_data, drugcoding)

write_csv(full_data, "rawdata/LY354740_Expt3_Locomotor_FoodDep.csv")




data_60mins <- full_data %>% 
  group_by(Subj, Drug, Period, bin60mins) %>% 
  summarise(activity = sum(activity))

data_5mins <- full_data %>% 
  group_by(Subj, Drug, Period, bin5mins) %>% 
  summarise(activity = sum(activity)) %>% 
  ungroup()


data_60mins %>%   ggplot(mapping = aes(x = as.factor(bin60mins), y = activity, group = Drug, colour = Drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "point") +
  stat_summary_bin(fun.data = "mean_se", geom = "line") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.01) +
  scale_y_continuous( expand = expansion(mult = c(0, 0.1))) +
  theme_classic() 

#####

# Load data
baseline_data <- read_csv("rawdata/LY354740_Expt4_Locomotor_AdLib_Habituation.csv")
baseline_data <- baseline_data %>% 
  mutate(Period = "habituation")
test_data <- read_csv("rawdata/LY354740_Expt4_Locomotor_AdLib_Test.csv")
test_data <- test_data %>% 
  mutate(Period = "Test")

full_data <- full_join(baseline_data, test_data)

drugcoding <- tribble(
  ~Drug, ~Amph, ~LY,
  "Veh_Amph","Amph", "Veh",
  "LY_Amph","Amph", "LY",
  "Veh_Veh","Veh","Veh",
  "Veh_LY", "Veh","LY",
)

full_data <- inner_join(full_data, drugcoding)

write_csv(full_data, "rawdata/LY354740_Expt4_Locomotor_AdLib.csv")

data_60mins %>%   ggplot(mapping = aes(x = as.factor(bin60mins), y = activity, group = Drug, colour = Drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "point") +
  stat_summary_bin(fun.data = "mean_se", geom = "line") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.01) +
  scale_y_continuous( expand = expansion(mult = c(0, 0.1))) +
  theme_classic() 



##### 
#Expt 5 Amph dose repsonse with Feeding manipulation Locomotort and DBS

#Locomotor

full_data_wide <- read_csv("rawdata/WideFormatData/Expt5_Feeding_AmphREsponseCurve_Locomotor_Wide.csv")

full_data_long <- full_data_wide %>% 
  pivot_longer(7:42, names_to = "bin5mins", values_to = "activity") %>% 
  mutate(
    bin5mins = as.numeric(bin5mins), bin5mins = bin5mins - 12,
    bin10mins = ceiling(bin5mins/2), 
    bin15mins = ceiling(bin5mins/3),
    bin20mins = ceiling(bin5mins/4),
    bin30mins = ceiling(bin5mins/6),
    bin60mins = ceiling(bin5mins/12),
    Amph = as.factor(Amph)
  )


full_data_long %>%   ggplot(mapping = aes(x = as.factor(bin10mins), y = activity, group = interaction(Amph, Feeding), colour = interaction(Amph, Feeding))) +
  stat_summary_bin(fun.data = "mean_se", geom = "point") +
  stat_summary_bin(fun.data = "mean_se", geom = "line") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.01) +
  scale_y_continuous( expand = expansion(mult = c(0, 0.1))) +
  theme_cowplot(12)



write_csv(full_data_long, "rawdata/LY354740_Expt5_Locomotor_FoodDepAmphDose.csv")

#DBS

full_data_wide <- read_csv("rawdata/WideFormatData/Expt5_Feeding_AmphREsponseCurve_DBS_Wide.csv")


full_data_long <- full_data_wide %>% 
  pivot_longer(5:8, names_to = "Time_hrs", values_to = "nM") %>% 
  mutate( Amph = as.factor(Amph)
  )


full_data_long %>%   ggplot(mapping = aes(x =Time_hrs, y = nM, group = interaction(Amph, Feeding), colour = interaction(Amph, Feeding))) +
  stat_summary_bin(fun.data = "mean_se", geom = "point") +
  stat_summary_bin(fun.data = "mean_se", geom = "line") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.01) +
  scale_y_continuous( expand = expansion(mult = c(0, 0.1))) +
  theme_cowplot(12)


write_csv(full_data_long, "rawdata/LY354740_Expt5_DBS_FoodDepAmphDose.csv")

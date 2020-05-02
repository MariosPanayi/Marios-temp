## Packages for data organisation and plotting
library(tidyverse)
#library(ggpubr)
library(cowplot)
################################################################################
## Packages for Data analysis
library(afex)
library(emmeans)
# install.packages("devtools")
# devtools::install_github("crsh/papaja")
library(papaja)
library(knitr)


full_data <- read_csv("rawdata/AwakeVarReward_rawdata.csv")
# summary(full_data)
# colnames(full_data)
# [1] "subject"        
# [2] "date"           
# [3] "drug"           
# [4] "channel"        
# [5] "uniqueID"       
# [6] "include"        
# [7] "rewardMagnitude"
# [8] "AUC"            
# [9] "peak"           
# [10] "latency2peak" 


full_data %>% 
  filter(include == TRUE) %>% 
  ggplot(mapping = aes(x = rewardMagnitude, y = peak, group = drug, fill= drug )) +
  stat_summary(fun.data = "mean_se", geom = "bar", position = "dodge", color = "black") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = "dodge") +
  scale_y_continuous( expand = expansion(mult = c(0, 0.1))) +
  theme_classic() 

full_data %>% 
  filter(include == TRUE) %>% 
  ggplot(mapping = aes(x = rewardMagnitude, y = AUC, group = drug, fill= drug )) +
  stat_summary(fun.data = "mean_se", geom = "bar", position = "dodge", color = "black") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = "dodge") +
  scale_y_continuous( expand = expansion(mult = c(0, 0.1))) +
  theme_classic() 

full_data %>% 
  filter(include == TRUE) %>% 
  ggplot(mapping = aes(x = rewardMagnitude, y = latency2peak, group = drug, fill= drug )) +
  stat_summary(fun.data = "mean_se", geom = "bar", position = "dodge", color = "black") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = "dodge") +
  scale_y_continuous( expand = expansion(mult = c(0, 0.1))) +
  theme_classic() 

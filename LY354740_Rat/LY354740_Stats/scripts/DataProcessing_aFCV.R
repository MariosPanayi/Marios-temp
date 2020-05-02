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


full_data <- read_csv("rawdata/Anaesthetized_rawdata.csv")

# summary(full_data)
# colnames(full_data)

# ExperimentalStage
# StageType
# stimPulses
# stimStrength
# drugs
# subj
# DA_max
# DA_AUC
# DA_Latency_s
# DA_max_PercBaseline
# DA_AUC_PercBaseline
# DA_Latency_s_PercBaseline


full_data %>% 
  filter(ExperimentalStage == "02_StimIntensity_Pre") %>% 
  ggplot(mapping = aes(x = stimStrength, y = DA_max, group = drugs, fill= drugs )) +
  stat_summary(fun.data = "mean_se", geom = "bar", position = "dodge", color = "black") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = "dodge") +
  scale_y_continuous( expand = expansion(mult = c(0, 0.1))) +
theme_classic() 
  

##### Data analysis ##### 

data_stim_intensity_pre <- full_data %>% 
  filter(ExperimentalStage == "02_StimIntensity_Pre")

anova_stim_intensity_pre <- aov_4(DA_max ~ drugs + (stimStrength|subj), data = data_stim_intensity_pre, anova_table = list(correction = "none", es = "pes"))

anova_stim_intensity_pre_print <- apa_print(anova_stim_intensity_pre, mse = "FALSE",correction = "none",es = "pes")


simple_stim_intensity_pre <- emmeans(anova_stim_intensity_pre, ~stimStrength)
simple_stim_intensity_pre_print <- apa_print.emmGrid(pairs(simple_stim_intensity_pre))



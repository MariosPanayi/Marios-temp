# Load Packagaes ----------------------------------------------------------
## Packages for data organisation and plotting
library(tidyverse)
library(knitr)
library(data.table)
# Package for relative file paths
library(here)

library(cowplot)


# Load Data ---------------------------------------------------------------

rawdata <- fread(here("data", "mouseData.csv"))

# Recode data

rawdata <- rawdata %>% 
  mutate( actualsignal = ifelse(mapping_1 == 1, "NoSignal", ifelse(mapping_2 == 1, "Signal", "Other")) ) %>% 
  mutate( choice = recode(choice, "0" = "NoSignal", "1" = "Signal"),
          ROC = ifelse(choice == "NoSignal" & actualsignal == "NoSignal", "CorrectRejection", 
                       ifelse(choice == "NoSignal" & actualsignal == "Signal", "Miss", 
                              ifelse(choice == "Signal" & actualsignal == "NoSignal", "FalseAlarm", 
                                     ifelse(choice == "Signal" & actualsignal == "Signal", "CorrectDetection", "Other")))),
  ) 
     
rawdata %>% 
  filter(!is.na(blockBias) & !is.na(ROC)) %>% 
ggplot(mapping = aes(x = confidence, group = ROC, colour = ROC, fill = ROC, shape = ROC,linetype = ROC))+
  facet_wrap(~ blockBias * ROC) +
  geom_histogram(aes(y = stat(count / sum(count))))
                                 
# Filter data
data_baseline <- rawdata %>% 
  filter(blockBias != "NaN", 
         ketamine == "NaN", 
         optogenetics == "NaN", 
         haloperidol == "NaN")



# Recode data
summary_overall <- data_baseline %>% 
 group_by(subjectId, blockBias, choice, actualsignal, ROC) %>% 
  count()


summary_percentage <- summary_overall %>% 
  group_by(subjectId, blockBias, actualsignal) %>% 
  filter(!is.na(choice) ) %>% 
  mutate(ROC_Percent = n/sum(n) * 100)
    
summary_percentage %>% 
  filter(ROC == "FalseAlarm") %>% 
  group_by( blockBias) %>% 
  summarise(mean(ROC_Percent))
    
summary_percentage %>% 
  # filter(ROC == "FalseAlarm") %>%
  ggplot(mapping = aes(x = as.factor(blockBias), y = ROC_Percent, group = ROC, colour = ROC, fill = ROC, shape = ROC,linetype = ROC)) +
  # facet_wrap(~ sex) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # facet_wrap(~counterbalancing, ) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,10)) +
  coord_cartesian(ylim = c(0,100)) +
  ggtitle("blockBias Sessions") + xlab("Signal Proportion") + ylab("Percentage") +
  theme_cowplot(11)

sum(summary_overall$n)



                            
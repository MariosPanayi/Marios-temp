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

# Filter data
data_baseline <- rawdata %>% 
  filter(!is.na(ROC),
         blockBias == "NaN", 
         ketamine == "NaN", 
         optogenetics == "NaN", 
         haloperidol == "NaN")
     
data_baseline %>% 
  filter(!is.na(ROC),
         # ROC == "FalseAlarm" | 
         # ROC == "CorrectDetection",
         catchtrial == 1,
         ) %>% 
ggplot(mapping = aes(x = confidence, group = ROC, colour = ROC, fill = ROC, shape = ROC,linetype = ROC))+
  geom_histogram() + 
  facet_wrap(~ actualsignal, ) 

medianconfidence <- data_baseline %>% 
group_by(subjectId, ROC, blockBias) %>% 
  summarise(medianConfidence = median(confidence))

medianconfidencecatchtrials <- data_baseline %>% 
  filter(catchtrial == 1) %>% 
  group_by(subjectId, ROC, blockBias) %>% 
  summarise(medianConfidence = median(confidence))

medianconfidencecatchtrials <- data_baseline %>% 
  filter(catchtrial == 1) %>% 
  group_by(ROC, subjectId) %>% 
  summarise(medianConfidence = median(confidence),
            abovemedian = sum(confidence > median(confidence)),
            above54median = sum(confidence > 5.4469) ,
            totaltrials = sum(confidence >0 ) ,
            percTrialsAbove = (above54median/totaltrials) *100 ) 
         

medianconfidence %>% 
  filter( ROC == "FalseAlarm" | ROC == "Miss") %>% 
ggplot(mapping = aes(x = ROC, y = medianConfidence, group = ROC, colour = ROC, fill = ROC))+
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  geom_line(aes(group = subjectId), colour = "darkslategrey") +
  facet_wrap(~blockBias, ) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,.2)) +
  coord_cartesian(ylim = c(2,7)) +
  ggtitle("Block BIas") + xlab("Error Type") + ylab("Median Confidence (s)") +
  theme_cowplot(11)

medianconfidencecatchtrials %>% 
  # filter( ROC == "FalseAlarm" | ROC == "Miss") %>% 
  ggplot(mapping = aes(x = ROC, y = medianConfidence, group = ROC, colour = ROC, fill = ROC))+
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  geom_line(aes(group = subjectId), colour = "darkslategrey") +
  facet_wrap(~blockBias, ) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,.2)) +
  coord_cartesian(ylim = c(2,7)) +
  ggtitle("Block BIas") + xlab("Error Type") + ylab("Median Confidence (s)") +
  theme_cowplot(11)

sum(summary_overall$n)


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



                            
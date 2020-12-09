# Load Packages  ----------------------------------------------------------

## Packages for data organisation and plotting
library(tidyverse)
#library(ggpubr)
library(cowplot)

#### Packages for Data analysis
library(afex)
afex_options(emmeans_model = "multivariate") # use multivariate model for all follow-up tests.
library(emmeans)
# install.packages("devtools")
# devtools::install_github("crsh/papaja")
library(papaja)
library(knitr)
library(here)


# SKR214_Inst_Acquisition ---------------------------------------------------------------

# Load Data
full_data <- read_csv(here("rawdata", "SKR214_Inst_Acquisition.csv"))

# Check that there are equal numbers of entries per subject/reward/day
full_data %>% 
  group_by(Subject,RewardName, Day) %>% 
  summarise(n = n()) %>% 
  spread(Day, n)%>%
  kable()



# Summarise data into averages per session/day
summary <- full_data %>% 
  group_by(Experiment, Schedule, Day, Subject, RewardName, LeverName) %>% 
  summarise(LP = sum(`LP Bin`),
            Rewards = sum(`Rewards Bin`),
            SessionLength = median(`Session Length`[`Session Length` != 0]),
            LP_rate_min = (LP/SessionLength) *60,
            MagEntry = sum(`MagEntry Bin`),
            MagEntry_rate_min = (MagEntry/SessionLength) *60,
  ) %>% 
  ungroup()

# Check that there is only a single value per subject/reward/day
summary %>% 
  group_by(Subject,RewardName, Day) %>% 
  summarise(n = n()) %>% 
  spread(Day, n)%>%
  kable()


# Quick plot to verify data
  
summary %>% ggplot(mapping = aes(x = as.factor(Day), y = LP_rate_min, colour = RewardName, group = RewardName)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5) +
  stat_summary(fun = "mean", geom = "point", size = 3) +
  stat_summary(fun = "mean", geom = "line", size = 0.5) +
  coord_cartesian(ylim = c(0,40)) +
  scale_y_continuous(breaks = seq(0,40,by = 5), expand = c(0.0,0)) +
  ggtitle("Instrumental Acquisition",subtitle = "SKR214 - Two Lever Design") + ylab("LP/min") + xlab("Day") +
  theme_cowplot()

summary %>% ggplot(mapping = aes(x = as.factor(Day), y = MagEntry_rate_min, colour = RewardName, group = RewardName)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5) +
  stat_summary(fun = "mean", geom = "point", size = 3) +
  stat_summary(fun = "mean", geom = "line", size = 0.5) +
  coord_cartesian(ylim = c(0,40)) +
  scale_y_continuous(breaks = seq(0,40,by = 5), expand = c(0.0,0)) +
  ggtitle("Instrumental Acquisition",subtitle = "SKR214 - Two Lever Design") + ylab("Magazine Entries/min") + xlab("Day") +
  theme_cowplot()



# Check for bias/reward preference in lever pressing rates
summary_wide_LP <- summary %>% 
  select(Experiment, Schedule, Day, Subject, RewardName, LP_rate_min) %>% 
  spread(RewardName, LP_rate_min) %>% 
  filter(Schedule == "RR10") %>% 
  mutate(MaltBias = maltodextrin - Sucrose,
         zdiff = (MaltBias - mean(MaltBias, na.rm = TRUE))/sd(MaltBias, na.rm = TRUE),
         malt_suc_ratio = maltodextrin / Sucrose)
  
summary_wide_LP %>% 
  filter(Day == 8) %>% 
  ggplot(mapping = aes(x = malt_suc_ratio, fill = as.factor(Day), group = as.factor(Day), alpha = .8)) +
  geom_histogram(binwidth = .1, show.legend = FALSE) + 
  geom_density(show.legend = FALSE)+
  theme_cowplot()

summary_wide_LP %>% 
  filter(Day == 8) %>% 
  ggplot(mapping = aes(x = zdiff, fill = as.factor(Day), group = as.factor(Day), alpha = .8)) +
  geom_histogram(binwidth = .1, show.legend = FALSE) + 
  geom_density(show.legend = FALSE)+
  theme_cowplot()

# SKR214_Pav_Freq_Acquisition  ----------------------------------------------------------------------

# Load Data
full_data <- read_csv(here("rawdata", "SKR214_Pav_Freq_Acquisition.csv"))

# Check that there are equal numbers of entries per subject/day
full_data %>% 
  group_by(Subject, Day) %>% 
  summarise(n = n()) %>% 
  spread(Day, n)%>%
  kable()

# Remove unnecessary calculation columns
full_data <- full_data %>% 
  select(`Day`,                                                    
         `MSN`,                                                    
         `StartDate`,                                              
         `StartTime`,                                              
         `Experiment`,                                             
         `Group`,                                                  
         `Box`,                                                    
         `Subject`,                                                
         `Total MagEntries`,                                       
         `Total MagDuration`,                                      
         `Total Rewards`,                                          
         `Total Session Time`,                                     
         `Trial Number`,                                           
         `ITI_Duration(minus PreCS)`,                              
         `CS_Identity [1 = Noise, 2 = Click, 3 = tone]`,           
         `US_Identity [1 = Sucrose, 2 = Maltodextrin, 3 = Pellet]`,
         `Number of USs`,                                          
         `MagEntry_ITI`,
         `Latency to first magazine Entry`,                        
         `First response was an Exit`,                             
         `MagEntry_PreCS (2mins)`,                                 
         `MagEntry_CS (2 mins)`,                                   
         `CS-PreCS`,                                               
         `Trial Number, 1 CS`,                                     
         `CS_Name`,                                                
         `US_Name`,                                                
         `MagEntriesCS excluding reward delivery 5s`,              
         `CS_Duration (s)_NoReward5s`,                             
         `Devalued Reward 1st test`,                               
         `Devalued?`)

# Summarise data into averages per Outcome ID, rates per trial (2 mins)
summary <- full_data %>% 
  group_by(Experiment, Day, Subject, CS_Name, US_Name) %>% 
  summarise(AvgRewards = mean(`Number of USs`),
            CS_PreCS = mean(`CS-PreCS`),
            MagEntry_PreCS = mean(`MagEntry_PreCS (2mins)`),
            MagEntry_CS = mean(`MagEntry_CS (2 mins)`)
  ) %>% 
  ungroup()

# Check that there are equal numbers of trial types/subject
summary %>% 
  group_by(Subject, Day) %>% 
  summarise(n = n()) %>% 
  spread(Day, n)%>%
  kable()

# Quick Plot to verify data
summary %>% ggplot(mapping = aes(x = as.factor(Day), y = CS_PreCS, colour = US_Name, group = US_Name, shape = US_Name)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5) +
  stat_summary(fun = "mean", geom = "point", size = 3) +
  stat_summary(fun = "mean", geom = "line", size = 0.5) +
  coord_cartesian(ylim = c(0,30)) +
  scale_y_continuous(breaks = seq(0,30,by = 5), expand = c(0.0,0)) +
  ggtitle("Pavlovian Acquisition",subtitle = "SKR214 - Magazine Frequency") + ylab("Mag Entries/2 mins") + xlab("Day") +
  theme_cowplot()

summary %>% ggplot(mapping = aes(x = as.factor(Day), y = CS_PreCS, colour = CS_Name, group = CS_Name, shape = CS_Name)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5) +
  stat_summary(fun = "mean", geom = "point", size = 3) +
  stat_summary(fun = "mean", geom = "line", size = 0.5) +
  coord_cartesian(ylim = c(0,30)) +
  scale_y_continuous(breaks = seq(0,30,by = 5), expand = c(0.0,0)) +
  ggtitle("Pavlovian Acquisition",subtitle = "SKR214 - Magazine Frequency") + ylab("Mag Entries/2 mins") + xlab("Day") +
  theme_cowplot()

# Check for response bias to maltodextrin vs sucrose
summary_CS_bias <- summary %>% 
  select(-CS_Name, -MagEntry_PreCS, -MagEntry_CS, -AvgRewards) %>% 
  filter(US_Name != "Pellet") %>% 
  pivot_wider(names_from = US_Name, values_from = CS_PreCS) %>% 
  mutate(MaltBias = Maltodextrin - Sucrose,
         zdiff = (MaltBias - mean(MaltBias, na.rm = TRUE))/sd(MaltBias, na.rm = TRUE),
         malt_suc_ratio = Maltodextrin / Sucrose)

summary_CS_bias %>% 
  filter(Day == 8) %>% 
  ggplot(mapping = aes(x = log(malt_suc_ratio), fill = as.factor(Day), group = as.factor(Day), alpha = .8)) +
  geom_histogram(binwidth = .1, show.legend = FALSE) + 
  geom_density(show.legend = FALSE)+
  theme_cowplot()

summary_CS_bias %>% 
  filter(Day == 8) %>% 
  ggplot(mapping = aes(x = zdiff, fill = as.factor(Day), group = as.factor(Day), alpha = .8)) +
  geom_histogram(binwidth = .1, show.legend = FALSE) + 
  geom_density(show.legend = FALSE)+
  theme_cowplot()

summary_CS_bias %>% 
  filter(Day == 8) %>% 
  select(Subject, zdiff, malt_suc_ratio) %>% 
  mutate(log_malt_suc_ratio = log(malt_suc_ratio)) %>% 
  kable()

# SKR214_Pav_Dur_Acquisition  ----------------------------------------------------------------------

# Load Data
full_data <- read_csv(here("rawdata", "SKR214_Pav_Dur_Acquisition.csv"))

# Check that there are equal numbers of entries per subject/day
full_data %>% 
  group_by(Subject, Day) %>% 
  summarise(n = n()) %>% 
  spread(Day, n)%>%
  kable()

# Remove unnecessary calculation columns
full_data <- full_data %>% 
  select(`Day`,                                                    
         `MSN`,                                                    
         `StartDate`,                                              
         `StartTime`,                                              
         `Experiment`,                                             
         `Group`,                                                  
         `Box`,                                                    
         `Subject`,                                                
         `Total MagEntries`,                                       
         `Total MagDuration`,                                      
         `Total Rewards`,                                          
         `Total Session Time`,                                     
         `Trial Number`,                                           
         `ITI_Duration(minus PreCS)`,                              
         `CS_Identity [1 = Noise, 2 = Click, 3 = tone]`,           
         `US_Identity [1 = Sucrose, 2 = Maltodextrin, 3 = Pellet]`,
         `Number of USs`,                                          
         `MagEntry_ITI`,
         `Latency to first magazine Entry`,                        
         `First response was an Exit`,                             
         `MagDuration_PreCS/s (2mins)`,                                 
         `MagDuration_CS/s (2 mins)`,                                   
         `CS-PreCS`,                                               
         `Trial Number, 1 CS`,                                     
         `CS_Name`,                                                
         `US_Name`,                                                
         `Devalued reward first test`,              
         `Devalued CS first test?`)

# Summarise data into averages per Outcome ID, rates per trial (2 mins)
summary <- full_data %>% 
  group_by(Experiment, Day, Subject, CS_Name, US_Name) %>% 
  summarise(AvgRewards = mean(`Number of USs`),
            CS_PreCS = mean(`CS-PreCS`),
            MagDur_PreCS = mean(`MagDuration_PreCS/s (2mins)`),
            MagDur_CS = mean(`MagDuration_CS/s (2 mins)`)
  ) %>% 
  ungroup()

# Check that there are equal numbers of trial types/subject
summary %>% 
  group_by(Subject, Day) %>% 
  summarise(n = n()) %>% 
  spread(Day, n)%>%
  kable()

# Quick Plot to verify data
summary %>% ggplot(mapping = aes(x = as.factor(Day), y = CS_PreCS, colour = US_Name, group = US_Name, shape = US_Name)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5) +
  stat_summary(fun = "mean", geom = "point", size = 3) +
  stat_summary(fun = "mean", geom = "line", size = 0.5) +
  coord_cartesian(ylim = c(0,55)) +
  scale_y_continuous(breaks = seq(0,55,by = 5), expand = c(0.0,0)) +
  ggtitle("Pavlovian Acquisition",subtitle = "SKR214 - Magazine Frequency") + ylab("Mag Entries/2 mins") + xlab("Day") +
  theme_cowplot()

summary %>% ggplot(mapping = aes(x = as.factor(Day), y = CS_PreCS, colour = CS_Name, group = CS_Name, shape = CS_Name)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5) +
  stat_summary(fun = "mean", geom = "point", size = 3) +
  stat_summary(fun = "mean", geom = "line", size = 0.5) +
  coord_cartesian(ylim = c(0,55)) +
  scale_y_continuous(breaks = seq(0,55,by = 5), expand = c(0.0,0)) +
  ggtitle("Pavlovian Acquisition",subtitle = "SKR214 - Magazine Frequency") + ylab("Mag Entries/2 mins") + xlab("Day") +
  theme_cowplot()



# SKR214_Satiety_consumption ----------------------------------------------

# Load Data
full_data <- read_csv(here("rawdata", "SKR214_Satiety_consumption.csv"))

# Check that there are equal numbers of entries per subject/day
full_data %>% 
  group_by(Rat, TestNumber) %>% 
  summarise(n = n()) %>% 
  spread(TestNumber, n)%>%
  kable()

# Quick Plot to verify data
full_data %>% ggplot(mapping = aes(x = as.factor(TestNumber), y = `Consumed (g)`, colour = Liquid, group = Liquid, shape = Liquid)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5) +
  stat_summary(fun = "mean", geom = "point", size = 3) +
  stat_summary(fun = "mean", geom = "line", size = 0.5) +
  coord_cartesian(ylim = c(0,30)) +
  scale_y_continuous(breaks = seq(0,30,by = 5), expand = c(0.0,0)) +
  ggtitle("Satiety",subtitle = "SKR214 - Consumption") + ylab("Consumed (g)") + xlab("Satiety Test Number") +
  theme_cowplot()


# SKR214_LiCl_TasteAversion -----------------------------------------------

# Load Data
full_data <- read_csv(here("rawdata", "SKR214_LiCl_TasteAversion.csv"))

# Check that there are equal numbers of entries per subject/day
full_data %>% 
  group_by(Rat, Pairing, Injection) %>% 
  summarise(n = n()) %>% 
  spread(Rat, n)%>%
  kable()

# Quick Plot to verify data
full_data %>% ggplot(mapping = aes(x = as.factor(Pairing), y = `Consumed (g)`, colour = Injection, group = Injection, shape = Injection)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5) +
  stat_summary(fun = "mean", geom = "point", size = 3) +
  stat_summary(fun = "mean", geom = "line", size = 0.5) +
  coord_cartesian(ylim = c(0,30)) +
  scale_y_continuous(breaks = seq(0,30,by = 5), expand = c(0.0,0)) +
  ggtitle("LiCl",subtitle = "SKR214 - Consumption") + ylab("Consumed (g)") + xlab("Injection Pairing Number") +
  theme_cowplot()


# SKR214_DevaluationinMag_LiCl --------------------------------------------

# Load Data
full_data <- read_csv(here("rawdata", "SKR214_DevaluationinMag_LiCl.csv"))

summary <- full_data %>% 
  mutate(Liquid = case_when(str_detect(MSN,"sucrose") ~ "Sucrose",
                            str_detect(MSN,"Maltodextrin") ~ "Maltodextrin")
  ) %>% 
  select(Injection, Liquid, Subject, Bin_4mins, Mag1_5sBins, Mag1_duration_5sbins) %>% 
  group_by(Injection, Subject, Liquid, Bin_4mins) %>% 
  summarise(Bin_4mins = median(Bin_4mins),
            MagEntry = sum(Mag1_5sBins),
            MagDur = sum(Mag1_duration_5sbins)/100) %>% 
  ungroup() %>% 
  filter(Bin_4mins < 7)

# Quick Plot to verify data
summary %>%
  # filter(Subject != 8) %>% 
  ggplot(mapping = aes(x = as.factor(Bin_4mins), y = MagEntry, colour = Injection, group = Injection, fill = Injection)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5) +
  stat_summary(fun = "mean", geom = "point", size = 3) +
  stat_summary(fun = "mean", geom = "line", size = 0.5) +
  coord_cartesian(ylim = c(0,40)) +
  scale_y_continuous(breaks = seq(0,40,by = 5), expand = c(0.0,0)) +
  ggtitle("LiCl",subtitle = "SKR21w - Taste Aversion in Chamber") + ylab("MagEntries/4mins") + xlab("Bin 4 Mins") +
  theme_cowplot()



# SKR214_sPIT_Test --------------------------------------------------------

# Load Data
full_data <- read_csv(here("rawdata", "SKR214_sPIT_Test.csv"))


## 1. Satiety Deval Extinction - Pre test  --------------------------------------------------------

summary <- full_data %>% 
  filter(`Trial Structure` == "Baseline Extinction",
         Devaluation == "LiCl") %>% 
  group_by(Devaluation, Group, Session, Subject, `Minute Counter`) %>% 
  summarise(`Devalued Lever` = mean(`Devalued Lever`)*60,
         `Non-Devalued Lever` = mean(`Non-Devalued Lever`)*60,
         MagEntries = mean(MagEntries)*60,
         # MagDuration = mean(MagDuration)*60 # MagDuration not recorded in these data
         ) %>% 
  ungroup()


summary %>% ggplot(mapping = aes(x = as.factor(`Minute Counter`), y = MagEntries, colour = Group, group = Group)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5) +
  stat_summary(fun = "mean", geom = "point", size = 3) +
  stat_summary(fun = "mean", geom = "line", size = 0.5) +
  coord_cartesian(ylim = c(0,30)) +
  scale_y_continuous(breaks = seq(0,30,by = 5), expand = c(0.0,0)) +
  ggtitle("Satiety Test",subtitle = "SKR214 - MagEntries") + ylab("MagEntries/min") + xlab("Minute") +
  theme_cowplot()

summary_LP <- summary %>% 
  select(Session, Subject, Group, `Minute Counter`, `Devalued Lever`, `Non-Devalued Lever`) %>% 
  gather(key = "Devaluation", value = "lever Presses", `Devalued Lever`, `Non-Devalued Lever`)

# Check that there are equal numbers of entries per subject/day
summary_LP %>% 
  group_by(Subject, Session) %>% 
  summarise(n = n()) %>% 
  spread(Subject, n)%>%
  kable()

summary_LP %>% ggplot(mapping = aes(x = as.factor(`Minute Counter`), y = `lever Presses`, colour = Devaluation, group = Devaluation)) +
  # facet_wrap(~ Session) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5) +
  stat_summary(fun = "mean", geom = "point", size = 3) +
  stat_summary(fun = "mean", geom = "line", size = 0.5) +
  coord_cartesian(ylim = c(0,15)) +
  scale_y_continuous(breaks = seq(0,15,by = 5), expand = c(0.0,0)) +
  ggtitle("Satiety Test",subtitle = "SKR214 - Lever Pressing") + ylab("Lever Presses/min") + xlab("Minute") +
  theme_cowplot()

## 2. Satiety Deval sPIT test  --------------------------------------------------------
# Load Data
full_data <- read_csv(here("rawdata", "SKR214_sPIT_Test.csv"))

summary <- full_data %>% 
  filter(
    `Trial Structure` == "PreCS" | `Trial Structure` == "CS",
    `trial period split minutes [1,2]` == "PreCS2"|`trial period split minutes [1,2]` == "CS1",
    # `trial period split minutes [1,2]` == "PreCS1" | `trial period split minutes [1,2]` == "PreCS2"|`trial period split minutes [1,2]` == "CS1"|`trial period split minutes [1,2]` == "CS2",
     # `Trial Number` == 1|`Trial Number` == 2,
         Devaluation == "LiCl"
    ) %>% 
  group_by(Devaluation, Group, Session, Subject, `Trial Structure`, `Devalued Pavlovian US?`) %>% 
  summarise(`Devalued Lever` = mean(`Devalued Lever`)*60,
            `Non-Devalued Lever` = mean(`Non-Devalued Lever`)*60,
            MagEntries = mean(MagEntries)*60,
            # MagDuration = mean(MagDuration)*60 # MagDuration not recorded in these data
  ) %>% 
  ungroup()

summary %>% ggplot(mapping = aes(x = `Trial Structure`, y = MagEntries, colour = `Devalued Pavlovian US?`, group = `Devalued Pavlovian US?`)) +
  # facet_wrap(~ Session) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5) +
  stat_summary(fun = "mean", geom = "point", size = 3) +
  stat_summary(fun = "mean", geom = "line", size = 0.5) +
  coord_cartesian(ylim = c(0,7)) +
  scale_y_continuous(breaks = seq(0,7,by = 1), expand = c(0.0,0)) +
  ggtitle("Satiety Test",subtitle = "SKR214 - Lever Pressing") + ylab("MagEntries/min") + xlab("Minute") +
  theme_cowplot()



summary_LP <- summary %>% 
  select(-MagEntries, -Devaluation) %>% 
  pivot_longer( cols =  `Devalued Lever`:`Non-Devalued Lever`, names_to = "Devaluation", values_to = "lever Presses") 

summary_LP %>% ggplot(mapping = aes(x = `Devalued Pavlovian US?`, y = `lever Presses`, colour = Devaluation, group = Devaluation, fill = Devaluation)) +
  facet_wrap(~ `Trial Structure`) +
  stat_summary(fun = "mean", geom = "bar", size = 3, position = position_dodge(width = .9)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5, position = position_dodge(width = 0.9), colour = "black") +
  coord_cartesian(ylim = c(0,6)) +
  scale_y_continuous(breaks = seq(0,6,by = 1), expand = c(0.0,0)) +
  ggtitle("Satiety Test",subtitle = "SKR214 - Lever Pressing") + ylab("Lever Presses/min") + xlab("Minute") +
  theme_cowplot()

summary_LP_wide <- summary_LP %>% 
  pivot_wider(names_from = `Trial Structure`, values_from = `lever Presses`) 

summary_LP_PreCSonly <- summary_LP_wide %>% 
  select(-CS) %>% 
  group_by(Group, Session, Subject, Devaluation) %>% 
  summarise(PreCS = mean(PreCS)) %>%
  ungroup()

summary_LP_PreCSonly <- summary_LP_PreCSonly %>% 
  pivot_longer(cols = PreCS, names_to = "Trial Structure", values_to = "lever Presses") %>% 
  mutate("Devalued Pavlovian US?" = "PreCS")
  
summary_LP_commonPreCS <- summary_LP %>% 
  filter(`Trial Structure` == "CS") %>% 
  full_join(summary_LP_PreCSonly)

summary_LP_commonPreCS <- summary_LP_commonPreCS %>% 
 unite("same_diff",c("Devalued Pavlovian US?", "Devaluation"), sep = "_", remove = FALSE) %>% 
  mutate(same_diff = str_replace(string = same_diff, pattern = "Non-Devalued_Non-Devalued Lever",replacement = "Same"),
         same_diff = str_replace(string = same_diff, pattern = "Devalued_Non-Devalued Lever",replacement = "Different"),
         same_diff = str_replace(string = same_diff, pattern = "Non-Devalued_Devalued Lever",replacement = "Different"),
         same_diff = str_replace(string = same_diff, pattern = "Devalued_Devalued Lever",replacement = "Same"),
         same_diff = str_replace(string = same_diff, pattern = "General_Devalued Lever",replacement = "General"),
         same_diff = str_replace(string = same_diff, pattern = "General_Non-Devalued Lever",replacement = "General"),
         same_diff = str_replace(string = same_diff, pattern = "PreCS_Devalued Lever",replacement = "PreCS"),
         same_diff = str_replace(string = same_diff, pattern = "PreCS_Non-Devalued Lever",replacement = "PreCS"))

summary_LP_commonPreCS %>% ggplot(mapping = aes(x = `Devalued Pavlovian US?`, y = `lever Presses`, colour = Devaluation, group = Devaluation, fill = Devaluation)) +
  # facet_wrap(~ `Trial Structure`) +
  stat_summary(fun = "mean", geom = "bar", size = 3, position = position_dodge(width = .9)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5, position = position_dodge(width = 0.9), colour = "black") +
  coord_cartesian(ylim = c(0,7)) +
  scale_y_continuous(breaks = seq(0,7,by = 1), expand = c(0.0,0)) +
  ggtitle("Satiety Test",subtitle = "SKR214 - Lever Pressing") + ylab("Lever Presses/min") + xlab("CS") +
  theme_cowplot()

summary_LP_commonPreCS %>% ggplot(mapping = aes(x = Devaluation, y = `lever Presses`, colour = `Devalued Pavlovian US?`, group = `Devalued Pavlovian US?`, fill = `Devalued Pavlovian US?`)) +
  # facet_wrap(~ `Trial Structure`) +
  stat_summary(fun = "mean", geom = "bar", size = 3, position = position_dodge(width = .9)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5, position = position_dodge(width = 0.9), colour = "black") +
  coord_cartesian(ylim = c(0,7)) +
  scale_y_continuous(breaks = seq(0,7,by = 1), expand = c(0.0,0)) +
  ggtitle("Satiety Test",subtitle = "SKR214 - Lever Pressing") + ylab("Lever Presses/min") + xlab("Lever") +
  theme_cowplot()

summary_LP_commonPreCS %>% ggplot(mapping = aes(x = Devaluation, y = `lever Presses`, colour =same_diff, group =same_diff, fill =same_diff)) +
  # facet_wrap(~ `Trial Structure`) +
  stat_summary(fun = "mean", geom = "bar", size = 3, position = position_dodge(width = .9)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5, position = position_dodge(width = 0.9), colour = "black") +
  coord_cartesian(ylim = c(0,7)) +
  scale_y_continuous(breaks = seq(0,7,by = 1), expand = c(0.0,0)) +
  ggtitle("Satiety Test",subtitle = "SKR214 - Lever Pressing") + ylab("Lever Presses/min") + xlab("Lever") +
  theme_cowplot()

# Same vs Different Lever comparison -> ignores the overwhelming effect of Pav and Inst Devaluation

# Load Data
full_data <- read_csv(here("rawdata", "SKR214_sPIT_Test.csv"))

summary <- full_data %>% 
  filter(
    `Trial Structure` == "PreCS" | `Trial Structure` == "CS",
    # `trial period split minutes [1,2]` == "PreCS1" | `trial period split minutes [1,2]` == "PreCS2"|`trial period split minutes [1,2]` == "CS1"|`trial period split minutes [1,2]` == "CS2",
    # `Trial Number` == 1|`Trial Number` == 2,
    Devaluation == "Satiety",
    `Devalued Pavlovian US?` == "Non-Devalued" | `Devalued Pavlovian US?` ==  "Devalued"
  ) %>% 
  group_by(Devaluation, Group, Session, Subject, `Trial Structure`, `Devalued Pavlovian US?`) %>% 
  summarise(Same = mean(as.numeric(`Same Lever`))*60,
            Different = mean(as.numeric(`Different Lever`))*60
  ) %>% 
  ungroup()


summary_LP <- summary %>% 
  select( -Devaluation) %>% 
  pivot_longer( cols =  Same:Different, names_to = "Lever", values_to = "lever Presses") 

summary_LP %>% ggplot(mapping = aes(x = `Devalued Pavlovian US?`, y = `lever Presses`, colour = Lever, group = Lever, fill = Lever)) +
  facet_wrap(~ `Trial Structure`) +
  stat_summary(fun = "mean", geom = "bar", size = 3, position = position_dodge(width = .9)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5, position = position_dodge(width = 0.9), colour = "black") +
  coord_cartesian(ylim = c(0,6)) +
  scale_y_continuous(breaks = seq(0,6,by = 1), expand = c(0.0,0)) +
  ggtitle("Satiety Test",subtitle = "SKR214 - Lever Pressing") + ylab("Lever Presses/min") + xlab("CS") +
  theme_cowplot()




summary_LP_wide <- summary_LP %>% 
  pivot_wider(names_from = `Trial Structure`, values_from = `lever Presses`) 

summary_LP_PreCSonly <- summary_LP_wide %>% 
  select(-CS) %>% 
  group_by(Group, Session, Subject, Lever) %>% 
  summarise(PreCS = mean(PreCS)) %>%
  ungroup()

summary_LP_PreCSonly <- summary_LP_PreCSonly %>% 
  pivot_longer(cols = PreCS, names_to = "Trial Structure", values_to = "lever Presses") %>% 
  mutate("Devalued Pavlovian US?" = "PreCS")

summary_LP_commonPreCS <- summary_LP %>% 
  filter(`Trial Structure` == "CS") %>% 
  full_join(summary_LP_PreCSonly)



summary_LP_commonPreCS %>% ggplot(mapping = aes(x = `Devalued Pavlovian US?`, y = `lever Presses`, colour = Lever, group = Lever, fill = Lever)) +
  # facet_wrap(~ `Trial Structure`) +
  stat_summary(fun = "mean", geom = "bar", size = 3, position = position_dodge(width = .9)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5, position = position_dodge(width = 0.9), colour = "black") +
  coord_cartesian(ylim = c(0,7)) +
  scale_y_continuous(breaks = seq(0,7,by = 1), expand = c(0.0,0)) +
  ggtitle("Satiety Test",subtitle = "SKR214 - Lever Pressing") + ylab("Lever Presses/min") + xlab("CS") +
  theme_cowplot()

summary_LP_commonPreCS %>% ggplot(mapping = aes(x = Lever, y = `lever Presses`, colour = `Devalued Pavlovian US?`, group = `Devalued Pavlovian US?`, fill = `Devalued Pavlovian US?`)) +
  # facet_wrap(~ `Trial Structure`) +
  stat_summary(fun = "mean", geom = "bar", size = 3, position = position_dodge(width = .9)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5, position = position_dodge(width = 0.9), colour = "black") +
  coord_cartesian(ylim = c(0,7)) +
  scale_y_continuous(breaks = seq(0,7,by = 1), expand = c(0.0,0)) +
  ggtitle("Satiety Test",subtitle = "SKR214 - Lever Pressing") + ylab("Lever Presses/min") + xlab("CS") +
  theme_cowplot()



# SKR216 ------------------------------------------------------------------

# SKR216_Inst_Acquisition -------------------------------------------------

# Load Data
full_data <- read_csv(here("rawdata", "SKR216_Inst_Acquisition.csv"))

# Check that there are equal numbers of entries per subject/reward/day
full_data %>% 
  group_by(Subject,RewardName, Day) %>% 
  summarise(n = n()) %>% 
  spread(Day, n)%>%
  kable()

# Summarise data into averages per session/day
summary <- full_data %>% 
  group_by(Experiment,Experiment_Devaluation, Schedule, Day, Subject, RewardName, LeverName) %>% 
  summarise(LP = sum(`LP Bin`),
            Rewards = sum(`Rewards Bin`),
            SessionLength = median(`Session Length`[`Session Length` != 0]),
            LP_rate_min = (LP/SessionLength) *60,
            MagEntry = sum(`MagEntry Bin`),
            MagEntry_rate_min = (MagEntry/SessionLength) *60,
  ) %>% 
  ungroup()

# Check that there is only a single value per subject/reward/day
summary %>% 
  group_by(Subject,RewardName, Day) %>% 
  summarise(n = n()) %>% 
  spread(Day, n)%>%
  kable()


# Quick plot to verify data

summary %>% ggplot(mapping = aes(x = as.factor(Day), y = LP_rate_min, colour = RewardName, group = RewardName)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5) +
  stat_summary(fun = "mean", geom = "point", size = 3) +
  stat_summary(fun = "mean", geom = "line", size = 0.5) +
  facet_wrap(~Experiment_Devaluation) +
  coord_cartesian(ylim = c(0,40)) +
  scale_y_continuous(breaks = seq(0,40,by = 5), expand = c(0.0,0)) +
  ggtitle("Instrumental Acquisition",subtitle = "SKR214 - Two Lever Design") + ylab("LP/min") + xlab("Day") +
  theme_cowplot()

summary %>% ggplot(mapping = aes(x = as.factor(Day), y = MagEntry_rate_min, colour = RewardName, group = RewardName)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5) +
  stat_summary(fun = "mean", geom = "point", size = 3) +
  stat_summary(fun = "mean", geom = "line", size = 0.5) +
  coord_cartesian(ylim = c(0,40)) +
  facet_wrap(~Experiment_Devaluation) +
  scale_y_continuous(breaks = seq(0,40,by = 5), expand = c(0.0,0)) +
  ggtitle("Instrumental Acquisition",subtitle = "SKR214 - Two Lever Design") + ylab("Magazine Entries/min") + xlab("Day") +
  theme_cowplot()



# Check for bias/reward preference in lever pressing rates
summary_wide_LP <- summary %>% 
  select(Experiment_Devaluation, Schedule, Day, Subject, RewardName, LP_rate_min) %>% 
  spread(RewardName, LP_rate_min) %>% 
  filter(Schedule == "RR10") %>% 
  mutate(MaltBias = maltodextrin - Sucrose,
         zdiff = (MaltBias - mean(MaltBias, na.rm = TRUE))/sd(MaltBias, na.rm = TRUE),
         malt_suc_ratio = maltodextrin / Sucrose)

summary_wide_LP %>% 
  filter(Day == 6) %>% 
  ggplot(mapping = aes(x = log(malt_suc_ratio), fill = as.factor(Day), group = as.factor(Day), alpha = .8)) +
  geom_histogram(binwidth = .1, show.legend = FALSE) + 
  geom_density(show.legend = FALSE)+
  theme_cowplot()

summary_wide_LP %>% 
  filter(Day == 6) %>% 
  ggplot(mapping = aes(x = malt_suc_ratio, fill = as.factor(Day), group = as.factor(Day), alpha = .8)) +
  geom_histogram(binwidth = .1, show.legend = FALSE) + 
  geom_density(show.legend = FALSE)+
  theme_cowplot()

summary_wide_LP %>% 
  filter(Day == 6) %>% 
  ggplot(mapping = aes(x = zdiff, fill = as.factor(Day), group = as.factor(Day), alpha = .8)) +
  geom_histogram(binwidth = .1, show.legend = FALSE) + 
  geom_density(show.legend = FALSE)+
  theme_cowplot()


# SKR216_Pav_Freq_Acquisition ---------------------------------------------------------------------


# Load Data
full_data <- read_csv(here("rawdata", "SKR216_Pav_Freq_Acquisition.csv"))

# Check that there are equal numbers of entries per subject/day
full_data %>% 
  group_by(Subject, Day) %>% 
  summarise(n = n()) %>% 
  spread(Day, n)%>%
  kable()


full_data <- full_data %>% 
  mutate(Experiment_Devaluation = case_when(Subject < 9 ~ "Taste Aversion",
                                            Subject > 8 ~ "Satiety") )


# Remove unnecessary calculation columns
full_data <- full_data %>% 
  select(Experiment_Devaluation,
         `Day`,                                                    
         `MSN`,                                                    
         `StartDate`,                                              
         `StartTime`,                                              
         `Experiment`,                                             
         `Group`,                                                  
         `Box`,                                                    
         `Subject`,                                                
         `Total MagEntries`,                                       
         `Total MagDuration`,                                      
         `Total Rewards`,                                          
         `Total Session Time`,                                     
         `Trial Number`,                                           
         `ITI_Duration(minus PreCS)`,                              
         `CS_Identity [1 = Noise, 2 = Click, 3 = tone]`,           
         `US_Identity [1 = Sucrose, 2 = Maltodextrin, 3 = Pellet]`,
         `Number of USs`,                                          
         `MagEntry_ITI`,
         `Latency to first magazine Entry`,                        
         `First response was an Exit`,                             
         `MagEntry_PreCS (2mins)`,                                 
         `MagEntry_CS (2 mins)`,                                   
         `CS-PreCS`,                                               
         `Trial Number, 1 CS`,                                     
         `CS_Name`,                                                
         `US_Name`,                                                
         `MagEntriesCS excluding reward delivery 5s`,              
         `CS_Duration (s)_NoReward5s`,                             
         `Devalued Reward 1st test`,                               
         `Devalued?`)

# Summarise data into averages per Outcome ID, rates per trial (2 mins)
summary <- full_data %>% 
  group_by(Experiment_Devaluation, Day, Subject, CS_Name, US_Name) %>% 
  summarise(AvgRewards = mean(`Number of USs`),
            CS_PreCS = mean(`CS-PreCS`),
            MagEntry_PreCS = mean(`MagEntry_PreCS (2mins)`),
            MagEntry_CS = mean(`MagEntry_CS (2 mins)`)
  ) %>% 
  ungroup()

# Check that there are equal numbers of trial types/subject
summary %>% 
  group_by(Subject, Day) %>% 
  summarise(n = n()) %>% 
  spread(Day, n)%>%
  kable()

# Quick Plot to verify data
summary %>% ggplot(mapping = aes(x = as.factor(Day), y = CS_PreCS, colour = US_Name, group = US_Name, shape = US_Name)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5) +
  stat_summary(fun = "mean", geom = "point", size = 3) +
  stat_summary(fun = "mean", geom = "line", size = 0.5) +
  coord_cartesian(ylim = c(0,30)) +
  facet_wrap(~ Experiment_Devaluation) +
  scale_y_continuous(breaks = seq(0,30,by = 5), expand = c(0.0,0)) +
  ggtitle("Pavlovian Acquisition",subtitle = "SKR214 - Magazine Frequency") + ylab("Mag Entries/2 mins") + xlab("Day") +
  theme_cowplot()

summary %>% ggplot(mapping = aes(x = as.factor(Day), y = CS_PreCS, colour = CS_Name, group = CS_Name, shape = CS_Name)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5) +
  stat_summary(fun = "mean", geom = "point", size = 3) +
  stat_summary(fun = "mean", geom = "line", size = 0.5) +
  facet_wrap(~ Experiment_Devaluation) +
  coord_cartesian(ylim = c(0,30)) +
  scale_y_continuous(breaks = seq(0,30,by = 5), expand = c(0.0,0)) +
  ggtitle("Pavlovian Acquisition",subtitle = "SKR214 - Magazine Frequency") + ylab("Mag Entries/2 mins") + xlab("Day") +
  theme_cowplot()

# Check for response bias to maltodextrin vs sucrose
summary_CS_bias <- summary %>% 
  select(-CS_Name, -MagEntry_PreCS, -MagEntry_CS, -AvgRewards) %>% 
  filter(US_Name != "Pellet") %>% 
  pivot_wider(names_from = US_Name, values_from = CS_PreCS) %>% 
  mutate(MaltBias = Maltodextrin - Sucrose,
         zdiff = (MaltBias - mean(MaltBias, na.rm = TRUE))/sd(MaltBias, na.rm = TRUE),
         malt_suc_ratio = Maltodextrin / Sucrose)

summary_CS_bias %>% 
  filter(Day == 6) %>% 
  ggplot(mapping = aes(x = log(malt_suc_ratio), fill = as.factor(Day), group = as.factor(Day), alpha = .8)) +
  geom_histogram(binwidth = .1, show.legend = FALSE) + 
  geom_density(show.legend = FALSE)+
  theme_cowplot()

summary_CS_bias %>% 
  filter(Day == 6) %>% 
  ggplot(mapping = aes(x = zdiff, fill = as.factor(Day), group = as.factor(Day), alpha = .8)) +
  geom_histogram(binwidth = .1, show.legend = FALSE) + 
  geom_density(show.legend = FALSE)+
  theme_cowplot()

summary_CS_bias %>% 
  filter(Day == 6) %>% 
  select(Experiment_Devaluation, Subject, zdiff, malt_suc_ratio) %>% 
  mutate(log_malt_suc_ratio = log(malt_suc_ratio)) %>% 
  kable()
#Exclusions - Satiety Expt --- Rat 29 and 31 Over 3x response bias to Malt vs Suc 
#Exclusions - LiCl Expt --- Rat 8  2.8 x response bias to Malt vs Suc 

# |Experiment_Devaluation | Subject|      zdiff| malt_suc_ratio| log_malt_suc_ratio|
#   |:----------------------|-------:|----------:|--------------:|------------------:|
#   |Satiety                |       9| -0.3445688|      0.8543689|         -0.1573922|
#   |Satiety                |      10| -1.1003189|      0.5729167|         -0.5570150|
#   |Satiety                |      11|  1.0506623|      1.7173913|          0.5408065|
#   |Satiety                |      12| -0.0538956|      0.8437500|         -0.1698990|
#   |Satiety                |      13|  1.7773451|      2.1153846|          0.7492366|
#   |Satiety                |      14|  2.3296241|      2.0405405|          0.7132147|
#   |Satiety                |      15|  0.4983833|      1.3111111|          0.2708750|
#   |Satiety                |      16|  1.1959989|      1.6551724|          0.5039052|
#   |Satiety                |      17| -1.4491267|      0.5000000|         -0.6931472|
#   |Satiety                |      18| -1.0712516|      0.5294118|         -0.6359888|
#   |Satiety                |      19| -0.3736361|      0.7377049|         -0.3042114|
#   |Satiety                |      20| -0.1120303|      1.3500000|          0.3001046|
#   |Satiety                |      21|  2.1552202|      2.2456140|          0.8089790|
#   |Satiety                |      22|  1.4576047|      1.8703704|          0.6261365|
#   |Satiety                |      23| -2.1758096|      0.3858268|         -0.9523668|
#   |Satiety                |      24|  0.1205082|      1.0126582|          0.0125788|
#   |Satiety                |      25| -0.7805785|      0.6428571|         -0.4418328|
#   |Satiety                |      26|  0.4983833|      1.2058824|          0.1872115|
#   |Satiety                |      27|  0.4693160|      1.1710526|          0.1579030|
#   |Satiety                |      28|  0.1786429|      1.0491803|          0.0480092|
#   |Satiety                |      29|  2.1261529|      3.9166667|          1.3652410|  ##
#   |Satiety                |      30|  1.9226817|      1.7411765|          0.5545610|
#   |Satiety                |      31| -1.5944633|      0.2467532|         -1.3993664|  ##
#   |Satiety                |      32|  0.4111814|      1.0982143|          0.0936855|
#   |Taste Aversion         |       1| -1.0131170|      0.5581395|         -0.5831463|
#   |Taste Aversion         |       2| -0.5480400|      0.6507937|         -0.4295627|
#   |Taste Aversion         |       3|  0.0042390|      0.9400000|         -0.0618754|
#   |Taste Aversion         |       4|  0.6727872|      1.6250000|          0.4855078|
#   |Taste Aversion         |       5| -0.2573668|      0.8208955|         -0.1973594|
#   |Taste Aversion         |       6|  0.0333063|      0.9740260|         -0.0263173|
#   |Taste Aversion         |       7|  0.6146526|      1.2045455|          0.1861023|
#   |Taste Aversion         |       8|  1.1378642|      2.8000000|          1.0296194|  ##???


# SKR216_Satiety_consumption ----------------------------------------------
# Load Data
full_data <- read_csv(here("rawdata", "SKR216_Satiety_consumption.csv"))

# Check that there are equal numbers of entries per subject/day
full_data %>% 
  group_by(Rat, SatietyTestNum) %>% 
  summarise(n = n()) %>% 
  spread(SatietyTestNum, n)%>%
  kable()

# Quick Plot to verify data
full_data %>% 
  # filter(Rat != 29 & Rat != 31)%>% 
  ggplot(mapping = aes(x = as.factor(SatietyTestNum), y = `consumed(g)`, colour = Devaluation, group = Devaluation, shape = Devaluation)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5) +
  stat_summary(fun = "mean", geom = "point", size = 3) +
  stat_summary(fun = "mean", geom = "line", size = 0.5) +
  coord_cartesian(ylim = c(0,30)) +
  scale_y_continuous(breaks = seq(0,30,by = 5), expand = c(0.0,0)) +
  ggtitle("Satiety",subtitle = "SKR214 - Consumption") + ylab("Consumed (g)") + xlab("Satiety Test Number") +
  theme_cowplot()


# SKR216_LiCl_TasteAversion -----------------------------------------------

# Load Data
full_data <- read_csv(here("rawdata", "SKR216_LiCl_TasteAversion.csv"))

# Check that there are equal numbers of entries per subject/day
full_data %>% 
  group_by(Rat, Pairing, Injection) %>% 
  summarise(n = n()) %>% 
  spread(Rat, n)%>%
  kable()

# Quick Plot to verify data
full_data %>% ggplot(mapping = aes(x = as.factor(Pairing), y = `consumed(g)`, colour = Injection, group = Injection, shape = Injection)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5) +
  stat_summary(fun = "mean", geom = "point", size = 3) +
  stat_summary(fun = "mean", geom = "line", size = 0.5) +
  coord_cartesian(ylim = c(0,30)) +
  scale_y_continuous(breaks = seq(0,30,by = 5), expand = c(0.0,0)) +
  ggtitle("LiCl",subtitle = "SKR214 - Consumption") + ylab("Consumed (g)") + xlab("Injection Pairing Number") +
  theme_cowplot()


# SKR216_LiCl_TasteAversion_inMagazine ------------------------------------

# Load Data
full_data <- read_csv(here("rawdata", "SKR216_LiCl_TasteAversion_inMagazine.csv"))

summary <- full_data %>% 
  mutate(Liquid = case_when(str_detect(MSN,"sucrose") ~ "Sucrose",
                            str_detect(MSN,"Maltodextrin") ~ "Maltodextrin")
         ) %>% 
  select(Injection, Liquid, Subject, Bin_4mins, Mag1_5sBins, Mag1_duration_5sbins) %>% 
  group_by(Injection, Subject, Liquid, Bin_4mins) %>% 
  summarise(Bin_4mins = median(Bin_4mins),
            MagEntry = sum(Mag1_5sBins),
            MagDur = sum(Mag1_duration_5sbins)/100) %>% 
  ungroup() %>% 
  filter(Bin_4mins < 7)
  
# Quick Plot to verify data
summary %>%
  # filter(Subject != 8) %>% 
  ggplot(mapping = aes(x = as.factor(Bin_4mins), y = MagEntry, colour = Injection, group = Injection, fill = Injection)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5) +
  stat_summary(fun = "mean", geom = "point", size = 3) +
  stat_summary(fun = "mean", geom = "line", size = 0.5) +
  coord_cartesian(ylim = c(0,40)) +
  scale_y_continuous(breaks = seq(0,40,by = 5), expand = c(0.0,0)) +
  ggtitle("LiCl",subtitle = "SKR216 - Taste Aversion in Chamber") + ylab("MagEntries/4mins") + xlab("Bin 4 Mins") +
  theme_cowplot()


# SKR216_sPIT_Test --------------------------------------------------------

# Load Data
full_data <- read_csv(here("rawdata", "SKR216_sPIT_Test.csv"))

summary <- full_data %>% 
  filter(`Trial Structure` == "Baseline Extinction",
         Devaluation == "Satiety") %>% 
  group_by(Devaluation, Group, Session, Subject,`1Lever_LeverOutcomeIdentity`,`1Lever_DevaluedLever?`, `Minute Counter`) %>% 
  summarise(LP = mean(`1 Lever Test_Lever Pressing`)*60,
            MagEntries = mean(MagEntries)*60,
            MagDuration = mean(MagDuration)*60
  ) %>% 
  ungroup()

# Quick Plot to verify data
summary %>%
  # filter(Subject != 8) %>% 
  filter(Subject != 29 & Subject != 31) %>% 
  # filter(Session == 1 | Session == 2) %>%
  ggplot(mapping = aes(x = as.factor(`Minute Counter`), y = LP, colour = `1Lever_DevaluedLever?`, group = `1Lever_DevaluedLever?`, fill = `1Lever_DevaluedLever?`)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5) +
  stat_summary(fun = "mean", geom = "point", size = 3) +
  stat_summary(fun = "mean", geom = "line", size = 0.5) +
  # facet_wrap(~ Session, nrow = 1) +
  coord_cartesian(ylim = c(0,50)) +
  scale_y_continuous(breaks = seq(0,50,by = 5), expand = c(0.0,0)) +
  ggtitle("Satiety",subtitle = "SKR216 -Devaluation Extinction Test") + ylab("LP/mins") + xlab("Bin 1 Mins") +
  theme_cowplot()

## 2. Satiety Deval sPIT test  --------------------------------------------------------
# Load Data
full_data <- read_csv(here("rawdata", "SKR216_sPIT_Test.csv"))

summary <- full_data %>% 
  filter(
    `Trial Structure` == "PreCS" | `Trial Structure` == "CS",
    # `trial period split minutes [1,2]` == "PreCS2"|`trial period split minutes [1,2]` == "CS1",
    # `trial period split minutes [1,2]` == "PreCS1" | `trial period split minutes [1,2]` == "PreCS2"|`trial period split minutes [1,2]` == "CS1"|`trial period split minutes [1,2]` == "CS2",
    # `Trial Number` == 1|`Trial Number` == 2,
    Devaluation == "LiCl"
  ) %>% 
  group_by(Devaluation, Group, Session, Subject, `Trial Structure`, `Devalued Pavlovian US?`, `1Lever_LeverOutcomeIdentity`,`1Lever_DevaluedLever?`, `1Lever_PavlovianUS_Same/Different`) %>% 
  summarise(LP = mean(`1 Lever Test_Lever Pressing`)*60,
            MagEntries = mean(MagEntries)*60,
            MagDuration = mean(MagDuration)*60
  ) %>% 
  ungroup()

summary %>% 
  filter(Subject != 29 & Subject != 31 & Subject !=8) %>% 
  filter(Session == 1 | Session == 2) %>%
  ggplot(mapping = aes(x = `Trial Structure`, y = MagEntries, colour = `Devalued Pavlovian US?`, group = `Devalued Pavlovian US?`)) +
  # facet_wrap(~ Session) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5) +
  stat_summary(fun = "mean", geom = "point", size = 3) +
  stat_summary(fun = "mean", geom = "line", size = 0.5) +
  coord_cartesian(ylim = c(0,7)) +
  scale_y_continuous(breaks = seq(0,7,by = 1), expand = c(0.0,0)) +
  ggtitle("Satiety Test",subtitle = "SKR214 - Lever Pressing") + ylab("MagEntries/min") + xlab("Minute") +
  theme_cowplot()



summary %>% 
  filter(Subject != 29 & Subject != 31 & Subject !=8) %>% 
  # filter(Session == 1 | Session == 2) %>%
  ggplot(mapping = aes(x = `1Lever_DevaluedLever?`, y = LP , colour = `1Lever_PavlovianUS_Same/Different`, group = `1Lever_PavlovianUS_Same/Different`, fill = `1Lever_PavlovianUS_Same/Different`)) +
  facet_wrap(~ `Trial Structure`) +
  stat_summary(fun = "mean", geom = "bar", size = 3, position = position_dodge(width = .9)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5, position = position_dodge(width = 0.9), colour = "black") +
  coord_cartesian(ylim = c(0,10)) +
  scale_y_continuous(breaks = seq(0,10,by = 1), expand = c(0.0,0)) +
  ggtitle("Satiety Test",subtitle = "SKR214 - Lever Pressing") + ylab("Lever Presses/min") + xlab("Minute") +
  theme_cowplot()


summary %>% 
  filter(Subject != 29 & Subject != 31) %>% 
  filter(Session == 1 | Session == 2) %>%
  ggplot(mapping = aes(x = `1Lever_DevaluedLever?`, y = MagEntries , colour = `1Lever_PavlovianUS_Same/Different`, group = `1Lever_PavlovianUS_Same/Different`, fill = `1Lever_PavlovianUS_Same/Different`)) +
  facet_wrap(~ `Trial Structure`) +
  stat_summary(fun = "mean", geom = "bar", size = 3, position = position_dodge(width = .9)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5, position = position_dodge(width = 0.9), colour = "black") +
  coord_cartesian(ylim = c(0,10)) +
  scale_y_continuous(breaks = seq(0,10,by = 1), expand = c(0.0,0)) +
  ggtitle("Satiety Test",subtitle = "SKR214 - MagEntries") + ylab("MagEntries/min") + xlab("Minute") +
  theme_cowplot()


# --------


summary_LP_wide <- summary %>% 
  select(-`Devalued Pavlovian US?`, -MagEntries, -MagDuration) %>% 
  pivot_wider(names_from = `Trial Structure`, values_from = LP) 

summary_LP_PreCSonly <- summary_LP_wide %>% 
  select(-CS) %>% 
  group_by(Group, Session, Subject, Devaluation, `1Lever_DevaluedLever?`) %>% 
  summarise(PreCS = mean(PreCS)) %>%
  ungroup()

summary_LP_PreCSonly <- summary_LP_PreCSonly %>% 
  pivot_longer(cols = PreCS, names_to = "Trial Structure", values_to = "LP") %>% 
  mutate("1Lever_PavlovianUS_Same/Different" = "PreCS")

summary_LP_commonPreCS <- summary %>% 
  filter(`Trial Structure` == "CS") %>% 
  full_join(summary_LP_PreCSonly)

summary_LP_commonPreCS %>% 
  filter(Subject != 29 & Subject != 31 & Subject !=8) %>% 
  # filter(Session == 1 | Session == 2) %>%
  ggplot(mapping = aes(x = `1Lever_DevaluedLever?`, y = LP , colour = `1Lever_PavlovianUS_Same/Different`, group = `1Lever_PavlovianUS_Same/Different`, fill = `1Lever_PavlovianUS_Same/Different`)) +
  stat_summary(fun = "mean", geom = "bar", size = 3, position = position_dodge(width = .9)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5, position = position_dodge(width = 0.9), colour = "black") +
  coord_cartesian(ylim = c(0,10)) +
  scale_y_continuous(breaks = seq(0,10,by = 1), expand = c(0.0,0)) +
  ggtitle("Satiety Test",subtitle = "SKR214 - Lever Pressing") + ylab("Lever Presses/min") + xlab("Minute") +
  theme_cowplot()

summary_LP_commonPreCS %>% 
  filter(Subject != 29 & Subject != 31 & Subject !=8) %>% 
  # filter(Session == 1 | Session == 2) %>%
  ggplot(mapping = aes(x = `1Lever_DevaluedLever?`, y = LP , colour = `Devalued Pavlovian US?`, group = `Devalued Pavlovian US?`, fill = `Devalued Pavlovian US?`)) +
  stat_summary(fun = "mean", geom = "bar", size = 3, position = position_dodge(width = .9)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5, position = position_dodge(width = 0.9), colour = "black") +
  coord_cartesian(ylim = c(0,10)) +
  scale_y_continuous(breaks = seq(0,10,by = 1), expand = c(0.0,0)) +
  ggtitle("Satiety Test",subtitle = "SKR214 - Lever Pressing") + ylab("Lever Presses/min") + xlab("Minute") +
  theme_cowplot()


# List of all raw data files ----------------------------------------------

# > dir(here("rawdata"))
# [1] "SKR214_CRF_Acquisition.csv"              
# [2] "SKR214_Inst_Acquisition.csv"             
# [3] "SKR214_LiCl_TasteAversion.csv"           
# [4] "SKR214_Pav_Dur_Acquisition.csv"          
# [5] "SKR214_Pav_Freq_Acquisition.csv"         
# [6] "SKR214_Satiety_consumption.csv"          
# [7] "SKR214_Satiety_consumptionbaseline.csv"  
# [8] "SKR214_sPIT_Test.csv"                    
# [9] "SKR216_consumptionbaseline.csv"          
# [10] "SKR216_Inst_Acquisition.csv"             
# [11] "SKR216_LiCl_TasteAversion.csv"           
# [12] "SKR216_LiCl_TasteAversion_inMagazine.csv"
# [13] "SKR216_Pav_Dur_Acquisition.csv"          
# [14] "SKR216_Pav_Freq_Acquisition.csv"         
# [15] "SKR216_Satiety_consumption.csv"          
# [16] "SKR216_sPIT_Test.csv"  

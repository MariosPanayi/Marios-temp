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


##### SKR1127 - Simultaneous procedure ----
full_data <- read_csv(here('data','SKR127_ConditionedInhibitionSimultaneous_MagFreq.csv'))

# Filter out exclusions due to histology/behaviouralcriterion
full_data <- full_data %>% 
  filter(!Subject %in% c(11,6,27,14))
# Re-label columns for easier variable names
full_data <- full_data %>% 
rename(Cue = `Cue name`,
       InfusionGroup = `Infusion Group`,
       US = `US identity`,
       CS_id = `CS identity`,
       CS_name = `CS Name`) %>% 
  mutate(reward = US, 
  reward = recode(reward, "3" = "+", "0" = "-")) %>% 
  unite("GroupCueID", InfusionGroup, Cue, reward, sep = "", remove = FALSE)

# Re order and rename levels for plotting

levels <- c("Saline: A+" = "SalineA+",
            "Saline: B+" = "SalineB+",
            "Saline: AX-" = "SalineAX-",
            "Saline: B-" = "SalineB-",
            "Saline: BX-" = "SalineBX-",
            "Saline: X+" = "SalineX+",
            "Saline: Y+" = "SalineY+",
            "Saline: Z+" = "SalineZ+",
            "Saline: Y-" = "SalineY-",
            "Saline: Z-" = "SalineZ-",
            "Muscimol: A+" = "MuscimolA+",
            "Muscimol: B+" = "MuscimolB+",
            "Muscimol: AX-" = "MuscimolAX-",
            "Muscimol: B-" = "MuscimolB-",
            "Muscimol: BX-" = "MuscimolBX-",
            "Muscimol: X+" = "MuscimolX+",
            "Muscimol: Y+" = "MuscimolY+",
            "Muscimol: Z+" = "MuscimolZ+",
            "Muscimol: Y-" = "MuscimolY-",
            "Muscimol: Z-" = "MuscimolZ-")
full_data$GroupCueID <- fct_recode(full_data$GroupCueID, !!!levels)

levelorder <- c("Saline: A+" ,
               "Saline: B+" ,
               "Saline: AX-",
               "Saline: B-" ,
               "Saline: BX-",
               "Saline: X+" ,
               "Saline: Y+" ,
               "Saline: Z+" ,
               "Saline: Y-" ,
               "Saline: Z-" ,
               "Muscimol: A+" ,
               "Muscimol: B+" ,
               "Muscimol: AX-",
               "Muscimol: B-" ,
               "Muscimol: BX-",
               "Muscimol: X+" ,
               "Muscimol: Y+" ,
               "Muscimol: Z+" ,
               "Muscimol: Y-" ,
               "Muscimol: Z-" )
full_data$GroupCueID <- fct_relevel(as.factor(full_data$GroupCueID), levelorder)

levelorder <- c("Saline", "Muscimol")

full_data$InfusionGroup <- fct_relevel(as.factor(full_data$InfusionGroup), levelorder)

# Consumption test data N.B. fewer subjects due to loss of patency of cannulae
full_consumption_data <- read_csv(here('data','SKR127_ConsumptionTest.csv'))
#Order factor for plotting
levelorder <- c("Saline", "Muscimol")
full_consumption_data$InfusionGroup <- fct_relevel(as.factor(full_consumption_data$InfusionGroup), levelorder)

levelorder <- c("No Infusion", "Infusion")
full_consumption_data$Infusion <- fct_relevel(as.factor(full_consumption_data$Infusion), levelorder)

##### Summarise Data
#N.B. Summarising data - Averaging over 2 sessions per day

data_stage1 <- full_data %>% 
  filter(Stage == "Stage 1")

data_stage1_summary <- data_stage1 %>% 
  group_by(Day, Subject, Cue, US, CS_id, CS_name, InfusionGroup, GroupCueID) %>% 
  summarise(CS = mean(`CS(10s)`), na.rm=TRUE,
            PreCS = mean(`PreCS (10s)`), na.rm=TRUE,
            CS_Pre = mean(`CS-PreCS`), na.rm=TRUE,
            Latency_mean = mean(`MagEntry Latency to CS`)/100, na.rm=TRUE,
            Latency_median = median(`MagEntry Latency to CS`)/100, na.rm=TRUE,)


#N.B. Summarising data 

data_stage2 <- full_data %>% 
  filter(Stage == "Stage 2")

data_stage2_summary <- data_stage2 %>% 
  group_by(Day, Subject, Cue, US, CS_id, CS_name, InfusionGroup, GroupCueID) %>% 
  summarise(CS = mean(`CS(10s)`), na.rm=TRUE,
            PreCS = mean(`PreCS (10s)`), na.rm=TRUE,
            CS_Pre = mean(`CS-PreCS`), na.rm=TRUE,
            Latency_mean = mean(`MagEntry Latency to CS`)/100, na.rm=TRUE,
            Latency_median = median(`MagEntry Latency to CS`)/100, na.rm=TRUE,)

#N.B. Summarising data 

data_stage3 <- full_data %>% 
  filter(Stage == "Stage 3")

data_stage3_summary <- data_stage3 %>% 
  group_by(Day, Subject, Cue, US, CS_id, CS_name, InfusionGroup, GroupCueID) %>% 
  summarise(CS = mean(`CS(10s)`), na.rm=TRUE,
            PreCS = mean(`PreCS (10s)`), na.rm=TRUE,
            CS_Pre = mean(`CS-PreCS`), na.rm=TRUE,
            Latency_mean = mean(`MagEntry Latency to CS`)/100, na.rm=TRUE,
            Latency_median = median(`MagEntry Latency to CS`)/100, na.rm=TRUE,)

#N.B. Summarising data 

data_stage4 <- full_data %>% 
  filter(Stage == "Stage 4")

data_stage4_summary <- data_stage4 %>% 
  group_by(Day, Subject, Cue, US, CS_id, CS_name, InfusionGroup, GroupCueID) %>% 
  summarise(CS = mean(`CS(10s)`), na.rm=TRUE,
            PreCS = mean(`PreCS (10s)`), na.rm=TRUE,
            CS_Pre = mean(`CS-PreCS`), na.rm=TRUE,
            Latency_mean = mean(`MagEntry Latency to CS`)/100, na.rm=TRUE,
            Latency_median = median(`MagEntry Latency to CS`)/100, na.rm=TRUE,)

data_stage4_summary_Block2Trials <- data_stage4 %>% 
  group_by(Day, Subject, Cue, US, CS_id, CS_name, InfusionGroup, GroupCueID, `Trial Number Summation Test Blocked`) %>% 
  summarise(CS = mean(`CS(10s)`), na.rm=TRUE,
            PreCS = mean(`PreCS (10s)`), na.rm=TRUE,
            CS_Pre = mean(`CS-PreCS`), na.rm=TRUE,
            Latency_mean = mean(`MagEntry Latency to CS`)/100, na.rm=TRUE,
            Latency_median = median(`MagEntry Latency to CS`)/100, na.rm=TRUE,)


#N.B. Summarising data 

data_stage5 <- full_data %>% 
  filter(Stage == "Stage 5")

data_stage5_summary <- data_stage5 %>% 
  group_by(Day, Subject, Cue, US, CS_id, CS_name, InfusionGroup, GroupCueID) %>% 
  summarise(CS = mean(`CS(10s)`), na.rm=TRUE,
            PreCS = mean(`PreCS (10s)`), na.rm=TRUE,
            CS_Pre = mean(`CS-PreCS`), na.rm=TRUE,
            Latency_mean = mean(`MagEntry Latency to CS`)/100, na.rm=TRUE,
            Latency_median = median(`MagEntry Latency to CS`)/100, na.rm=TRUE,)



#### Figure plotting Parameter ----

DarkRed = "#67001F"
MediumRed = "#B2182B"
LightRed = "#D6604D"
DarkBlue = "#053061"
MediumBlue = "#2166AC"
LightBlue = "#4393C3"
Black       = "#000000"
White       = "#FFFFFF"
LightGrey   = "#F0F0F0"
MediumGrey  = "#BDBDBD"
DarkGrey    = "#252525"

## Shapes for Geom_point
circle            = 21
square            = 22
diamond           = 23
triangleUp        = 24
triangleDown      = 25
cross       = 4
# fillcolours <- c("Saline: A+" = MediumBlue,
#                  "Saline: B+" = MediumBlue,
#                  "Saline: AX-"= LightBlue ,
#                  "Saline: B-" = MediumBlue,
#                  "Saline: BX-"= LightBlue,
#                  "Saline: X+" =LightBlue,
#                  "Saline: Y+" = MediumBlue,
#                  "Saline: Z+" = MediumBlue,
#                  "Saline: Y-" = White,
#                  "Saline: Z-" = White,
#                  "Muscimol: A+" = MediumRed,
#                  "Muscimol: B+" = MediumRed,
#                  "Muscimol: AX-"= LightRed,
#                  "Muscimol: B-" = MediumRed,
#                  "Muscimol: BX-"= LightRed,
#                  "Muscimol: X+" = LightRed,
#                  "Muscimol: Y+" = MediumRed,
#                  "Muscimol: Z+" = MediumRed,
#                  "Muscimol: Y-" = White,
#                  "Muscimol: Z-" = White)
# 
# linecolours <- c("Saline: A+" = MediumBlue,
#                  "Saline: B+" = MediumBlue,
#                  "Saline: AX-"= DarkBlue,
#                  "Saline: B-" = DarkBlue,
#                  "Saline: BX-"= DarkBlue,
#                  "Saline: X+" = MediumBlue,
#                  "Saline: Y+" = MediumBlue,
#                  "Saline: Z+" = MediumBlue,
#                  "Saline: Y-" = MediumBlue,
#                  "Saline: Z-" = MediumBlue,
#                  "Muscimol: A+" = MediumRed,
#                  "Muscimol: B+" = MediumRed,
#                  "Muscimol: AX-"= DarkRed,
#                  "Muscimol: B-" = MediumRed,
#                  "Muscimol: BX-"= MediumRed,
#                  "Muscimol: X+" = MediumRed,
#                  "Muscimol: Y+" = MediumRed,
#                  "Muscimol: Z+" = MediumRed,
#                  "Muscimol: Y-" = MediumRed,
#                  "Muscimol: Z-" = MediumRed)
# 
# linetypes <- c("Saline: A+" = "dotted",
#                "Saline: B+" = "dotted",
#                "Saline: AX-"= "dotted",
#                "Saline: B-" = "dotted",
#                "Saline: BX-"= "dotted",
#                "Saline: X+" = "dotted",
#                "Saline: Y+" = "dotted",
#                "Saline: Z+" = "dotted",
#                "Saline: Y-" = "dotted",
#                "Saline: Z-" = "dotted",
#                "Muscimol: A+" = "solid",
#                "Muscimol: B+" = "solid",
#                "Muscimol: AX-"= "solid",
#                "Muscimol: B-" = "solid",
#                "Muscimol: BX-"= "solid",
#                "Muscimol: X+" = "solid",
#                "Muscimol: Y+" = "solid",
#                "Muscimol: Z+" = "solid",
#                "Muscimol: Y-" = "solid",
#                "Muscimol: Z-" = "solid")
# 
# pointshapes <- c("Saline: A+" = circle,
#                  "Saline: B+" = diamond,
#                  "Saline: AX-"= square,
#                  "Saline: B-" = diamond,
#                  "Saline: BX-"= square,
#                  "Saline: X+" = triangleDown,
#                  "Saline: Y+" = triangleUp,
#                  "Saline: Z+" = circle,
#                  "Saline: Y-" = triangleDown,
#                  "Saline: Z-" = circle,
#                  "Muscimol: A+" = circle,
#                  "Muscimol: B+" = diamond,
#                  "Muscimol: AX-"= square,
#                  "Muscimol: B-" = diamond,
#                  "Muscimol: BX-"= square,
#                  "Muscimol: X+" = triangleDown,
#                  "Muscimol: Y+" = triangleUp,
#                  "Muscimol: Z+" = circle,
#                  "Muscimol: Y-" = triangleDown,
#                  "Muscimol: Z-" = circle)
# 
# # Plotting parameters when only using Saline/Muscimol distinction
# fillcolours_infusion <- c("Saline" = MediumBlue,
#                           "Muscimol" = MediumRed)
# 
# linecolours_infusion <- c("Saline" = MediumBlue,
#                           "Muscimol" = MediumRed)
# 
# linetypes_infusion <- c("Saline" = "dotted",
#                         "Muscimol" = "solid")
# 
# pointshapes_infusion <- c("Saline" = square,
#                           "Muscimol" = square)
fillcolours <- c("Saline: A+" = White,
                 "Saline: A-" = White,
                 "Saline: B+" = White,
                 "Saline: C+" = LightGrey,
                 "Saline: C-" = LightGrey,
                 "Saline: AX-"= White,
                 "Saline: B-" = White,
                 "Saline: BX-"= LightGrey,
                 "Saline: X+" = LightGrey,
                 "Saline: Y+" = White,
                 "Saline: Z+" = White,
                 "Saline: Y-" = White,
                 "Saline: Z-" = White,
                 "Muscimol: A+" = DarkGrey,
                 "Muscimol: A-" = DarkGrey,
                 "Muscimol: B+" = DarkGrey,
                 "Muscimol: C+" = MediumGrey,
                 "Muscimol: C-" = MediumGrey,
                 "Muscimol: AX-"= DarkGrey,
                 "Muscimol: B-" = DarkGrey,
                 "Muscimol: BX-"= MediumGrey,
                 "Muscimol: X+" = MediumGrey,
                 "Muscimol: Y+" = DarkGrey,
                 "Muscimol: Z+" = DarkGrey,
                 "Muscimol: Y-" = DarkGrey,
                 "Muscimol: Z-" = DarkGrey)

linecolours <- c("Saline: A+" = Black,
                 "Saline: A-" = Black,
                 "Saline: B+" = Black,
                 "Saline: C+" = Black,
                 "Saline: C-" = Black,
                 "Saline: AX-"= Black,
                 "Saline: B-" = Black,
                 "Saline: BX-"= Black,
                 "Saline: X+" = Black,
                 "Saline: Y+" = Black,
                 "Saline: Z+" = Black,
                 "Saline: Y-" = Black,
                 "Saline: Z-" = Black,
                 "Muscimol: A+" = Black,
                 "Muscimol: A-" = Black,
                 "Muscimol: B+" = Black,
                 "Muscimol: C+" = Black,
                 "Muscimol: C-" = Black,
                 "Muscimol: AX-"= Black,
                 "Muscimol: B-" = Black,
                 "Muscimol: BX-"= Black,
                 "Muscimol: X+" = Black,
                 "Muscimol: Y+" = Black,
                 "Muscimol: Z+" = Black,
                 "Muscimol: Y-" = Black,
                 "Muscimol: Z-" = Black)

linetypes <- c("Saline: A+" = "dotted",
               "Saline: A-" = "dotted",
               "Saline: B+" = "dotted",
               "Saline: C+" = "dotted",
               "Saline: C-" = "dotted",
               "Saline: AX-"= "dotted",
               "Saline: B-" = "dotted",
               "Saline: BX-"= "dotted",
               "Saline: X+" = "dotted",
               "Saline: Y+" = "dotted",
               "Saline: Z+" = "dotted",
               "Saline: Y-" = "dotted",
               "Saline: Z-" = "dotted",
               "Muscimol: A+" = "solid",
               "Muscimol: A-" = "solid",
               "Muscimol: B+" = "solid",
               "Muscimol: C+" = "solid",
               "Muscimol: C-" = "solid",
               "Muscimol: AX-"= "solid",
               "Muscimol: B-" = "solid",
               "Muscimol: BX-"= "solid",
               "Muscimol: X+" = "solid",
               "Muscimol: Y+" = "solid",
               "Muscimol: Z+" = "solid",
               "Muscimol: Y-" = "solid",
               "Muscimol: Z-" = "solid")

pointshapes <- c("Saline: A+" = circle,
                 "Saline: A-" = circle,
                 "Saline: B+" = diamond,
                 "Saline: C+" = circle,
                 "Saline: C-" = circle,
                 "Saline: AX-"= square,
                 "Saline: B-" = diamond,
                 "Saline: BX-"= square,
                 "Saline: X+" = triangleDown,
                 "Saline: Y+" = triangleUp,
                 "Saline: Z+" = circle,
                 "Saline: Y-" = triangleDown,
                 "Saline: Z-" = circle,
                 "Muscimol: A+" = circle,
                 "Muscimol: A-" = circle,
                 "Muscimol: B+" = diamond,
                 "Muscimol: C+" = circle,
                 "Muscimol: C-" = circle,
                 "Muscimol: AX-"= square,
                 "Muscimol: B-" = diamond,
                 "Muscimol: BX-"= square,
                 "Muscimol: X+" = triangleDown,
                 "Muscimol: Y+" = triangleUp,
                 "Muscimol: Z+" = circle,
                 "Muscimol: Y-" = triangleDown,
                 "Muscimol: Z-" = circle)

# Plotting parameters when only using Saline/Muscimol distinction

fillcolours_infusion <- c("Saline" = White,
                          "Muscimol" = DarkGrey)
linecolours_infusion <- c("Saline" = Black,
                          "Muscimol" = DarkGrey)
linetypes_infusion <- c("Saline" = "dotted",
                        "Muscimol" = "solid")
pointshapes_infusion <- c("Saline" = square,
                          "Muscimol" = square)


##### Plot Figures ----
##### Main figures ====
stage1_freq <- data_stage1_summary %>% filter(Cue != "Z") %>% 
  ggplot(mapping = aes(x = as.factor(Day), y = CS_Pre, group = GroupCueID, colour = GroupCueID, fill = GroupCueID, shape = GroupCueID,linetype = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-10,20,1)) +
  ggtitle("Stage 1 - Acquisition") + xlab("Day") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,5.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))  


stage2_freq <- data_stage2_summary %>% filter(Cue != "Z") %>% 
  ggplot(mapping = aes(x = as.factor(Day), y = CS_Pre, group = GroupCueID, colour = GroupCueID, fill = GroupCueID, shape = GroupCueID,linetype = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-10,20,1)) +
  ggtitle("Stage 2 - Feature Negative") + xlab("Day") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,5.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))  

stage3_freq <- data_stage3_summary %>% filter(Cue != "Z") %>% 
  ggplot(mapping = aes(x = as.factor(Day), y = CS_Pre, group = GroupCueID, colour = GroupCueID, fill = GroupCueID, shape = GroupCueID,linetype = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-10,20,1)) +
  ggtitle("Reacquisition") + xlab("Day") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,5.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))  


summation_freq <- data_stage4_summary %>% filter(!Cue %in%  c("Z", "Y"), 
                               US == "0") %>% 
  ggplot(mapping = aes(x = as.factor(InfusionGroup), y = CS_Pre, group = GroupCueID, colour = GroupCueID ,fill = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-10,20,1)) +
  ggtitle("Summation") + xlab("Group") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,5.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(.5,"line"))  


summation_freq_2trials <- data_stage4_summary_Block2Trials %>% filter(!Cue %in%  c("Z", "Y"), 
                               US == "0",
                               `Trial Number Summation Test Blocked` == 1) %>% 
  ggplot(mapping = aes(x = as.factor(InfusionGroup), y = CS_Pre, group = GroupCueID, colour = GroupCueID ,fill = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-10,20,1)) +
  ggtitle("Summation") + xlab("Group") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,5.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(.5,"line"))  


summation_freq_alltrials <- data_stage4_summary_Block2Trials %>% filter(!Cue %in%  c("Z", "Y"), 
                                            US == "0") %>% 
  ggplot(mapping = aes(x = as.factor(`Trial Number Summation Test Blocked`), y = CS_Pre, group = GroupCueID, colour = GroupCueID, fill = GroupCueID, shape = GroupCueID,linetype = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-10,20,1)) +
  ggtitle("Summation") + xlab("Block 2 Trials") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,5.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))  


retardation_freq <- data_stage5_summary %>% filter(Cue != "Z") %>% 
  ggplot(mapping = aes(x = as.factor(Day), y = CS_Pre, group = GroupCueID, colour = GroupCueID, fill = GroupCueID, shape = GroupCueID,linetype = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-10,20,1)) +
  ggtitle("Retardation") + xlab("Day") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,2.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))  


##### Cue Z figures ====
stage1_freq_z <- data_stage1_summary %>% filter(Cue == "Z") %>% 
  ggplot(mapping = aes(x = as.factor(Day), y = CS_Pre, group = GroupCueID, colour = GroupCueID, fill = GroupCueID, shape = GroupCueID,linetype = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-10,20,1)) +
  ggtitle("Stage 1 - Acquisition") + xlab("Day") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,5.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))  


stage2_freq_z <- data_stage2_summary %>% filter(Cue == "Z") %>% 
  ggplot(mapping = aes(x = as.factor(Day), y = CS_Pre, group = GroupCueID, colour = GroupCueID, fill = GroupCueID, shape = GroupCueID,linetype = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-10,20,1)) +
  ggtitle("Stage 2 - Feature Negative") + xlab("Day") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,5.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))  

stage3_freq_z <- data_stage3_summary %>% filter(Cue == "Z") %>% 
  ggplot(mapping = aes(x = as.factor(Day), y = CS_Pre, group = GroupCueID, colour = GroupCueID, fill = GroupCueID, shape = GroupCueID,linetype = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-10,20,1)) +
  ggtitle("Reacquisition") + xlab("Day") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,5.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))  


retardation_freq_z <- data_stage5_summary %>% filter(Cue == "Z") %>% 
  ggplot(mapping = aes(x = as.factor(Day), y = CS_Pre, group = GroupCueID, colour = GroupCueID, fill = GroupCueID, shape = GroupCueID,linetype = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-10,20,1)) +
  ggtitle("Retardation") + xlab("Day") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,5.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))  

#### Consumption Test Figures ====

consumption_freq <- full_consumption_data %>% 
  ggplot(mapping = aes(x = as.factor(Block5min), y = MagEntries, group = InfusionGroup, colour = InfusionGroup, fill = InfusionGroup, shape = InfusionGroup,linetype = InfusionGroup)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  facet_wrap( ~ Infusion, strip.position = "bottom") +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-10,1000,10)) +
  ggtitle("Consumption Test") + xlab("Block 5 mins") + ylab("Magazine Entry") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,100.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  scale_linetype_manual(name = "", values = linetypes_infusion)  +
  scale_colour_manual(name = "", values = linecolours_infusion, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes_infusion) +
  scale_fill_manual(name = "", values = fillcolours_infusion) +
  theme(legend.key.width=unit(1,"line"))  


##### Figure Layout and Save ----

A = stage1_freq + theme(legend.position= c(0.05,.9), 
                        legend.justification='left',
                        legend.direction='vertical') 

B = stage2_freq + theme(legend.position= c(0.05,.9), 
                        legend.justification='left',
                        legend.direction='vertical', 
                        axis.title.y = element_blank())

C = stage3_freq + theme(legend.position= c(0.05,.95), 
                        legend.justification='left',
                        legend.direction='vertical', 
                        axis.title.y = element_blank())

D = summation_freq + theme(legend.position= c(0.05,.90), 
        legend.justification='left',
        legend.direction='vertical', 
        axis.title.y = element_blank())

E = retardation_freq + theme(legend.position= c(0.05,.9), 
                             legend.justification='left',
                             legend.direction='vertical')




# summation_freq_2trials
# summation_freq_alltrials

Fig2 <- (A + B + C + D + E) + plot_annotation(tag_levels = 'A') + plot_layout(nrow = 1, widths = c(1.2,1.8,1,1.1,1))

filename = here("figures", "Fig2.png")
ggsave(filename, Fig2, width =  200, height = 100, units = "mm", dpi = 1200)

filename = here("figures", "Fig2.pdf")
ggsave(filename, Fig2, width =  200, height = 100, units = "mm")


#Supplementary figures
Fig_S2 <-summation_freq_2trials

filename = here("figures", "Fig_S2.png")
ggsave(filename, Fig_S2, width =  80 , height = 80, units = "mm", dpi = 1200)

filename = here("figures", "Fig_S2.pdf")
ggsave(filename, Fig_S2, width =  80 , height = 80, units = "mm")

#Figure 2 Cue Z
A2<-stage1_freq_z + theme(legend.position= c(0.05,.95), 
                        legend.justification='left',
                        legend.direction='vertical') 

B2<-stage2_freq_z + theme(legend.position= c(0.05,.95), 
                        legend.justification='left',
                        legend.direction='vertical', 
                        axis.title.y = element_blank())

C2<-stage3_freq_z + theme(legend.position= c(0.05,.95), 
                        legend.justification='left',
                        legend.direction='vertical', 
                        axis.title.y = element_blank())


D2<-retardation_freq_z + theme(legend.position= c(0.05,.95), 
                             legend.justification='left',
                             legend.direction='vertical', 
                             axis.title.y = element_blank())



Fig3 <- (A2 + B2 + C2 + D2) + plot_annotation(tag_levels = 'A')+ plot_layout(nrow = 1, widths = c(1.2,2,1,1))

filename = here("figures", "Fig3.png")
ggsave(filename, Fig3, width =  160, height = 80, units = "mm", dpi = 1200) 

filename = here("figures", "Fig3.pdf")
ggsave(filename, Fig3, width =  160, height = 80, units = "mm") 



Fig_S3 <-consumption_freq + theme(legend.position= c(0.05,.95), 
                             legend.justification='left',
                             legend.direction='vertical')
filename = here("figures", "Fig_S3.png")
ggsave(filename, Fig_S3, width =  86, height = 80, units = "mm", dpi = 1200) 

filename = here("figures", "Fig_S3.pdf")
ggsave(filename, Fig_S3, width =  86, height = 80, units = "mm") 


##### Save Plot Data as CSV for publication ----

data_Fig2A <- data_stage1_summary %>% filter(Cue != "Z")
data_Fig2B <- data_stage2_summary %>% filter(Cue != "Z")
data_Fig2C <- data_stage3_summary %>% filter(Cue != "Z")
data_Fig2D <- data_stage4_summary %>% filter(!Cue %in%  c("Z", "Y"), US == "0")
data_Fig2E <- data_stage5_summary %>% filter(Cue != "Z")

data_Fig_S2A <- data_stage4_summary_Block2Trials %>% filter(!Cue %in%  c("Z", "Y"), US == "0", `Trial Number Summation Test Blocked` == 1)

data_Fig3A <- data_stage1_summary %>% filter(Cue == "Z")
data_Fig3B <- data_stage2_summary %>% filter(Cue == "Z")
data_Fig3C <- data_stage3_summary %>% filter(Cue == "Z")
data_Fig3D <- data_stage5_summary %>% filter(Cue == "Z")

data_Fig_S3A <- full_consumption_data

dataset_name <-  c("data_Fig2A",
                  "data_Fig2B",
                  "data_Fig2C",
                  "data_Fig2D",
                  "data_Fig2E",
                  "data_Fig_S2A",
                  "data_Fig3A",
                  "data_Fig3B",
                  "data_Fig3C",
                  "data_Fig3D",
                  "data_Fig_S3A")


for (i in dataset_name) { 
filename = here("figures", "figure_data", str_c(i,".csv"))
temp_data = as.name(i)
write_csv(eval(temp_data), filename)
}


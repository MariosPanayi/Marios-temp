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



# Plot Style Parameters ---------------------------------------------------

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



fillcolours <- c("A++--" = DarkRed,
                 "B+-" = LightRed,
                 "C++" = DarkBlue,
                 "D+" = LightBlue,
                 "High" = DarkGrey,
                 "Low" = White,
                 "P100" = DarkGrey,
                 "P50" = White,
                 "Diff" = DarkGrey,
                 "PercDiff" = DarkGrey,
                 "P100_HighVsLow" = DarkRed,
                 "P50_HighVsLow" = LightRed,
                 "High_100Vs50" = DarkBlue,
                 "Low_100Vs50" = LightBlue)

linecolours <- c("A++--" = DarkRed,
                 "B+-" = MediumRed,
                 "C++" = DarkBlue,
                 "D+" = MediumBlue,
                 "High" = Black,
                 "Low" = Black,
                 "P100" = Black,
                 "P50" = Black,
                 "Diff" = Black,
                 "PercDiff" = Black,
                 "P100_HighVsLow" = Black,
                 "P50_HighVsLow" = Black,
                 "High_100Vs50" = Black,
                 "Low_100Vs50" = Black)


linetypes <- c("A++--" = "dotted",
               "B+-" = "dotted",
               "C++" = "solid",
               "D+" = "solid",
               "High" = "solid",
               "Low" = "solid",
               "P100" = "dotted",
               "P50" = "dotted",
               "Diff" = "solid",
               "PercDiff" = "solid",
               "P100_HighVsLow" = "solid",
               "P50_HighVsLow" = "solid",
               "High_100Vs50" = "solid",
               "Low_100Vs50" = "solid")


pointshapes <- c("A++--" = circle,
                 "B+-" = circle,
                 "C++" = square,
                 "D+" = square,
                 "High" = circle,
                 "Low" = circle,
                 "P100" = square,
                 "P50" = square,
                 "Diff" = square,
                 "PercDiff" = square,
                 "P100_HighVsLow" = square,
                 "P50_HighVsLow" = square,
                 "High_100Vs50" = square,
                 "Low_100Vs50" = square)


# Load Data ---------------------------------------------------------------

#  Acquisition Data -------------------------------------------------------

folderpath <- here("rawdata","Marios","2_ConditionedReinforcement","CombinedData")
filename <- "CRF_ProcessedData_pertrial_1sbins.csv"

rawdata <- read_csv(here(folderpath,filename))

# Fix Day factor to numeric
rawdata <- rawdata %>% 
  mutate(Day = as.numeric(str_remove(Day, "Day")))

# Add factor separating probability and reward magnitude 
CS_name <- c("A++--",
             "B+-",
             "C++",
             "D+")

probability <- c(50,
                 50,
                 100,
                 100)

magnitude <- c("High",
     "Low",
     "High",
     "Low")

factorlookup <- data.frame(CS_name, probability, magnitude)
rawdata <- left_join(rawdata, factorlookup, by = "CS_name")
  


data_PerSession <- rawdata %>% 
  group_by(Day, subject, probability, magnitude, sex, CS_name, Period) %>% 
  summarise(MagEntries = mean(A3_freq)*10,
            MagDuration = mean(A3_dur)*10) %>%
  ungroup()

data_PerSession_CSPre <- data_PerSession %>% 
  pivot_wider(names_from = Period,values_from = c(MagEntries, MagDuration)) %>% 
  mutate(MagEntries_CSPre = MagEntries_CS - MagEntries_Pre,
         MagDuration_CSPre = MagDuration_CS - MagDuration_Pre) %>% 
  pivot_longer(c(MagEntries_CS, MagEntries_Post, MagEntries_Pre, MagDuration_CS, MagDuration_Post, MagDuration_Pre, MagEntries_CSPre, MagDuration_CSPre), names_to = c("Measure", "Period"), names_sep = "_", values_to = "Mag") %>% 
  pivot_wider(names_from = Measure, values_from = Mag)
  

data_PerSession_last5s <- rawdata %>% 
  filter(bin_timewithin > 5) %>% 
  group_by(Day, subject,probability, magnitude, sex, CS_name, Period) %>% 
  summarise(MagEntries = mean(A3_freq)*5,
            MagDuration = mean(A3_dur)*5) %>%
  ungroup()


data_PerSession_last5s_CSPre <- data_PerSession_last5s %>% 
  pivot_wider(names_from = Period,values_from = c(MagEntries, MagDuration)) %>% 
  mutate(MagEntries_CSPre = MagEntries_CS - MagEntries_Pre,
         MagDuration_CSPre = MagDuration_CS - MagDuration_Pre) %>% 
  pivot_longer(c(MagEntries_CS, MagEntries_Post, MagEntries_Pre, MagDuration_CS, MagDuration_Post, MagDuration_Pre, MagEntries_CSPre, MagDuration_CSPre), names_to = c("Measure", "Period"), names_sep = "_", values_to = "Mag") %>% 
  pivot_wider(names_from = Measure, values_from = Mag)


# Acquisition Plots -------------------------------------------------------



Acqsuisition_Stage1_MagFreq <- data_PerSession_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(Day), y = MagEntries, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  # facet_wrap(~ sex) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Acquisition") + xlab("Day") + ylab("Magazine Entry 10s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,6.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

Acqsuisition_Stage1_MagFreq


Acqsuisition_Stage1_MagDur <- data_PerSession_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(Day), y = MagDuration, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  # facet_wrap(~ sex) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Acquisition") + xlab("Day") + ylab("Magazine Durations 10s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,6.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

Acqsuisition_Stage1_MagDur


Acqsuisition_Stage1_MagFreq_last5s <- data_PerSession_last5s_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(Day), y = MagEntries, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  # facet_wrap(~ sex) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Acquisition") + xlab("Day") + ylab("Magazine Entry 5s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,6.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

Acqsuisition_Stage1_MagFreq_last5s


Acqsuisition_Stage1_MagDur_last5s <- data_PerSession_last5s_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(Day), y = MagDuration, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  # facet_wrap(~ sex) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Acquisition") + xlab("Day") + ylab("Magazine Durations 5s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,6.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

Acqsuisition_Stage1_MagDur_last5s

# Enhanced Acquisition Period - Plot separately and collapse over days


EnhancedAcqsuisition_Stage1_MagFreq <- data_PerSession_CSPre %>% 
  filter(Period == "CSPre",
         Day >= 21) %>%
  group_by(subject, probability, magnitude, sex, CS_name, Period) %>% 
  summarise(MagEntries = mean(MagEntries),
            MagDuration = mean(MagDuration)) %>%
  ungroup() %>% 
  ggplot(mapping = aes(x = as.factor(CS_name), y = MagEntries, group = CS_name, colour = CS_name, fill = CS_name)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # geom_point(aes(group = subject), colour = Black) +
  # geom_line(aes(group = subject), colour = Black) +
  # facet_wrap(~Day,) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,1)) +
  ggtitle("Enhanced Acquisition: Final 2 days") + xlab("CS Identity") + ylab("Magazine Durations 10s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,6.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(0.5,"line"))

EnhancedAcqsuisition_Stage1_MagFreq

# #Inspect individual animals
# 
# data_PerSession_CSPre %>% 
#   filter(Period == "CS") %>% 
#   select(-MagEntries, -Period) %>%
#   pivot_wider(names_from = subject, values_from = MagDuration) %>% 
#   kable()
# 
# data_PerSession_CSPre %>%
#   filter(Period == "CS") %>%
#   select(-MagDuration, -Period, -sex) %>%
#   pivot_wider(names_from = subject, values_from = MagEntries) %>%
#   kable()
# 
# data_PerSession_CSPre %>%
#   filter(Period == "CS") %>%
#   select(-MagEntries, -Period, -sex) %>%
#   pivot_wider(names_from = subject, values_from = MagDuration) %>%
#   kable()
# 
# 


# Save Stage 1 Data for analysis ------------------------------------------


savefile <- "CRF_Acquisition_CSPre.csv"
write_csv(data_PerSession_CSPre, here("figures", "figure_data", savefile))


savefile <- "CRF_Acquisition_CSPre_Last5s.csv"
write_csv(data_PerSession_last5s_CSPre, here("figures", "figure_data", savefile))


# Load CRF Test Data  -----------------------------------------------------




folderpath <- here("rawdata","Marios","2_ConditionedReinforcement","CombinedData")
filename <- "CRF_ProcessedData_CRFTests_withinSession1minBin.csv"

rawdata <- read_csv(here(folderpath,filename))
# Add 10 min bin filter
rawdata <- rawdata %>% 
  mutate(timebins_10 = ceiling(timebins/10))


# Data: P100_HighVsLow ----------------------------------------------------

# Separate each condition for separate analysis
data_P100_HighvsLow <- rawdata %>% 
  filter(TestCondition=="P100_HighVsLow")

## Recode lever identity and reward identity based on counterbalancing
data_P100_HighvsLow <- data_P100_HighvsLow %>% 
  group_by(Day, subject) %>% 
  mutate(LP_P100_High  = ifelse(CS_LeftLeverName == "C++", A1_freq, ifelse(CS_RightLeverName == "C++", A2_freq, NA)),
         LP_P100_Low  = ifelse(CS_LeftLeverName == "D+", A1_freq, ifelse(CS_RightLeverName == "D+", A2_freq, NA)),
         # LP_P100_High_dur  = ifelse(CS_LeftLeverName == "C++", A1_dur, ifelse(CS_RightLeverName == "C++", A2_dur, NA)),
         # LP_P100_Low_dur  = ifelse(CS_LeftLeverName == "D+", A1_dur, ifelse(CS_RightLeverName == "D+", A2_dur, NA)),
         CS_P100_High = ifelse(CS_LeftLeverName == "C++", ifelse(Left_LeverCueID == "Click", 
                                                                 Click_freq, ifelse(Left_LeverCueID == "Noise", Noise_freq, 
                                                                                    ifelse(Left_LeverCueID == "Tone", Tone_freq, 
                                                                                           ifelse(Left_LeverCueID == "Siren", Siren_freq, NA )))), 
                               ifelse(CS_RightLeverName == "C++", ifelse(Right_LeverCueID == "Click", 
                                                                        Click_freq, ifelse(Right_LeverCueID == "Noise", Noise_freq, 
                                                                                           ifelse(Right_LeverCueID == "Tone", Tone_freq, 
                                                                                                  ifelse(Right_LeverCueID == "Siren", Siren_freq, NA ))))) ),
         CS_P100_Low = ifelse(CS_LeftLeverName == "D+", ifelse(Left_LeverCueID == "Click", 
                                                                 Click_freq, ifelse(Left_LeverCueID == "Noise", Noise_freq, 
                                                                                    ifelse(Left_LeverCueID == "Tone", Tone_freq, 
                                                                                           ifelse(Left_LeverCueID == "Siren", Siren_freq, NA )))), 
                               ifelse(CS_RightLeverName == "D+", ifelse(Right_LeverCueID == "Click", 
                                                                         Click_freq, ifelse(Right_LeverCueID == "Noise", Noise_freq, 
                                                                                            ifelse(Right_LeverCueID == "Tone", Tone_freq, 
                                                                                                   ifelse(Right_LeverCueID == "Siren", Siren_freq, NA ))))) ),
         
         
  ) %>% 
  ungroup()

data_P100_HighvsLow_long <- data_P100_HighvsLow %>% 
  pivot_longer(c(LP_P100_High, LP_P100_Low, CS_P100_High, CS_P100_Low), names_to = c("measure", "Probability", "Value"), names_sep = "_" ,values_to = "Freq") 




# Data: P50_HighVsLow ----------------------------------------------------

# Separate each condition for separate analysis
data_P50_HighVsLow <- rawdata %>% 
  filter(TestCondition=="P50_HighVsLow")

## Recode lever identity and reward identity based on counterbalancing
data_P50_HighVsLow <- data_P50_HighVsLow %>% 
  group_by(Day, subject) %>% 
  mutate(LP_P50_High  = ifelse(CS_LeftLeverName == "A++--", A1_freq, ifelse(CS_RightLeverName == "A++--", A2_freq, NA)),
         LP_P50_Low  = ifelse(CS_LeftLeverName == "B+-", A1_freq, ifelse(CS_RightLeverName == "B+-", A2_freq, NA)),
         # LP_P50_High_dur  = ifelse(CS_LeftLeverName == "A++--", A1_dur, ifelse(CS_RightLeverName == "A++--", A2_dur, NA)),
         # LP_P50_Low_dur  = ifelse(CS_LeftLeverName == "B+-", A1_dur, ifelse(CS_RightLeverName == "B+-", A2_dur, NA)),
         CS_P50_High = ifelse(CS_LeftLeverName == "A++--", ifelse(Left_LeverCueID == "Click", 
                                                                  Click_freq, ifelse(Left_LeverCueID == "Noise", Noise_freq, 
                                                                                     ifelse(Left_LeverCueID == "Tone", Tone_freq, 
                                                                                            ifelse(Left_LeverCueID == "Siren", Siren_freq, NA )))), 
                              ifelse(CS_RightLeverName == "A++--", ifelse(Right_LeverCueID == "Click", 
                                                                          Click_freq, ifelse(Right_LeverCueID == "Noise", Noise_freq, 
                                                                                             ifelse(Right_LeverCueID == "Tone", Tone_freq, 
                                                                                                    ifelse(Right_LeverCueID == "Siren", Siren_freq, NA ))))) ),
         CS_P50_Low = ifelse(CS_LeftLeverName == "B+-", ifelse(Left_LeverCueID == "Click", 
                                                               Click_freq, ifelse(Left_LeverCueID == "Noise", Noise_freq, 
                                                                                  ifelse(Left_LeverCueID == "Tone", Tone_freq, 
                                                                                         ifelse(Left_LeverCueID == "Siren", Siren_freq, NA )))), 
                             ifelse(CS_RightLeverName == "B+-", ifelse(Right_LeverCueID == "Click", 
                                                                       Click_freq, ifelse(Right_LeverCueID == "Noise", Noise_freq, 
                                                                                          ifelse(Right_LeverCueID == "Tone", Tone_freq, 
                                                                                                 ifelse(Right_LeverCueID == "Siren", Siren_freq, NA ))))) ),
         
         
  ) %>% 
  ungroup()

data_P50_HighVsLow_long <- data_P50_HighVsLow %>% 
  pivot_longer(c(LP_P50_High, LP_P50_Low, CS_P50_High, CS_P50_Low), names_to = c("measure", "Probability", "Value"), names_sep = "_" ,values_to = "Freq") 


# Data: High_100Vs50 ----------------------------------------------------

# Separate each condition for separate analysis
data_High_100Vs50 <- rawdata %>% 
  filter(TestCondition=="High_100Vs50")

## Recode lever identity and reward identity based on counterbalancing
data_High_100Vs50 <- data_High_100Vs50 %>% 
  group_by(Day, subject) %>% 
  mutate(LP_P100_High  = ifelse(CS_LeftLeverName == "C++", A1_freq, ifelse(CS_RightLeverName == "C++", A2_freq, NA)),
         LP_P50_High  = ifelse(CS_LeftLeverName == "A++--", A1_freq, ifelse(CS_RightLeverName == "A++--", A2_freq, NA)),
         # LP_P100_High_dur  = ifelse(CS_LeftLeverName == "C++", A1_dur, ifelse(CS_RightLeverName == "C++", A2_dur, NA)),
         # LP_P50_High_dur  = ifelse(CS_LeftLeverName == "A++--", A1_dur, ifelse(CS_RightLeverName == "A++--", A2_dur, NA)),
         CS_P100_High = ifelse(CS_LeftLeverName == "C++", ifelse(Left_LeverCueID == "Click", 
                                                                 Click_freq, ifelse(Left_LeverCueID == "Noise", Noise_freq, 
                                                                                    ifelse(Left_LeverCueID == "Tone", Tone_freq, 
                                                                                           ifelse(Left_LeverCueID == "Siren", Siren_freq, NA )))), 
                               ifelse(CS_RightLeverName == "C++", ifelse(Right_LeverCueID == "Click", 
                                                                         Click_freq, ifelse(Right_LeverCueID == "Noise", Noise_freq, 
                                                                                            ifelse(Right_LeverCueID == "Tone", Tone_freq, 
                                                                                                   ifelse(Right_LeverCueID == "Siren", Siren_freq, NA ))))) ),
         CS_P50_High = ifelse(CS_LeftLeverName == "A++--", ifelse(Left_LeverCueID == "Click", 
                                                                  Click_freq, ifelse(Left_LeverCueID == "Noise", Noise_freq, 
                                                                                     ifelse(Left_LeverCueID == "Tone", Tone_freq, 
                                                                                            ifelse(Left_LeverCueID == "Siren", Siren_freq, NA )))), 
                              ifelse(CS_RightLeverName == "A++--", ifelse(Right_LeverCueID == "Click", 
                                                                          Click_freq, ifelse(Right_LeverCueID == "Noise", Noise_freq, 
                                                                                             ifelse(Right_LeverCueID == "Tone", Tone_freq, 
                                                                                                    ifelse(Right_LeverCueID == "Siren", Siren_freq, NA ))))) ),
         
         
  ) %>% 
  ungroup()

data_High_100Vs50_long <- data_High_100Vs50 %>% 
  pivot_longer(c(LP_P100_High, LP_P50_High, CS_P100_High, CS_P50_High), names_to = c("measure", "Probability", "Value"), names_sep = "_" ,values_to = "Freq") 




# Data: Low_100Vs50 ----------------------------------------------------

# Separate each condition for separate analysis
data_Low_100Vs50 <- rawdata %>% 
  filter(TestCondition=="Low_100Vs50")

## Recode lever identity and reward identity based on counterbalancing
data_Low_100Vs50 <- data_Low_100Vs50 %>% 
  group_by(Day, subject) %>% 
  mutate(LP_P100_Low  = ifelse(CS_LeftLeverName == "D+", A1_freq, ifelse(CS_RightLeverName == "D+", A2_freq, NA)),
         LP_P50_Low  = ifelse(CS_LeftLeverName == "B+-", A1_freq, ifelse(CS_RightLeverName == "B+-", A2_freq, NA)),
         # LP_P100_Low_dur  = ifelse(CS_LeftLeverName == "D+", A1_dur, ifelse(CS_RightLeverName == "D+", A2_dur, NA)),
         # LP_P50_Low_dur  = ifelse(CS_LeftLeverName == "B+-", A1_dur, ifelse(CS_RightLeverName == "B+-", A2_dur, NA)),
         CS_P100_Low = ifelse(CS_LeftLeverName == "D+", ifelse(Left_LeverCueID == "Click", 
                                                               Click_freq, ifelse(Left_LeverCueID == "Noise", Noise_freq, 
                                                                                  ifelse(Left_LeverCueID == "Tone", Tone_freq, 
                                                                                         ifelse(Left_LeverCueID == "Siren", Siren_freq, NA )))), 
                              ifelse(CS_RightLeverName == "D+", ifelse(Right_LeverCueID == "Click", 
                                                                       Click_freq, ifelse(Right_LeverCueID == "Noise", Noise_freq, 
                                                                                          ifelse(Right_LeverCueID == "Tone", Tone_freq, 
                                                                                                 ifelse(Right_LeverCueID == "Siren", Siren_freq, NA ))))) ),
         CS_P50_Low = ifelse(CS_LeftLeverName == "B+-", ifelse(Left_LeverCueID == "Click", 
                                                               Click_freq, ifelse(Left_LeverCueID == "Noise", Noise_freq, 
                                                                                  ifelse(Left_LeverCueID == "Tone", Tone_freq, 
                                                                                         ifelse(Left_LeverCueID == "Siren", Siren_freq, NA )))), 
                             ifelse(CS_RightLeverName == "B+-", ifelse(Right_LeverCueID == "Click", 
                                                                       Click_freq, ifelse(Right_LeverCueID == "Noise", Noise_freq, 
                                                                                          ifelse(Right_LeverCueID == "Tone", Tone_freq, 
                                                                                                 ifelse(Right_LeverCueID == "Siren", Siren_freq, NA ))))) ),
         
         
  ) %>% 
  ungroup()

data_Low_100Vs50_long <- data_Low_100Vs50 %>% 
  pivot_longer(c(LP_P100_Low, LP_P50_Low, CS_P100_Low, CS_P50_Low), names_to = c("measure", "Probability", "Value"), names_sep = "_" ,values_to = "Freq")

# Data: Process exclusions ------------------------------------------------


# Test Data Create average session data
CRF_data_P100_HighVsLow_long_Avg <- data_P100_HighvsLow_long %>% 
  group_by(TestCondition, Day, subject, measure, Value, Probability) %>% 
  summarise(Freq = sum(Freq)) 

CRF_data_P50_HighVsLow_long_Avg <- data_P50_HighVsLow_long %>% 
  group_by(TestCondition, Day, subject, measure, Value, Probability) %>% 
  summarise(Freq = sum(Freq)) 

CRF_data_High_100Vs50_long_Avg <- data_High_100Vs50_long %>% 
  group_by(TestCondition, Day, subject, measure, Value, Probability) %>% 
  summarise(Freq = sum(Freq)) 

CRF_data_Low_100Vs50_long_Avg <- data_Low_100Vs50_long %>% 
  group_by(TestCondition, Day, subject, measure, Value, Probability) %>% 
  summarise(Freq = sum(Freq)) 

## Check things are being averaged correctly 
# CRF_data_P100_HighVsLow_long_Avg %>% 
#      count(unique(subject)) %>% 
# kable()


CRF_data_P100_HighVsLow_long_Avg <- CRF_data_P100_HighVsLow_long_Avg %>% 
  pivot_wider(names_from = c(Value, Probability, measure), values_from =  Freq) %>% 
  mutate(ExcludeSubject = ifelse(High_P100_CS == 0 | Low_P100_CS == 0, "Exclude", "Keep"),
         Diff_P100_CS = High_P100_CS - Low_P100_CS,
         Diff_P100_LP = High_P100_LP - Low_P100_LP,
         PercDiff_P100_LP = 100*(High_P100_LP - Low_P100_LP)/ (High_P100_LP + Low_P100_LP)) %>% 
  pivot_longer(c(4:7, 9:11),names_to = c("Value", "Probability", "measure"), names_sep = "_" , values_to = c("Freq"))

CRF_data_P50_HighVsLow_long_Avg <- CRF_data_P50_HighVsLow_long_Avg %>% 
  pivot_wider(names_from = c(Value, Probability, measure), values_from =  Freq) %>% 
  mutate(ExcludeSubject = ifelse(High_P50_CS == 0 | Low_P50_CS == 0, "Exclude", "Keep"),
         Diff_P50_CS = High_P50_CS - Low_P50_CS,
         Diff_P50_LP = High_P50_LP - Low_P50_LP,
         PercDiff_P50_LP = 100*(High_P50_LP - Low_P50_LP)/ (High_P50_LP + Low_P50_LP)) %>% 
  pivot_longer(c(4:7, 9:11),names_to = c("Value", "Probability", "measure"), names_sep = "_" , values_to = c("Freq"))

CRF_data_High_100Vs50_long_Avg <- CRF_data_High_100Vs50_long_Avg %>% 
  pivot_wider(names_from = c(Value, Probability, measure), values_from =  Freq) %>% 
  mutate(ExcludeSubject = ifelse(High_P100_CS == 0 | High_P50_CS == 0, "Exclude", "Keep"),
         Diff_High_CS = High_P100_CS - High_P50_CS,
         Diff_High_LP = High_P100_LP - High_P50_LP,
         PercDiff_High_LP = 100*(High_P100_LP - High_P50_LP)/ (High_P100_LP + High_P50_LP)) %>% 
  pivot_longer(c(4:7, 9:11),names_to = c("Value", "Probability", "measure"), names_sep = "_" , values_to = c("Freq"))

CRF_data_Low_100Vs50_long_Avg <- CRF_data_Low_100Vs50_long_Avg %>% 
  pivot_wider(names_from = c(Value, Probability, measure), values_from =  Freq) %>% 
  mutate(ExcludeSubject = ifelse(Low_P100_CS == 0 | Low_P50_CS == 0, "Exclude", "Keep"),
         Diff_Low_CS = Low_P100_CS - Low_P50_CS,
         Diff_Low_LP = Low_P100_LP - Low_P50_LP,
         PercDiff_Low_LP = 100*(Low_P100_LP - Low_P50_LP)/ (Low_P100_LP + Low_P50_LP)) %>% 
  pivot_longer(c(4:7, 9:11),names_to = c("Value", "Probability", "measure"), names_sep = "_" , values_to = c("Freq"))

# COmbine all into one table to difference scores plot simultaneously
CRF_ALL_data_long_Avg <- full_join(full_join(CRF_data_P100_HighVsLow_long_Avg,CRF_data_P50_HighVsLow_long_Avg),
full_join(CRF_data_High_100Vs50_long_Avg,CRF_data_Low_100Vs50_long_Avg) )


savefile <- "CRF_ALL_data_long_Avg.csv"
write_csv(CRF_ALL_data_long_Avg, here("figures", "figure_data", savefile))

# 
# 
# # Save CRF Test Plot Data -------------------------------------------------
# 
# savefile <- "CRF_data_P100_HighvsLow_long.csv"
# write_csv(data_P100_HighvsLow_long, here("figures", "figure_data", savefile))
# 
# 
# savefile <- "CRF_data_P50_HighvsLow_long.csv"
# write_csv(data_P50_HighVsLow_long, here("figures", "figure_data", savefile))
# 
# 
# savefile <- "CRF_data_High_100Vs50_long.csv"
# write_csv(data_High_100Vs50_long, here("figures", "figure_data", savefile))
# 
# 
# savefile <- "CRF_data_Low_100Vs50_long.csv"
# write_csv(data_Low_100Vs50_long, here("figures", "figure_data", savefile))
# 

# Plots: P100_HighVsLow ---------------------------------------------------

P100_HighvsLow_TotalLP <- CRF_data_P100_HighVsLow_long_Avg %>% 
  filter(measure == "LP",
         Value != "Diff" & Value != "PercDiff",
         ExcludeSubject == "Keep") %>% 
  ggplot(mapping = aes(x = as.factor(Value), y = Freq, group = Value, colour = Value, fill = Value)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # geom_point(aes(group = subject), colour = Black) +
  # geom_line(aes(group = subject), colour = Black) +
  # facet_wrap(~Day,) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,10)) +
  ggtitle("P(100): High vs. Low Value") + xlab("CS Value") + ylab("Total LP (30 mins)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,60.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(0.5,"line"))
P100_HighvsLow_TotalLP

P100_HighvsLow_TotalCS <- CRF_data_P100_HighVsLow_long_Avg %>% 
  filter(measure == "CS",
         Value != "Diff" & Value != "PercDiff",
         ExcludeSubject == "Keep") %>% 
  ggplot(mapping = aes(x = as.factor(Value), y = Freq, group = Value, colour = Value, fill = Value)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # geom_point(aes(group = subject), colour = Black) +
  # geom_line(aes(group = subject), colour = Black) +
  # facet_wrap(~Day,) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,5)) +
  ggtitle("P(100): High vs. Low Value") + xlab("CS Value") + ylab("Total CS (30 mins)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,30.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(0.5,"line"))
P100_HighvsLow_TotalCS


P100_HighvsLow_TotalLPDiff <- CRF_data_P100_HighVsLow_long_Avg %>% 
  filter(measure == "LP",
         Value == "PercDiff",
         ExcludeSubject == "Keep") %>% 
  ggplot(mapping = aes(x = as.factor(Value), y = Freq, group = Value, colour = Value, fill = Value)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  geom_point(aes(group = subject), colour = Black) +
  # geom_line(aes(group = subject), colour = Black) +
  # facet_wrap(~Day,) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,10)) +
  ggtitle("P(100): High vs. Low Value") + xlab("High - Low") + ylab("Percent Difference") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-100,100.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(0.5,"line"))
P100_HighvsLow_TotalLPDiff

# 
# 
# P100_HighvsLow_1minBins <- data_P100_HighvsLow_long %>% 
#   group_by(Day, subject, measure, Value, timebins_10) %>% 
#   summarise(Freq = sum(Freq)) %>% 
#   filter(measure == "LP") %>% 
#   ggplot(mapping = aes(x = as.factor(timebins_10), y = Freq, group = Value, colour = Value, fill = Value, shape = Value,linetype = Value)) +
#   # facet_wrap(~ sex) +
#   stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
#   stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
#   stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
#   # Make Pretty
#   scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
#   ggtitle("P(100): High vs. Low Value") + xlab("10 min bins") + ylab("Total LP (30 mins)") +
#   theme_cowplot(11) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(plot.title = element_text(size=10)) +
#   coord_cartesian(ylim = c(0,20.0001)) +
#   theme(axis.title.x=element_text(face = "bold")) +
#   scale_linetype_manual(name = "", values = linetypes)  +
#   scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
#   scale_shape_manual(name = "", values = pointshapes) +
#   scale_fill_manual(name = "", values = fillcolours) +
#   theme(legend.key.width=unit(1,"line"))
# P100_HighvsLow_1minBins


# Plots: P50_HighVsLow ---------------------------------------------------


P50_HighvsLow_TotalLP <- CRF_data_P50_HighVsLow_long_Avg %>% 
  filter(measure == "LP",
         Value != "Diff" & Value != "PercDiff",
         ExcludeSubject == "Keep")  %>% 
  ggplot(mapping = aes(x = as.factor(Value), y = Freq, group = Value, colour = Value, fill = Value)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # geom_point(aes(group = subject), colour = "black") +
  # facet_wrap(~Day,) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,10)) +
  ggtitle("P(50): High vs. Low Value") + xlab("CS Value") + ylab("Total LP (30 mins)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,60.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(0.5,"line"))
P50_HighvsLow_TotalLP

P50_HighvsLow_TotalCS <- CRF_data_P50_HighVsLow_long_Avg %>% 
  filter(measure == "CS",
         Value != "Diff" & Value != "PercDiff",
         ExcludeSubject == "Keep")  %>%  
  ggplot(mapping = aes(x = as.factor(Value), y = Freq, group = Value, colour = Value, fill = Value)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # geom_point(aes(group = subject), colour = "black") +
  # facet_wrap(~Day,) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,5)) +
  ggtitle("P(50): High vs. Low Value") + xlab("CS Value") + ylab("Total CS (30 mins)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,25.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(0.5,"line"))
P50_HighvsLow_TotalCS

P50_HighvsLow_TotalLPDiff <- CRF_data_P50_HighVsLow_long_Avg %>% 
  filter(measure == "LP",
         Value == "PercDiff",
         ExcludeSubject == "Keep") %>% 
  ggplot(mapping = aes(x = as.factor(Value), y = Freq, group = Value, colour = Value, fill = Value)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  geom_point(aes(group = subject), colour = Black) +
  # geom_line(aes(group = subject), colour = Black) +
  # facet_wrap(~Day,) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,10)) +
  ggtitle("P(100): High vs. Low Value") + xlab("High - Low") + ylab("Percent Difference") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-100,100.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(0.5,"line"))
P50_HighvsLow_TotalLPDiff

# 
# 
# P50_HighvsLow_1minBins <- data_P50_HighVsLow_long %>% 
#   group_by(Day, subject, measure, Value, timebins_10) %>% 
#   summarise(Freq = sum(Freq)) %>% 
#   filter(measure == "LP") %>% 
#   ggplot(mapping = aes(x = as.factor(timebins_10), y = Freq, group = Value, colour = Value, fill = Value, shape = Value,linetype = Value)) +
#   # facet_wrap(~ sex) +
#   stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
#   stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
#   stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
#   # Make Pretty
#   scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
#   ggtitle("P(50): High vs. Low Value") + xlab("10 min bins") + ylab("Total LP (30 mins)") +
#   theme_cowplot(11) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(plot.title = element_text(size=10)) +
#   coord_cartesian(ylim = c(0,20.0001)) +
#   theme(axis.title.x=element_text(face = "bold")) +
#   scale_linetype_manual(name = "", values = linetypes)  +
#   scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
#   scale_shape_manual(name = "", values = pointshapes) +
#   scale_fill_manual(name = "", values = fillcolours) +
#   theme(legend.key.width=unit(1,"line"))
# P50_HighvsLow_1minBins



# Plots: High_100Vs50 ---------------------------------------------------



High_100Vs50_TotalLP <- CRF_data_High_100Vs50_long_Avg %>% 
  filter(measure == "LP",
         Value != "Diff" & Value != "PercDiff",
         ExcludeSubject == "Keep")  %>% 
  ggplot(mapping = aes(x = as.factor(Probability), y = Freq, group = Probability, colour = Probability, fill = Probability)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # geom_point(aes(group = subject), colour = "black") +
  # facet_wrap(~Day,) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,10)) +
  ggtitle("High Value: P(100) vs P(50)") + xlab("CS Probability") + ylab("Total LP (30 mins)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,60.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(0.5,"line"))
High_100Vs50_TotalLP

High_100Vs50_TotalCS <- CRF_data_High_100Vs50_long_Avg %>% 
  filter(measure == "CS",
         Value != "Diff" & Value != "PercDiff",
         ExcludeSubject == "Keep")  %>% 
  ggplot(mapping = aes(x = as.factor(Probability), y = Freq, group = Probability, colour = Probability, fill = Probability)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # geom_point(aes(group = subject), colour = "black") +
  # facet_wrap(~Day,) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,5)) +
  ggtitle("High Value: P(100) vs P(50)") + xlab("CS Probability") + ylab("Total CS (30 mins)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,25.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(0.5,"line"))
High_100Vs50_TotalCS


High_100Vs50_TotalLPDiff <- CRF_data_High_100Vs50_long_Avg %>% 
  filter(measure == "LP",
         Value == "PercDiff",
         ExcludeSubject == "Keep") %>% 
  ggplot(mapping = aes(x = as.factor(Value), y = Freq, group = Value, colour = Value, fill = Value)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  geom_point(aes(group = subject), colour = Black) +
  # geom_line(aes(group = subject), colour = Black) +
  # facet_wrap(~Day,) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,10)) +
  ggtitle("P(100): High vs. Low Value") + xlab("P100 - P50") + ylab("Percent Difference") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-100,100.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(0.5,"line"))
High_100Vs50_TotalLPDiff

# 
# 
# 
# High_100Vs50_1minBins <- data_High_100Vs50_long %>% 
#   group_by(Day, subject, measure, Probability, timebins_10) %>% 
#   summarise(Freq = sum(Freq)) %>% 
#   filter(measure == "LP") %>% 
#   ggplot(mapping = aes(x = as.factor(timebins_10), y = Freq, group = Probability, colour = Probability, fill = Probability, shape = Probability,linetype = Probability)) +
#   # facet_wrap(~ sex) +
#   stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
#   stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
#   stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
#   # Make Pretty
#   scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
#   ggtitle("High Value: P(100) vs P(50)") + xlab("10 min bins") + ylab("Total LP (30 mins)") +
#   theme_cowplot(11) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(plot.title = element_text(size=10)) +
#   coord_cartesian(ylim = c(0,20.0001)) +
#   theme(axis.title.x=element_text(face = "bold")) +
#   scale_linetype_manual(name = "", values = linetypes)  +
#   scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
#   scale_shape_manual(name = "", values = pointshapes) +
#   scale_fill_manual(name = "", values = fillcolours) +
#   theme(legend.key.width=unit(1,"line"))
# High_100Vs50_1minBins



# Plots: Low_100Vs50 ---------------------------------------------------


Low_100Vs50_TotalLP <- CRF_data_Low_100Vs50_long_Avg %>% 
  filter(measure == "LP",
         Value != "Diff" & Value != "PercDiff",
         ExcludeSubject == "Keep")  %>% 
  ggplot(mapping = aes(x = as.factor(Probability), y = Freq, group = Probability, colour = Probability, fill = Probability)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # geom_point(aes(group = subject), colour = "black") +
  # facet_wrap(~Day,) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,10)) +
  ggtitle("Low Value: P(100) vs P(50)") + xlab("CS Probability") + ylab("Total LP (30 mins)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,60.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(0.5,"line"))
Low_100Vs50_TotalLP

Low_100Vs50_TotalCS <- CRF_data_Low_100Vs50_long_Avg %>% 
  filter(measure == "CS",
         Value != "Diff" & Value != "PercDiff",
         ExcludeSubject == "Keep")  %>% 
  ggplot(mapping = aes(x = as.factor(Probability), y = Freq, group = Probability, colour = Probability, fill = Probability)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # geom_point(aes(group = subject), colour = "black") +
  # facet_wrap(~Day,) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,5)) +
  ggtitle("Low Value: P(100) vs P(50)") + xlab("CS Probability") + ylab("Total CS (30 mins)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,25.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(0.5,"line"))
Low_100Vs50_TotalCS



Low_100Vs50_TotalLPDiff <- CRF_data_Low_100Vs50_long_Avg %>% 
  filter(measure == "LP",
         Value == "PercDiff",
         ExcludeSubject == "Keep") %>% 
  ggplot(mapping = aes(x = as.factor(Value), y = Freq, group = Value, colour = Value, fill = Value)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  geom_point(aes(group = subject), colour = Black) +
  # geom_line(aes(group = subject), colour = Black) +
  # facet_wrap(~Day,) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,10)) +
  ggtitle("P(100): High vs. Low Value") + xlab("P100 - P50") + ylab("Percent Difference") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-100,100.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(0.5,"line"))
Low_100Vs50_TotalLPDiff

# 
# 
# Low_100Vs50_1minBins <- data_Low_100Vs50_long %>% 
#   group_by(Day, subject, measure, Probability, timebins_10) %>% 
#   summarise(Freq = sum(Freq)) %>% 
#   filter(measure == "LP") %>% 
#   ggplot(mapping = aes(x = as.factor(timebins_10), y = Freq, group = Probability, colour = Probability, fill = Probability, shape = Probability,linetype = Probability)) +
#   # facet_wrap(~ sex) +
#   stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
#   stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
#   stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
#   # Make Pretty
#   scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
#   ggtitle("Low Value: P(100) vs P(50)") + xlab("10 min bins") + ylab("Total LP (30 mins)") +
#   theme_cowplot(11) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(plot.title = element_text(size=10)) +
#   coord_cartesian(ylim = c(0,20.0001)) +
#   theme(axis.title.x=element_text(face = "bold")) +
#   scale_linetype_manual(name = "", values = linetypes)  +
#   scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
#   scale_shape_manual(name = "", values = pointshapes) +
#   scale_fill_manual(name = "", values = fillcolours) +
#   theme(legend.key.width=unit(1,"line"))
# Low_100Vs50_1minBins




# Plot Test All Conditions Difference scores ------------------------------

testdata_All_LP_PercDiff <- CRF_ALL_data_long_Avg %>% 
  filter(measure == "LP",
         Value == "PercDiff",
         ExcludeSubject == "Keep") %>% 
  ggplot(mapping = aes(x = as.factor(TestCondition), y = Freq, group = TestCondition, colour = TestCondition, fill = TestCondition)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  geom_point(aes(group = subject), colour = Black) +
  # geom_line(aes(group = subject), colour = Black) +
  # facet_wrap(~Day,) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,20)) +
  ggtitle("") + xlab("") + ylab("Percent Difference") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-100,100.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(0.5,"line")) + 
  scale_x_discrete(labels = c("P100 - P50\nHigh","P100 - P50\nLow","High - Low\nP100", "High - Low\nP50")) +
  theme(axis.text.x = element_text(face = "bold"))

testdata_All_LP_PercDiff

testdata_All_LP_Diff <- CRF_ALL_data_long_Avg %>% 
  filter(measure == "LP",
         Value == "Diff",
         ExcludeSubject == "Keep") %>% 
  ggplot(mapping = aes(x = as.factor(TestCondition), y = Freq, group = TestCondition, colour = TestCondition, fill = TestCondition)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  geom_point(aes(group = subject), colour = Black) +
  # geom_line(aes(group = subject), colour = Black) +
  # facet_wrap(~Day,) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,20)) +
  ggtitle("") + xlab("") + ylab("LP Difference") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-100,100.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(0.5,"line")) + 
  scale_x_discrete(labels = c("P100 - P50\nHigh","P100 - P50\nLow","High - Low\nP100", "High - Low\nP50")) +
  theme(axis.text.x = element_text(face = "bold"))

testdata_All_LP_Diff


# Save Plots --------------------------------------------------------------

# 
# P100_HighvsLow_TotalLP
# P100_HighvsLow_TotalCS
# 
# P50_HighvsLow_TotalLP
# P50_HighvsLow_TotalCS
# 
# High_100Vs50_TotalLP
# High_100Vs50_TotalCS
# 
# Low_100Vs50_TotalLP
# Low_100Vs50_TotalCS

# Acquisition Data
Acqsuisition_Stage1_MagFreq

A <- Acqsuisition_Stage1_MagFreq + theme(legend.position= c(0.05,.90), 
                                    legend.justification='left',
                                    legend.direction='vertical')

B <- Acqsuisition_Stage1_MagDur + theme(legend.position= c(0.05,.90), 
                                   legend.justification='left',
                                   legend.direction='vertical')


CRF_Acquisition_10s_combined <- (A + B) + plot_annotation(tag_levels = 'A') + plot_layout(nrow = 2, ncol = 1, widths = c(1, 1))

filename = here("figures", "CRF_Acquisition_10s_combined.png")
ggsave(filename, CRF_Acquisition_10s_combined, width = 100, height = 160, units = "mm", dpi = 1200)

## Test Data - LP
A <- P100_HighvsLow_TotalLP + theme(legend.position= c(0.05,.90), 
                                legend.justification='left',
                                legend.direction='vertical')

B <- P50_HighvsLow_TotalLP + theme(legend.position= c(0.05,.90), 
                                legend.justification='left',
                                legend.direction='vertical')

C <- High_100Vs50_TotalLP + theme(legend.position= c(0.05,.90), 
                                legend.justification='left',
                                legend.direction='vertical')

D <- Low_100Vs50_TotalLP + theme(legend.position= c(0.05,.90), 
                               legend.justification='left',
                               legend.direction='vertical')

CRF_Test_TotalLP <- (A + B + C + D) + plot_annotation(tag_levels = 'A') + plot_layout(nrow = 2, ncol = 2, widths = c(1, 1,1,1))

filename = here("figures", "CRF_Test_TotalLP.png")
ggsave(filename, CRF_Test_TotalLP, width = 160, height = 160, units = "mm", dpi = 1200)

## Test Data - CS presentations
A <- P100_HighvsLow_TotalCS + theme(legend.position= c(0.05,.90), 
                                    legend.justification='left',
                                    legend.direction='vertical')

B <- P50_HighvsLow_TotalCS + theme(legend.position= c(0.05,.90), 
                                   legend.justification='left',
                                   legend.direction='vertical')

C <- High_100Vs50_TotalCS + theme(legend.position= c(0.05,.90), 
                                  legend.justification='left',
                                  legend.direction='vertical')

D <- Low_100Vs50_TotalCS + theme(legend.position= c(0.05,.90), 
                                 legend.justification='left',
                                 legend.direction='vertical')

CRF_Test_TotalCS <- (A + B + C + D) + plot_annotation(tag_levels = 'A') + plot_layout(nrow = 2, ncol = 2, widths = c(1, 1,1,1))

filename = here("figures", "CRF_Test_TotalCS.png")
ggsave(filename, CRF_Test_TotalCS, width = 160, height = 160, units = "mm", dpi = 1200)


## Test Data - LP differences All conditions
A <- testdata_All_LP_Diff + theme(legend.position = "none")

B <- testdata_All_LP_PercDiff + theme(legend.position = "none")

CRF_Test_LPDiff_All <- (A + B ) + plot_annotation(tag_levels = 'A') + plot_layout(nrow = 2, ncol = 1, widths = c(1, 1))

filename = here("figures", "CRF_Test_LPDiff_All.png")
ggsave(filename, CRF_Test_LPDiff_All, width = 120, height = 160, units = "mm", dpi = 1200)



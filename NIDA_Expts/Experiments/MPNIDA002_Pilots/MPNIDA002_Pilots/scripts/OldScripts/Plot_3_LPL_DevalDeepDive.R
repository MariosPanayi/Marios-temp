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



fillcolours <- c("Flash" = DarkRed,
                 "Steady" = DarkBlue,
                 "Left" = DarkRed,
                 "Right" = DarkBlue,
                 "Banana" = DarkRed,
                 "Chocolate" = DarkBlue,
                 "Devalued" = Black,
                 "NonDevalued" = White,
                 "Magazine" = LightGrey,
                 "Same" = DarkRed,
                 "Different" = DarkBlue)

linecolours <- c("Flash" = DarkRed,
                 "Steady" = DarkBlue,
                 "Left" = DarkRed,
                 "Right" = DarkBlue,
                 "Banana" = DarkRed,
                 "Chocolate" = DarkBlue,
                 "Devalued" = Black,
                 "NonDevalued" = Black,
                 "Magazine" = MediumGrey,
                 "Same" = DarkRed,
                 "Different" = DarkBlue)


linetypes <- c("Flash" = "dotted",
               "Steady" = "solid",
               "Left" = "dotted",
               "Right" = "solid",
               "Banana" = "dotted",
               "Chocolate" = "solid",
               "Devalued" = "dotted",
               "NonDevalued" = "solid",
               "Magazine" = "solid",
               "Same" = "solid",
               "Different" = "solid")



pointshapes <- c("Flash" = circle,
                 "Steady" = square,
                 "Left" = circle,
                 "Right" = square,
                 "Banana" = circle,
                 "Chocolate" = square,
                 "Devalued" = square,
                 "NonDevalued" = circle,
                 "Magazine" = triangleDown,
                 "Same" = circle,
                 "Different" = square)



# Stage3 Devaluation Test -------------------------------------------------


# Load Data ---------------------------------------------------------------

folderpath <- here("rawdata","Marios","3_LeverPressingForLights","CombinedData")
filename <- "LPL_ProcessedData_DevlautionTest_WithinCue1minBins.csv"

rawdata <- read_csv(here(folderpath,filename))




rawdata <- rawdata %>% 
  separate(bin_state, sep = "_", into = c("Period", "Trial_ID")) %>% 
  mutate(trial_Name = ifelse(Trial_ID == "4", "Flash", ifelse(Trial_ID == "5", "Steady", NA)) )
  



## Recode lever identity based on counterbalancing
data_recode <- rawdata %>% 
  group_by(Day, subject) %>% 
  mutate(LP_Freq_Flash  = ifelse(FLash_leverCbx == "Left", A1_freq, ifelse(FLash_leverCbx == "Right", A2_freq, NA)),
         LP_Dur_Flash  = ifelse(FLash_leverCbx == "Left", A1_dur, ifelse(FLash_leverCbx == "Right", A2_dur, NA)),
         LP_Freq_Steady  = ifelse(Steady_levercbx == "Left", A1_freq, ifelse(Steady_levercbx == "Right", A2_freq, NA)),
         LP_Dur_Steady  = ifelse(Steady_levercbx == "Left", A1_dur, ifelse(Steady_levercbx == "Right", A2_dur, NA)),
         DevaluedStimulus = ifelse(DevaluedOutcome == Flash_OutcomeID, "Flash", ifelse(DevaluedOutcome == Steady_OutcomeID, "Steady", NA)),
         NonDevaluedStimulus = ifelse(DevaluedOutcome != Flash_OutcomeID, "Flash", ifelse(DevaluedOutcome != Steady_OutcomeID, "Steady", NA)),
         DevaluedLever = ifelse(DevaluedStimulus == "Flash", FLash_leverCbx , ifelse(DevaluedStimulus == "Steady", Steady_levercbx, NA)),
         NonDevaluedLever = ifelse(NonDevaluedStimulus == "Flash", FLash_leverCbx , ifelse(NonDevaluedStimulus == "Steady", Steady_levercbx, NA)),
         LP_Freq_Devalued  = ifelse(DevaluedLever == "Left", A1_freq, ifelse(DevaluedLever == "Right", A2_freq, NA)),
         LP_Dur_Devalued  = ifelse(DevaluedLever == "Left", A1_dur, ifelse(DevaluedLever == "Right", A2_dur, NA)),
         LP_Freq_NonDevalued  = ifelse(NonDevaluedLever == "Left", A1_freq, ifelse(NonDevaluedLever == "Right", A2_freq, NA)),
         LP_Dur_NonDevalued  = ifelse(NonDevaluedLever == "Left", A1_dur, ifelse(NonDevaluedLever == "Right", A2_dur, NA)),
         Devalued_Trial = ifelse(DevaluedStimulus == trial_Name, "Devalued", ifelse(NonDevaluedStimulus == trial_Name, "NonDevalued", NA)))   %>% 
  ungroup()




## relabel data
data_bin <- data_recode %>%
  # filter(bin_CueSpecificTrialNum <= 1) %>%
  group_by(Day, counterbalancing, Pavlovian_cbx, DevaluedOutcome, DevaluedStimulus, DevaluedLever, Period ,bin_timewithin, Devalued_Trial, subject) %>%
  summarise(LPFreq_Flash = mean(LP_Freq_Flash, na.rm=TRUE),
            LPDur_Flash = mean(LP_Dur_Flash, na.rm=TRUE),
            LPFreq_Steady = mean(LP_Freq_Steady, na.rm=TRUE),
            LPDur_Steady = mean(LP_Dur_Steady, na.rm=TRUE),
            LPFreq_Devalued = mean(LP_Freq_Devalued, na.rm=TRUE),
            LPDur_Devalued = mean(LP_Dur_Devalued, na.rm=TRUE),
            LPFreq_NonDevalued = mean(LP_Freq_NonDevalued, na.rm=TRUE),
            LPDur_NonDevalued = mean(LP_Dur_NonDevalued, na.rm=TRUE),
            LPFreq_Magazine = mean(A3_freq, na.rm=TRUE),
            LPDur_Magazine = mean(A3_dur, na.rm=TRUE)
  ) %>%
  ungroup()

#Long format  
data_bin_long_StimID <- data_bin %>% 
  pivot_longer(c(LPFreq_Flash, LPDur_Flash, LPFreq_Steady, LPDur_Steady, LPFreq_Magazine, LPDur_Magazine), names_to = c("Measure","Stimulus"), names_sep = "_", values_to = "LP") %>% 
  pivot_wider(names_from = Measure, values_from = LP)

data_bin_long_DevalID <- data_bin %>% 
  pivot_longer(c(LPFreq_Devalued, LPDur_Devalued, LPFreq_NonDevalued, LPDur_NonDevalued, LPFreq_Magazine, LPDur_Magazine), names_to = c("Measure","Stimulus"), names_sep = "_", values_to = "LP") %>% 
  pivot_wider(names_from = Measure, values_from = LP)

# Sum responding over time bins 
data_Period_long_DevalID <- data_bin_long_DevalID %>% 
  group_by(Day, counterbalancing, Pavlovian_cbx, DevaluedOutcome, DevaluedLever, Period, Stimulus, Devalued_Trial, subject) %>% 
  summarise(LPFreq = sum(LPFreq),
            LPDur = sum(LPDur),
            
  ) %>% 
  ungroup()

# Average across both test sessions
data_bin_long_DevalID_Avg <- data_bin_long_DevalID %>% 
  group_by(Devalued_Trial, Period, bin_timewithin, subject, Stimulus) %>% 
  summarise(LPFreq = mean(LPFreq),
            LPDur = mean(LPDur))  %>% 
  ungroup()

data_Period_long_DevalID_Avg <- data_Period_long_DevalID %>% 
  group_by(Devalued_Trial, Period,  subject, Stimulus) %>% 
  summarise(LPFreq = mean(LPFreq),
            LPDur = mean(LPDur))  %>% 
  ungroup()

data_Period_long_DevalID_Avg_separateDay <- data_Period_long_DevalID %>% 
  group_by(Day, Devalued_Trial, Period, subject, Stimulus) %>% 
  summarise(LPFreq = mean(LPFreq),
            LPDur = mean(LPDur))  %>% 
  ungroup()



# Plots STage 3 - Deval ---------------------------------------------------


# Exclude subjects
plot_bin <- data_bin_long_DevalID_Avg %>% 
  filter(subject != "17____") %>% 
  drop_na()

plot_Period <- data_Period_long_DevalID_Avg %>% 
  filter(subject != "17____")%>% 
  drop_na()

# 
# Devaluation_PerBin_LP <- plot_bin %>% 
#   filter(Stimulus == "Magazine",
#   ) %>% 
#   ggplot(mapping = aes(x = as.factor(bin_timewithin), y = LPFreq, group = Stimulus, colour = Stimulus, fill = Stimulus, shape = Stimulus, linetype = Stimulus)) +
#   stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
#   stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
#   stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
#   facet_wrap(~ Devalued_Trial + Period, nrow = 2, scales = "free_x") +
#   # Make Pretty
#   scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,0.2)) +
#   ggtitle("Stage 3: Devaluation Test") + xlab("Bin 1 min") + ylab("Total LP") +
#   theme_cowplot(11) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(plot.title = element_text(size=10)) +
#   coord_cartesian(ylim = c(0,1.6001)) +
#   theme(axis.title.x=element_text(face = "bold")) +
#   scale_linetype_manual(name = "", values = linetypes)  +
#   scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
#   scale_shape_manual(name = "", values = pointshapes) +
#   scale_fill_manual(name = "", values = fillcolours) +
#   theme(legend.key.width=unit(1,"line"))
# 
# Devaluation_PerBin_LP
# 


Devaluation_Total_MagFreq <- plot_Period %>% 
  filter(Stimulus == "Magazine",
) %>% 
  ggplot(mapping = aes(x = as.factor(Devalued_Trial), y = LPDur, group = Devalued_Trial, colour = Devalued_Trial, fill = Devalued_Trial)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  geom_point(aes(group = subject), colour = gray(.5)) +
  geom_line(aes(group = subject), colour = gray(.3) ) +
  facet_wrap(~ Period , nrow = 1) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,.5)) +
  ggtitle("Stage 3: Devaluation Test") + xlab("Cue Period") + ylab("Total Magazine Entries (5 seconds)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,4.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(0.5,"line"))


Devaluation_Total_MagFreq

Devaluation_Total_LP <- plot_Period %>% 
  filter(Stimulus != "Magazine",
  ) %>% 
  ggplot(mapping = aes(x = as.factor(Stimulus), y = LPFreq, group = Stimulus, colour = Stimulus, fill = Stimulus)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  geom_point(aes(group = subject), colour = gray(.5)) +
  geom_line(aes(group = subject), colour = gray(.3) ) +
  facet_wrap(~ Devalued_Trial +Period , nrow = 2, scales = "free") +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,.5)) +
  ggtitle("Stage 3: Devaluation Test") + xlab("Lever Devaluation") + ylab("Total Lever Presses (5 seconds)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,3.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(0.5,"line"))


Devaluation_Total_LP


# Save Data for analysis ------------------------------------------


savefile <- "LPL_Stage3_DevalTest_WithinCue_PeriodAvg.csv"
write_csv(plot_Period, here("figures", "figure_data",savefile))






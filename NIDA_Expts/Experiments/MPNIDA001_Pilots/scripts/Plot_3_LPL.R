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
                 "Magazine" = LightGrey)

linecolours <- c("Flash" = DarkRed,
                 "Steady" = DarkBlue,
                 "Left" = DarkRed,
                 "Right" = DarkBlue,
                 "Banana" = DarkRed,
                 "Chocolate" = DarkBlue,
                 "Devalued" = Black,
                 "NonDevalued" = Black,
                 "Magazine" = MediumGrey)


linetypes <- c("Flash" = "dotted",
               "Steady" = "solid",
               "Left" = "dotted",
               "Right" = "solid",
               "Banana" = "dotted",
               "Chocolate" = "solid",
               "Devalued" = "dotted",
               "NonDevalued" = "solid",
               "Magazine" = "solid")



pointshapes <- c("Flash" = circle,
                 "Steady" = square,
                 "Left" = circle,
                 "Right" = square,
                 "Banana" = circle,
                 "Chocolate" = square,
                 "Devalued" = square,
                 "NonDevalued" = circle,
                 "Magazine" = triangleDown)




# Stage 1: Instrumental Acquisition ---------------------------------------


# Load Data ---------------------------------------------------------------

folderpath <- here("rawdata","Marios","3_LeverPressingForLights","CombinedData")
filename <- "LPL_ProcessedData_WithinSession10minBins.csv"

rawdata <- read_csv(here(folderpath,filename))

## Recode lever identity based on counterbalancing
data_recode <- rawdata %>% 
  group_by(Day, subject) %>% 
  mutate(LP_Freq_Flash  = ifelse(FLash_leverCbx == "Left", A1_freq, ifelse(FLash_leverCbx == "Right", A2_freq, NA)),
         LP_Dur_Flash  = ifelse(FLash_leverCbx == "Left", A1_dur, ifelse(FLash_leverCbx == "Right", A2_dur, NA)),
         LP_Freq_Steady  = ifelse(Steady_levercbx == "Left", A1_freq, ifelse(Steady_levercbx == "Right", A2_freq, NA)),
         LP_Dur_Steady  = ifelse(Steady_levercbx == "Left", A1_dur, ifelse(Steady_levercbx == "Right", A2_dur, NA)),
         DevaluedStimulus = ifelse(DevaluedOutcome1 == Flash_OutcomeID, "Flash", ifelse(DevaluedOutcome1 == Steady_OutcomeID, "Steady", NA)),
         NonDevaluedStimulus = ifelse(DevaluedOutcome1 != Flash_OutcomeID, "Flash", ifelse(DevaluedOutcome1 != Steady_OutcomeID, "Steady", NA)),
         DevaluedLever = ifelse(DevaluedStimulus == "Flash", FLash_leverCbx , ifelse(DevaluedStimulus == "Steady", Steady_levercbx, NA)),
         NonDevaluedLever = ifelse(NonDevaluedStimulus == "Flash", FLash_leverCbx , ifelse(NonDevaluedStimulus == "Steady", Steady_levercbx, NA)),
         LP_Freq_Devalued  = ifelse(DevaluedLever == "Left", A1_freq, ifelse(DevaluedLever == "Right", A2_freq, NA)),
         LP_Dur_Devalued  = ifelse(DevaluedLever == "Left", A1_dur, ifelse(DevaluedLever == "Right", A2_dur, NA)),
         LP_Freq_NonDevalued  = ifelse(NonDevaluedLever == "Left", A1_freq, ifelse(NonDevaluedLever == "Right", A2_freq, NA)),
         LP_Dur_NonDevalued  = ifelse(NonDevaluedLever == "Left", A1_dur, ifelse(NonDevaluedLever == "Right", A2_dur, NA)),
         Reinforcer_Devalued = ifelse(DevaluedStimulus == "Flash", Flash_freq , ifelse(DevaluedStimulus == "Steady", Steady_freq, NA)),
         Reinforcer_NonDevalued = ifelse(NonDevaluedStimulus == "Flash", Flash_freq , ifelse(NonDevaluedStimulus == "Steady", Steady_freq, NA))
         
  ) %>% 
  ungroup()


## relabel data
data_bin <- data_recode %>%
  group_by(Day, counterbalancing, Pavlovian_cbx, DevaluedOutcome1, DevaluedStimulus, DevaluedLever, timebins, subject) %>%
  summarise(LPFreq_Flash = sum(LP_Freq_Flash),
            LPDur_Flash = sum(LP_Dur_Flash),
            LPFreq_Steady = sum(LP_Freq_Steady),
            LPDur_Steady = sum(LP_Dur_Steady),
            Reinforcer_Flash = sum(Flash_freq),
            Reinforcer_Steady = sum(Steady_freq),
            LPFreq_Devalued = sum(LP_Freq_Devalued),
            LPDur_Devalued = sum(LP_Dur_Devalued),
            LPFreq_NonDevalued = sum(LP_Freq_NonDevalued),
            LPDur_NonDevalued = sum(LP_Dur_NonDevalued),
            Reinforcer_Devalued = sum(Reinforcer_Devalued),
            Reinforcer_NonDevalued = sum(Reinforcer_NonDevalued)
  ) %>%
  ungroup()

#Long format  
data_bin_long_StimID <- data_bin %>% 
  pivot_longer(c(LPFreq_Flash, LPDur_Flash, LPFreq_Steady, LPDur_Steady, Reinforcer_Flash, Reinforcer_Steady), names_to = c("Measure","Stimulus"), names_sep = "_", values_to = "LP") %>% 
  pivot_wider(names_from = Measure, values_from = LP)

data_bin_long_DevalID <- data_bin %>% 
  pivot_longer(c(LPFreq_Devalued, LPDur_Devalued, LPFreq_NonDevalued, LPDur_NonDevalued, Reinforcer_Devalued, Reinforcer_NonDevalued), names_to = c("Measure","Stimulus"), names_sep = "_", values_to = "LP") %>% 
  pivot_wider(names_from = Measure, values_from = LP)

# Sum responding over time bins 
data_Period_long_StimID <- data_bin_long_StimID %>% 
  group_by(Day, subject, counterbalancing, Pavlovian_cbx, DevaluedOutcome1, DevaluedStimulus, DevaluedLever, Stimulus) %>% 
  summarise(LPFreq = sum(LPFreq),
            LPDur = sum(LPDur),
            Reinforcer = sum(Reinforcer)
  ) %>% 
  ungroup()

data_Period_long_DevalID <- data_bin_long_DevalID %>% 
  group_by(Day, subject, counterbalancing, Pavlovian_cbx, DevaluedOutcome1, DevaluedStimulus, DevaluedLever, Stimulus) %>% 
  summarise(LPFreq = sum(LPFreq),
            LPDur = sum(LPDur),
            Reinforcer = sum(Reinforcer)
  ) %>% 
  ungroup()




# #Long format
# data_PerSession_long <- data_PerSession %>% 
#   pivot_longer(c(LPFreq_Flash,LPDur_Flash, LPFreq_Steady,LPDur_Steady, Reinforcer_Flash, Reinforcer_Steady), names_to = c("Measure","Stimulus"), names_sep = "_", values_to = "LP") %>% 
#   pivot_wider(names_from = Measure, values_from = LP)
# 
# # Save total Experienced reinforcers to csv from first stage and use for determining devaluation coutnerbalancing
# AAAcounterbalancingDeval <- data_PerSession_long %>%
#   group_by(counterbalancing, Pavlovian_cbx, Instrumental_cbx, subject, Stimulus) %>%
#   summarise(Reinforcer = sum(Reinforcer)) %>%
#   pivot_wider(names_from = Stimulus, values_from = Reinforcer)
# 
# savefolderpath <- here("rawdata","Marios","3_LeverPressingForLights","CombinedData")
# savefilename <- "LPL_CounterbalancingDeval.csv"
# dir.create(savefolderpath)
# write_csv(AAAcounterbalancingDeval,here(savefolderpath,savefilename))
# 

# Plot Instrumental Acquisition: Stim ID -------------------------------------------

Acquisition_PerSession_Reinforcer <- data_Period_long_StimID %>% 
  ggplot(mapping = aes(x = as.factor(Day), y = Reinforcer, group = Stimulus, colour = Stimulus, fill = Stimulus, shape = Stimulus, linetype = Stimulus)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,2)) +
  ggtitle("Stage 1: Instrumental") + xlab("Day") + ylab("Total Reinforcers (30 mins)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,14.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))



Acquisition_PerSession_LP <- data_Period_long_StimID %>% 
  ggplot(mapping = aes(x = as.factor(Day), y = LPFreq, group = Stimulus, colour = Stimulus, fill = Stimulus, shape = Stimulus, linetype = Stimulus)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,5)) +
  ggtitle("Stage 1: Instrumental") + xlab("Day") + ylab("Total LP (30 mins)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,60.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))





Acquisition_PerBin_Reinforcer <- data_bin_long_StimID %>% 
  ggplot(mapping = aes(x = as.factor(timebins), y = Reinforcer, group = Stimulus, colour = Stimulus, fill = Stimulus, shape = Stimulus, linetype = Stimulus)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  facet_wrap(~Day, nrow = 1) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,1)) +
  ggtitle("Stage 1: Instrumental") + xlab("Bin 10 mins") + ylab("Total Reinforcers") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,6.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))




Acquisition_PerBin_LP <- data_bin_long_StimID %>% 
  ggplot(mapping = aes(x = as.factor(timebins), y = LPFreq, group = Stimulus, colour = Stimulus, fill = Stimulus, shape = Stimulus, linetype = Stimulus)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  facet_wrap(~Day, nrow = 1) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,5)) +
  ggtitle("Stage 1: Instrumental") + xlab("Bin 10 mins") + ylab("Total LP") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,30.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))




# Plot Instrumental Acquisition: Deval1 ID --------------------------------

Acquisition_PerSession_Reinforcer_DevalID <- data_Period_long_DevalID %>% 
  ggplot(mapping = aes(x = as.factor(Day), y = Reinforcer, group = Stimulus, colour = Stimulus, fill = Stimulus, shape = Stimulus, linetype = Stimulus)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,2)) +
  ggtitle("Stage 1: Instrumental") + xlab("Day") + ylab("Total Reinforcers (30 mins)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,14.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))



Acquisition_PerSession_LP_DevalID <- data_Period_long_DevalID %>% 
  ggplot(mapping = aes(x = as.factor(Day), y = LPFreq, group = Stimulus, colour = Stimulus, fill = Stimulus, shape = Stimulus, linetype = Stimulus)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,5)) +
  ggtitle("Stage 1: Instrumental") + xlab("Day") + ylab("Total LP (30 mins)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,60.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))





Acquisition_PerBin_Reinforcer_DevalID <- data_bin_long_DevalID %>% 
  ggplot(mapping = aes(x = as.factor(timebins), y = Reinforcer, group = Stimulus, colour = Stimulus, fill = Stimulus, shape = Stimulus, linetype = Stimulus)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  facet_wrap(~Day, nrow = 1) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,1)) +
  ggtitle("Stage 1: Instrumental") + xlab("Bin 10 mins") + ylab("Total Reinforcers") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,6.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))




Acquisition_PerBin_LP_DevalID <- data_bin_long_DevalID %>% 
  ggplot(mapping = aes(x = as.factor(timebins), y = LPFreq, group = Stimulus, colour = Stimulus, fill = Stimulus, shape = Stimulus, linetype = Stimulus)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  facet_wrap(~Day, nrow = 1) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,5)) +
  ggtitle("Stage 1: Instrumental") + xlab("Bin 10 mins") + ylab("Total LP") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,30.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))





# Save Stage 1 Data for analysis ------------------------------------------


savefile <- "LPL_Stage1_sessionAvg_StimID.csv"
write_csv(data_Period_long_StimID, here("figures", "figure_data",savefile))


savefile <- "CI_Stage1_7_5minBins_StimID.csv"
write_csv(data_bin_long_StimID, here("figures", "figure_data",savefile))

savefile <- "LPL_Stage1_sessionAvg_DevalID.csv"
write_csv(data_Period_long_DevalID, here("figures", "figure_data",savefile))


savefile <- "CI_Stage1_7_5minBins_DevalID.csv"
write_csv(data_bin_long_DevalID, here("figures", "figure_data",savefile))



# Stage 2: Pavlovian Acquisition ------------------------------------------

# Load Data - Stage 2 ---------------------------------------------------------------

folderpath <- here("rawdata","Marios","3_LeverPressingForLights","CombinedData")
filename <- "LPL_Pavlovian_ProcessedData_pertrial_1sbins.csv"

rawdata <- read_csv(here(folderpath,filename))

# Fix Day factor to numeric
rawdata <- rawdata %>% 
  mutate(Day = as.numeric(str_remove(Day, "Day")))


data_PerSession <- rawdata %>% 
  group_by(Day, subject, outcome_ID, CS_name, DevaluationID, Period) %>% 
  summarise(MagEntries = mean(A3_freq)*5,
            MagDuration = mean(A3_dur)*5) %>%
  ungroup()

data_PerSession_CSPre <- data_PerSession %>% 
  pivot_wider(names_from = Period,values_from = c(MagEntries, MagDuration)) %>% 
  mutate(MagEntries_CSPre = MagEntries_CS - MagEntries_Pre2,
         MagDuration_CSPre = MagDuration_CS - MagDuration_Pre2) %>% 
  pivot_longer(c(MagEntries_CS, MagEntries_Post1, MagEntries_Post2, MagEntries_Pre1,  MagEntries_Pre2, MagDuration_CS, MagDuration_Post1, MagDuration_Post2, MagDuration_Pre1, MagDuration_Pre2, MagEntries_CSPre, MagDuration_CSPre), names_to = c("Measure", "Period"), names_sep = "_", values_to = "Mag") %>% 
  pivot_wider(names_from = Measure, values_from = Mag)


#  Plots Stage 2 ----------------------------------------------------------

## 5s Data
### Frequency

Acquisition_Stage2_MagFreq <- data_PerSession_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(Day-6), y = MagEntries, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Stage 2: Pavlovian") + xlab("Day") + ylab("Magazine Entry 5s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,2.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

LPL_Stage2_5s_Freq <- shift_xaxis(Acquisition_Stage2_MagFreq)




## 5s Data
### Duration

Acquisition_Stage2_MagDur <- data_PerSession_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(Day-6), y = MagDuration, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Stage 2: Pavlovian") + xlab("Day") + ylab("Magazine Duration 5s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,2.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

LPL_Stage2_5s_Dur <- shift_xaxis(Acquisition_Stage2_MagDur)



# Plots by Devaluation1 Identity

## 5s Data
### Frequency

Acquisition_Stage2_MagFreq_DevalID <- data_PerSession_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(Day-6), y = MagEntries, group = DevaluationID, colour = DevaluationID, fill = DevaluationID, shape = DevaluationID,linetype = DevaluationID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Stage 2: Pavlovian") + xlab("Day") + ylab("Magazine Entry 5s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,2.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

LPL_Stage2_5s_Freq_DevalID <- shift_xaxis(Acquisition_Stage2_MagFreq_DevalID)




## 5s Data
### Duration

Acquisition_Stage2_MagDur_DevalID <- data_PerSession_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(Day-6), y = MagDuration, group = DevaluationID, colour = DevaluationID, fill = DevaluationID, shape = DevaluationID,linetype = DevaluationID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Stage 2: Pavlovian") + xlab("Day") + ylab("Magazine Entry 5s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,2.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

LPL_Stage2_5s_Dur_DevalID <- shift_xaxis(Acquisition_Stage2_MagDur_DevalID)



# Plots by Outcome Identity

## 5s Data
### Frequency

Acquisition_Stage2_MagFreq_OutcomeID <- data_PerSession_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(Day-6), y = MagEntries, group = outcome_ID, colour = outcome_ID, fill = outcome_ID, shape = outcome_ID,linetype = outcome_ID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Stage 2: Pavlovian") + xlab("Day") + ylab("Magazine Entry 5s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,2.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

LPL_Stage2_5s_Freq_OutcomeID <- shift_xaxis(Acquisition_Stage2_MagFreq_OutcomeID)




## 5s Data
### Duration

Acquisition_Stage2_MagDur_OutcomeID <- data_PerSession_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(Day-6), y = MagDuration, group = outcome_ID, colour = outcome_ID, fill = outcome_ID, shape = outcome_ID,linetype = outcome_ID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Stage 2: Pavlovian") + xlab("Day") + ylab("Magazine Entry 5s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,2.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

LPL_Stage2_5s_Dur_OutcomeID <- shift_xaxis(Acquisition_Stage2_MagDur_OutcomeID)




# Save Stage 2 Data for analysis ------------------------------------------

savefile <- "LPL_Stage2_CSPre.csv"
write_csv(data_PerSession_CSPre, here("figures", "figure_data",savefile))


# Stage3 Devaluation Test -------------------------------------------------


# Load Data ---------------------------------------------------------------

folderpath <- here("rawdata","Marios","3_LeverPressingForLights","CombinedData")
filename <- "LPL_ProcessedData_DevlautionTest_WithinSession1minBins.csv"

rawdata <- read_csv(here(folderpath,filename))

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
         Reinforcer_Devalued = ifelse(DevaluedStimulus == "Flash", Flash_freq , ifelse(DevaluedStimulus == "Steady", Steady_freq, NA)),
         Reinforcer_NonDevalued = ifelse(NonDevaluedStimulus == "Flash", Flash_freq , ifelse(NonDevaluedStimulus == "Steady", Steady_freq, NA))
         
  ) %>% 
  ungroup()




## relabel data
data_bin <- data_recode %>%
  group_by(Day, counterbalancing, Pavlovian_cbx, DevaluedOutcome, DevaluedStimulus, DevaluedLever, Test_Period,timebins, subject) %>%
  summarise(LPFreq_Flash = sum(LP_Freq_Flash),
            LPDur_Flash = sum(LP_Dur_Flash),
            LPFreq_Steady = sum(LP_Freq_Steady),
            LPDur_Steady = sum(LP_Dur_Steady),
            Reinforcer_Flash = sum(Flash_freq),
            Reinforcer_Steady = sum(Steady_freq),
            LPFreq_Devalued = sum(LP_Freq_Devalued),
            LPDur_Devalued = sum(LP_Dur_Devalued),
            LPFreq_NonDevalued = sum(LP_Freq_NonDevalued),
            LPDur_NonDevalued = sum(LP_Dur_NonDevalued),
            Reinforcer_Devalued = sum(Reinforcer_Devalued),
            Reinforcer_NonDevalued = sum(Reinforcer_NonDevalued),
            LPFreq_Magazine = sum(A3_freq),
            LPDur_Magazine = sum(A3_dur)
  ) %>%
  ungroup()

#Long format  
data_bin_long_StimID <- data_bin %>% 
  pivot_longer(c(LPFreq_Flash, LPDur_Flash, LPFreq_Steady, LPDur_Steady, Reinforcer_Flash, Reinforcer_Steady, LPFreq_Magazine, LPDur_Magazine), names_to = c("Measure","Stimulus"), names_sep = "_", values_to = "LP") %>% 
  pivot_wider(names_from = Measure, values_from = LP)

data_bin_long_DevalID <- data_bin %>% 
  pivot_longer(c(LPFreq_Devalued, LPDur_Devalued, LPFreq_NonDevalued, LPDur_NonDevalued, Reinforcer_Devalued, Reinforcer_NonDevalued, LPFreq_Magazine, LPDur_Magazine), names_to = c("Measure","Stimulus"), names_sep = "_", values_to = "LP") %>% 
  pivot_wider(names_from = Measure, values_from = LP)

# Sum responding over time bins 
data_Period_long_DevalID <- data_bin_long_DevalID %>% 
  group_by(Day, counterbalancing, Pavlovian_cbx, DevaluedOutcome, DevaluedStimulus, DevaluedLever, Test_Period, Stimulus, subject) %>% 
  summarise(LPFreq = sum(LPFreq),
            LPDur = sum(LPDur),
            Reinforcer = sum(Reinforcer)
  ) %>% 
  ungroup()

# Average across both test sessions
data_bin_long_DevalID_Avg <- data_bin_long_DevalID %>% 
  group_by(Test_Period, timebins, subject, Stimulus) %>% 
  summarise(LPFreq = mean(LPFreq),
            LPDur = mean(LPDur),
            Reinforcer = mean(Reinforcer))  %>% 
  ungroup()

data_Period_long_DevalID_Avg <- data_Period_long_DevalID %>% 
  group_by(Test_Period, subject, Stimulus) %>% 
  summarise(LPFreq = mean(LPFreq),
            LPDur = mean(LPDur),
            Reinforcer = mean(Reinforcer))  %>% 
  ungroup()

data_Period_long_DevalID_Avg_separateDay <- data_Period_long_DevalID %>% 
  group_by(Day, Test_Period, subject, Stimulus) %>% 
  summarise(LPFreq = mean(LPFreq),
            LPDur = mean(LPDur),
            Reinforcer = mean(Reinforcer))  %>% 
  ungroup()


# Plots STage 3 - Deval ---------------------------------------------------


# Exclude subjects
plot_bin <- data_bin_long_DevalID_Avg %>% 
  filter(subject != "17____",subject != "21____",subject != "18____",subject != "20____")

plot_Period <- data_Period_long_DevalID_Avg %>% 
  filter(subject != "17____",subject != "21____",subject != "18____",subject != "20____")

# subject != "42____" & subject != "18____" & subject != "43____" & subject != "25____"

Devaluation_PerBin_LP <- plot_bin %>% 
  filter(Stimulus != "Magazine",
         Test_Period != "ITI",
        ) %>% 
  ggplot(mapping = aes(x = as.factor(timebins), y = LPFreq, group = Stimulus, colour = Stimulus, fill = Stimulus, shape = Stimulus, linetype = Stimulus)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  facet_wrap(~Test_Period, nrow = 1, scales = "free_x") +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,5)) +
  ggtitle("Stage 3: Devaluation Test") + xlab("Bin 1 min") + ylab("Total LP") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,10.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

Devaluation_PerBin_LP


Devaluation_PerBin_Reinforcers <- plot_bin %>% 
  filter(Stimulus != "Magazine",
         Test_Period != "ITI") %>% 
  ggplot(mapping = aes(x = as.factor(timebins), y = Reinforcer, group = Stimulus, colour = Stimulus, fill = Stimulus, shape = Stimulus, linetype = Stimulus)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  facet_wrap(~Test_Period, nrow = 1, scales = "free_x") +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,1)) +
  ggtitle("Stage 3: Devaluation Test") + xlab("Bin 1 min") + ylab("Total Reinforcers") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,4.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

Devaluation_PerBin_Reinforcers

## Total Responses

Devaluation_Total_LP <- plot_Period %>% 
  filter(Stimulus != "Magazine",
         Test_Period != "ITI") %>% 
ggplot(mapping = aes(x = as.factor(Test_Period), y = LPFreq, group = Stimulus, colour = Stimulus, fill = Stimulus)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # facet_wrap(~Test,) +
   # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,5)) +
  ggtitle("Stage 3: Devaluation Test") + xlab("Test Period") + ylab("Total LP (10 mins)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,30.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(0.5,"line"))


Devaluation_Total_LP

Devaluation_Total_Reinforcers <- plot_Period %>% 
  filter(Stimulus != "Magazine",
         Test_Period != "ITI") %>% 
  ggplot(mapping = aes(x = as.factor(Test_Period), y = Reinforcer, group = Stimulus, colour = Stimulus, fill = Stimulus)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # facet_wrap(~Day,) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,5)) +
  ggtitle("Stage 3: Devaluation Test") + xlab("Test Period") + ylab("Total Reinforcers (10 mins)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,15.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(0.5,"line"))


Devaluation_Total_Reinforcers


# TEst Split by test day/order --------------------------------------------

Devaluation_Total_LP_TestOrder <- data_Period_long_DevalID_Avg_separateDay %>% 
  filter(Stimulus != "Magazine",
         Test_Period != "ITI") %>% 
  ggplot(mapping = aes(x = as.factor(Day), y = LPFreq, group = Stimulus, colour = Stimulus, fill = Stimulus)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  facet_wrap(~Day, scales="free") +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,5)) +
  ggtitle("Stage 3: Devaluation Test") + xlab("Test Period") + ylab("Total LP (10 mins)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,30.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(0.5,"line"))


Devaluation_Total_LP_TestOrder


Devaluation_Total_Reinforcer_TestOrder <- data_Period_long_DevalID_Avg_separateDay %>% 
  filter(Stimulus != "Magazine",
         Test_Period != "ITI") %>% 
  ggplot(mapping = aes(x = as.factor(Day), y = Reinforcer, group = Stimulus, colour = Stimulus, fill = Stimulus)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  facet_wrap(~Day, scales="free") +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,5)) +
  ggtitle("Stage 3: Devaluation Test") + xlab("Test Period") + ylab("Total Reinforcers (10 mins)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,15.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(0.5,"line"))


Devaluation_Total_Reinforcer_TestOrder


# Specific reinstatement test load data -----------------------------------

folderpath <- here("rawdata","Marios","3_LeverPressingForLights","CombinedData")
filename <- "LPL_ProcessedData_ReinstatamentTest_WithinSession1minBins.csv"

rawdata <- read_csv(here(folderpath,filename))



# Keep working from here! -------------------------------------------------


# 
# #Long format  
# data_bin_long_StimID <- data_bin %>% 
#   pivot_longer(c(LPFreq_Flash, LPDur_Flash, LPFreq_Steady, LPDur_Steady, Reinforcer_Flash, Reinforcer_Steady, LPFreq_Magazine, LPDur_Magazine, FlashLever, SteadyLever, BananaLever, ChocolateLever), names_to = c("Measure","Stimulus"), names_sep = "_", values_to = "LP") %>% 
#   pivot_wider(names_from = Measure, values_from = LP)





























# Combined Figure Panels ---------------------------------------------------
# # Stage 1: Instrumental
# Acquisition_PerSession_Reinforcer
# Acquisition_PerSession_LP
# Acquisition_PerBin_Reinforcer
# Acquisition_PerBin_LP
# 
# Acquisition_PerSession_Reinforcer_DevalID
# Acquisition_PerSession_LP_DevalID
# Acquisition_PerBin_Reinforcer_DevalID
# Acquisition_PerBin_LP_DevalID
# 
# # Stage 2: Pavlovian
# LPL_Stage2_5s_Freq
# LPL_Stage2_5s_Dur
# LPL_Stage2_5s_Freq_DevalID
# LPL_Stage2_5s_Dur_DevalID
# LPL_Stage2_5s_Freq_OutcomeID
# LPL_Stage2_5s_Dur_OutcomeID
# 
# # Stage 3: Devaluation Test
# Devaluation_PerBin_LP
# Devaluation_PerBin_Reinforcers
# Devaluation_Total_LP
# Devaluation_Total_Reinforcers



## Flash vs Steady Stage 1 and 2
A1 <- Acquisition_PerBin_LP + theme(legend.position= c(0.80,.75), 
                                   legend.justification='left',
                                   legend.direction='vertical')

D1 <- Acquisition_PerBin_Reinforcer + theme(legend.position= c(0.80,.75), 
                                           legend.justification='left',
                                           legend.direction='vertical', 
)

B1 <- LPL_Stage2_5s_Freq + theme(legend.position= c(0.05,.85), 
                                legend.justification='left',
                                legend.direction='vertical', 
)

E1 <- LPL_Stage2_5s_Dur + theme(legend.position= c(0.05,.85), 
                               legend.justification='left',
                               legend.direction='vertical', 
)

C1 <- Devaluation_Total_LP  + theme(legend.position= c(0.05,.90), 
                                    legend.justification='left',
                                    legend.direction='vertical',
                                    )

F1 <- Devaluation_Total_Reinforcers  + theme(legend.position= c(0.05,.90), 
                                             legend.justification='left',
                                             legend.direction='vertical',
)


LPL_Combined_Acquisition <- (A1 + B1 + C1 + D1 + E1 + F1) + plot_annotation(tag_levels = 'A') + plot_layout(ncol = 3, nrow = 2, widths = c(1, .5, .4, 1, .5, .4))
LPL_Combined_Acquisition


filename = here("figures", "LPL_Combined_Acquisition_Devaluation.png")
ggsave(filename, LPL_Combined_Acquisition, width = 280, height = 150, units = "mm", dpi = 1200)



# Test order plot ---------------------------------------------------------

A <- Devaluation_Total_LP_TestOrder + theme(legend.position= c(0.05,.90), 
                                                                         legend.justification='left',
                                                                         legend.direction='vertical')

B <- Devaluation_Total_Reinforcer_TestOrder + theme(legend.position= c(0.05,.90), 
                                            legend.justification='left',
                                            legend.direction='vertical')

LPL_SeparateTestOrder_Devaluation <- (A + B) + plot_annotation(tag_levels = 'A') + plot_layout(ncol = 1, nrow = 2, widths = c(1,1))


filename = here("figures", "LPL_SeparateTestOrder_Devaluation.png")
ggsave(filename, LPL_SeparateTestOrder_Devaluation, width = 80, height = 150, units = "mm", dpi = 1200)


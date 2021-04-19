##### Load relevant packages ----
## Packages for data organisation and plotting
library(tidyverse)
# Package for relative file paths
library(here)
library(ggpubr)
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



fillcolours <- c("A+++/---" = DarkRed,
                 "B+/-"= LightRed,
                 "C+++" = DarkBlue,
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

linecolours <- c("A+++/---" = DarkRed,
                 "B+/-"= MediumRed,
                 "C+++" = DarkBlue,
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


linetypes <- c("A+++/---" = "dotted",
               "B+/-"= "dotted",
               "C+++" = "solid",
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


pointshapes <- c("A+++/---" = circle,
                 "B+/-"= circle,
                 "C+++" = square,
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

folderpath <- here("rawdata","CRF","CombinedData")
filename <- "CRF_ProcessedData_pertrial_1sbins.csv"

rawdata <- read_csv(here(folderpath,filename))

# Fix Day factor to numeric
rawdata <- rawdata %>% 
  mutate(Day = as.numeric(str_remove(Day, "Day")))

# Add factor separating probability and reward magnitude 
CS_name <- c("A+++/---",
             "B+/-",
             "C+++",
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
  
# rawdata <- rawdata %>% 
#   mutate( Stage = ifelse(Stage =="Acquisition", "1.Acquisition", ifelse(Stage =="ReAcquisition", "2.ReAcquisition", ifelse(Stage =="EnhancedAcquisition", "3.EnhancedAcquisition", NA))) )

# 

# Add trial Number to data

# Function to calculate trial number specific to a cue ID within each session

temp <- rawdata

temp <- temp %>% 
  group_by(Day, subject, CS_name) %>% 
  summarise(trialnums = length(unique(bin_trial)),
            bin_trial = list(unique(bin_trial)),
            trialnumber = list(c(1:trialnums))) %>% 
  ungroup() %>%
  select(Day, subject, CS_name,bin_trial, trialnumber) %>% 
  unnest(c(bin_trial, trialnumber))

rawdata <- left_join(rawdata, temp, by = c("Day","subject", "CS_name", "bin_trial" ) )

##

data_PerTrial <- rawdata %>% 
  group_by(Stage, Day, subject, probability, magnitude, CS_name, Period, trialnumber) %>% 
  summarise(MagEntries = mean(A3_freq)*10,
            MagDuration = mean(A3_dur)*10) %>%
  ungroup()

data_PerTrial_CSPre <- data_PerTrial %>% 
  pivot_wider(names_from = Period,values_from = c(MagEntries, MagDuration)) %>% 
  mutate(MagEntries_CSPre = MagEntries_CS - MagEntries_Pre,
         MagDuration_CSPre = MagDuration_CS - MagDuration_Pre) %>% 
  pivot_longer(c(MagEntries_CS, MagEntries_Post, MagEntries_Pre, MagDuration_CS, MagDuration_Post, MagDuration_Pre, MagEntries_CSPre, MagDuration_CSPre), names_to = c("Measure", "Period"), names_sep = "_", values_to = "Mag") %>% 
  pivot_wider(names_from = Measure, values_from = Mag)



data_PerSession <- data_PerTrial %>% 
  group_by(Stage, Day, subject, probability, magnitude, CS_name, Period) %>% 
  summarise(MagEntries = mean(MagEntries),
            MagDuration = mean(MagDuration)) %>%
  ungroup()



data_PerSession_CSPre <- data_PerSession %>% 
  pivot_wider(names_from = Period,values_from = c(MagEntries, MagDuration)) %>% 
  mutate(MagEntries_CSPre = MagEntries_CS - MagEntries_Pre,
         MagDuration_CSPre = MagDuration_CS - MagDuration_Pre) %>% 
  pivot_longer(c(MagEntries_CS, MagEntries_Post, MagEntries_Pre, MagDuration_CS, MagDuration_Post, MagDuration_Pre, MagEntries_CSPre, MagDuration_CSPre), names_to = c("Measure", "Period"), names_sep = "_", values_to = "Mag") %>% 
  pivot_wider(names_from = Measure, values_from = Mag)
  


# Acquisition Plots -------------------------------------------------------

Acqsuisition_Stage1_MagFreq_PerTrial <- data_PerTrial_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(trialnumber), y = MagEntries, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  # facet_wrap(~ sex) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  facet_grid(.~Stage,scales = "free_x", space = "free_x") +
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

Acqsuisition_Stage1_MagFreq_PerTrial


Acqsuisition_Stage1_MagDur_PerTrial <- data_PerTrial_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(trialnumber), y = MagDuration, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  # facet_wrap(~ sex) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  facet_grid(.~Stage,scales = "free_x", space = "free_x") +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Acquisition") + xlab("Day") + ylab("Magazine Duration 10s (CS-Pre)") +
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

Acqsuisition_Stage1_MagDur_PerTrial



Acqsuisition_Stage1_MagFreq <- data_PerSession_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(Day), y = MagEntries, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  # facet_wrap(~ sex) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # facet_grid(.~Stage,scales = "free_x", space = "free_x") +
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

CRF_Stage1_MagFreq <- shift_xaxis(Acqsuisition_Stage1_MagFreq)
CRF_Stage1_MagFreq

Acqsuisition_Stage1_MagDur <- data_PerSession_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(Day), y = MagDuration, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  # facet_wrap(~ sex) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # facet_grid(.~Stage,scales = "free_x", space = "free_x") +
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

CRF_Stage1_MagDur <- shift_xaxis(Acqsuisition_Stage1_MagDur)
CRF_Stage1_MagDur

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

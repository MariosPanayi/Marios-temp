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


fillcolours <- c("A_O1" = DarkRed,
                 "B_O2" = DarkBlue, 
                 "C_O1" = MediumRed,
                 "D_O2" = MediumBlue,
                 "AX_" = White,
                 "BY_" = White,
                 "CX_" = LightRed,
                 "DY_" = LightBlue,
                 "CY_" = MediumRed,
                 "DX_" = MediumBlue,
                 "CXDY" = White,
                 "CYDX" = Black,
                 "X_O1" = White,
                 "X_O2" = Black,
                 "Y_O1" = DarkGrey,
                 "Y_O2" = LightGrey,
                 "Congruent" = White,
                 "Incongruent" = Black)

linecolours <- c("A_O1" = DarkRed,
                 "B_O2" = DarkBlue, 
                 "C_O1" = MediumRed,
                 "D_O2" = MediumBlue,
                 "AX_" = LightRed,
                 "BY_" = LightBlue,
                 "CX_" = LightRed,
                 "DY_" = LightBlue,
                 "CY_" = LightRed,
                 "DX_" = LightBlue,
                 "CXDY" = Black,
                 "CYDX" = Black,
                 "X_O1" = DarkRed,
                 "X_O2" = DarkBlue,
                 "Y_O1" = DarkRed,
                 "Y_O2" = DarkBlue,
                 "Congruent" = Black,
                 "Incongruent" = Black)


linetypes <- c("A_O1" = "solid",
               "B_O2" = "solid", 
               "C_O1" = "solid",
               "D_O2" = "solid",
               "AX_" = "dotted",
               "BY_" = "dotted",
               "CX_" = "dotted",
               "DY_" = "dotted",
               "CY_" = "solid",
               "DX_" = "solid",
               "CXDY" = "dotted",
               "CYDX" = "solid",
               "X_O1" = "dotted",
               "X_O2" = "solid",
               "Y_O1" = "dotted",
               "Y_O2" = "solid",
               "Congruent" = "dotted",
               "Incongruent" = "solid")


pointshapes <- c("A_O1" = square,
                    "B_O2" = circle, 
                    "C_O1" = square,
                    "D_O2" = circle,
                    "AX_" = triangleUp,
                    "BY_" = triangleDown,
                 "CX_" = triangleUp,
                 "DY_" = triangleDown,
                 "CY_" = square,
                 "DX_" = circle,
                 "CXDY" = triangleUp,
                 "CYDX" = square,
                 "X_O1" = triangleDown,
                 "X_O2" = triangleUp,
                 "Y_O1" = triangleUp,
                 "Y_O2" = triangleDown,
                 "Congruent" = triangleDown,
                 "Incongruent" = triangleUp)


# Load Data - Stage 1 ---------------------------------------------------------------

folderpath <- here("rawdata","SpecificCI","CombinedData")
filename <- "CI_ProcessedData_pertrial_1sbins.csv"

rawdata <- read_csv(here(folderpath,filename))

# Fix Day factor to numeric
rawdata <- rawdata %>% 
  mutate(Day = as.numeric(str_remove(Day, "Day")))

# Add new factors to represent predicted outcome identity and Cue pairs

CS_name <- c("A_O1",
"B_O2",
"C_O1",
"D_O2")

Outcome <- c("O1",
                     "O2",
                     "O1",
                     "O2")
CSPair <- c("AB",
            "AB",
            "CD",
            "CD")

newfactorlookup <- data.frame(CS_name, Outcome, CSPair)
rawdata <- left_join(rawdata, newfactorlookup, by = "CS_name")

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

# Note that data are rate per minute since these are variable duration CSs
# Furthermore, we need to average per trial first before making a per-session average

data_WithinTrial <- rawdata %>% 
  group_by(Day, subject, Outcome, CSPair, CS_name, Period, bin_timewithin) %>% 
  summarise(MagEntries = mean(A3_freq),
            MagDuration = mean(A3_dur)) %>%
  ungroup()

data_PerTrial <- rawdata %>% 
  group_by(Day, subject, Outcome, CSPair, CS_name, trialnumber, Period) %>% 
  summarise(MagEntries = mean(A3_freq)*60,
            MagDuration = mean(A3_dur)*60) %>%
  ungroup()

data_PerTrial_CSPre <- data_PerTrial %>% 
  pivot_wider(names_from = Period,values_from = c(MagEntries, MagDuration)) %>% 
  mutate(MagEntries_CSPre = MagEntries_CS - MagEntries_Pre,
         MagDuration_CSPre = MagDuration_CS - MagDuration_Pre) %>% 
  pivot_longer(c(MagEntries_CS, MagEntries_Post, MagEntries_Pre, MagDuration_CS, MagDuration_Post, MagDuration_Pre, MagEntries_CSPre, MagDuration_CSPre), names_to = c("Measure", "Period"), names_sep = "_", values_to = "Mag") %>% 
  pivot_wider(names_from = Measure, values_from = Mag)



data_PerSession <- data_PerTrial %>% 
  group_by(Day, subject, Outcome, CSPair, CS_name, Period) %>% 
  summarise(MagEntries = mean(MagEntries),
            MagDuration = mean(MagDuration)) %>%
  ungroup()

data_PerSession_CSPre <- data_PerSession %>% 
  pivot_wider(names_from = Period,values_from = c(MagEntries, MagDuration)) %>% 
  mutate(MagEntries_CSPre = MagEntries_CS - MagEntries_Pre,
         MagDuration_CSPre = MagDuration_CS - MagDuration_Pre) %>% 
  pivot_longer(c(MagEntries_CS, MagEntries_Post, MagEntries_Pre, MagDuration_CS, MagDuration_Post, MagDuration_Pre, MagEntries_CSPre, MagDuration_CSPre), names_to = c("Measure", "Period"), names_sep = "_", values_to = "Mag") %>% 
  pivot_wider(names_from = Measure, values_from = Mag)





#  Plots Stage 1 ----------------------------------------------------------



Acqsuisition_Stage1_MagFreq_WithinTrial <- data_WithinTrial %>% 
  filter(Period == "CS" ) %>%
  ggplot(mapping = aes(x = as.factor(bin_timewithin), y = MagEntries, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # facet_wrap( ~ Period, scales = "free") +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Stage 1") + xlab("Day") + ylab("Magazine Entry/60s (CS-Pre)") +
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
Acqsuisition_Stage1_MagFreq_WithinTrial

Acqsuisition_Stage1_MagDur_WithinTrial <- data_WithinTrial %>% 
  filter(Period == "CS" ) %>%
  ggplot(mapping = aes(x = as.factor(bin_timewithin), y = MagDuration, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # facet_wrap( ~ Period, scales = "free") +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Stage 1") + xlab("Day") + ylab("Magazine Entry/60s (CS-Pre)") +
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
Acqsuisition_Stage1_MagDur_WithinTrial



Acqsuisition_Stage1_MagFreq_PerTrial <- data_PerTrial_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(trialnumber), y = MagEntries, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  facet_wrap( ~ Day, scales = "free") +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,5)) +
  ggtitle("Stage 1") + xlab("Day") + ylab("Magazine Entry/60s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-10,20.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

CI_Stage1_10s_Freq_PerTrial <- shift_xaxis_facet(Acqsuisition_Stage1_MagFreq_PerTrial)
CI_Stage1_10s_Freq_PerTrial

Acqsuisition_Stage1_MagDur_PerTrial <- data_PerTrial_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(trialnumber), y = MagDuration, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  facet_wrap( ~ Day, scales = "free") +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,5)) +
  ggtitle("Stage 1") + xlab("Day") + ylab("Magazine Duration/60s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-10,20.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

CI_Stage1_10s_Dur_PerTrial <- shift_xaxis_facet(Acqsuisition_Stage1_MagDur_PerTrial)
CI_Stage1_10s_Dur_PerTrial


## Session Average Data
### Frequency

Acqsuisition_Stage1_MagFreq <- data_PerSession_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(Day), y = MagEntries, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,5)) +
  ggtitle("Stage 1") + xlab("Day") + ylab("Magazine Entry/60s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-5,20.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

CI_Stage1_10s_Freq <- shift_xaxis(Acqsuisition_Stage1_MagFreq)
CI_Stage1_10s_Freq

## 10s Data
### Duration

Acqsuisition_Stage1_MagDur <- data_PerSession_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(Day), y = MagDuration, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,5)) +
  ggtitle("Stage 1") + xlab("Day") + ylab("Magazine Duration 10s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-5,20.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

CI_Stage1_10s_Dur <- shift_xaxis(Acqsuisition_Stage1_MagDur)
CI_Stage1_10s_Dur

# #Inspect individual animals
# 
# openme <- data_PerSession_CSPre %>%
#   filter(Period == "CSPre") %>%
#   select(-MagEntries, -Period) %>%
#   pivot_wider(names_from = subject, values_from = MagDuration)
# 
# data_PerSession_CSPre %>% 
#   filter(Period == "CS") %>% 
#   select(-MagDuration, -Period) %>%
#   pivot_wider(names_from = subject, values_from = MagEntries) %>% 
#   kable()
# 


# Save Stage 1 Figures ----------------------------------------------------

# CI_Stage1_10s_Freq
# CI_Stage1_10s_Dur
# CI_Stage1_5s_Freq
# CI_Stage1_5s_Dur

# 
# filename = here("figures", "CI_Stage1_10s_Freq.png")
# ggsave(filename, CI_Stage1_10s_Freq, width = 80, height = 80, units = "mm", dpi = 1200)
# filename = here("figures", "CI_Stage1_10s_Dur.png")
# ggsave(filename, CI_Stage1_10s_Dur, width = 80, height = 80, units = "mm", dpi = 1200)
# filename = here("figures", "CI_Stage1_5s_Freq.png")
# ggsave(filename, CI_Stage1_5s_Freq, width = 80, height = 80, units = "mm", dpi = 1200)
# filename = here("figures", "CI_Stage1_5s_Dur.png")
# ggsave(filename, CI_Stage1_5s_Dur, width = 80, height = 80, units = "mm", dpi = 1200)
# 


# Save Stage 1 Data for analysis ------------------------------------------


savefile <- "CI_Stage1_CSPre.csv"
write_csv(data_PerSession_CSPre, here("figures", "figure_data",savefile))


savefile <- "CI_Stage1_CSPre_last5s.csv"
write_csv(data_PerSession_last5s_CSPre, here("figures", "figure_data",savefile))


  
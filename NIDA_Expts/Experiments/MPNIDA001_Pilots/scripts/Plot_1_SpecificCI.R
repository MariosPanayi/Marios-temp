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

folderpath <- here("rawdata","Marios","1_SpecificCI","CombinedData")
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

data_PerSession <- rawdata %>% 
  group_by(Day, subject, Outcome, CSPair, CS_name, Period) %>% 
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
  group_by(Day, subject, Outcome, CSPair, CS_name, Period) %>% 
  summarise(MagEntries = mean(A3_freq)*5,
            MagDuration = mean(A3_dur)*5) %>%
  ungroup()


data_PerSession_last5s_CSPre <- data_PerSession_last5s %>% 
  pivot_wider(names_from = Period,values_from = c(MagEntries, MagDuration)) %>% 
  mutate(MagEntries_CSPre = MagEntries_CS - MagEntries_Pre,
         MagDuration_CSPre = MagDuration_CS - MagDuration_Pre) %>% 
  pivot_longer(c(MagEntries_CS, MagEntries_Post, MagEntries_Pre, MagDuration_CS, MagDuration_Post, MagDuration_Pre, MagEntries_CSPre, MagDuration_CSPre), names_to = c("Measure", "Period"), names_sep = "_", values_to = "Mag") %>% 
  pivot_wider(names_from = Measure, values_from = Mag)


#  Plots Stage 1 ----------------------------------------------------------

## 10s Data
### Frequency

Acqsuisition_Stage1_MagFreq <- data_PerSession_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(Day), y = MagEntries, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Stage 1") + xlab("Day") + ylab("Magazine Entry 10s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,8.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

CI_Stage1_10s_Freq <- shift_xaxis(Acqsuisition_Stage1_MagFreq)

## 10s Data
### Duration

Acqsuisition_Stage1_MagDur <- data_PerSession_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(Day), y = MagDuration, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Stage 1") + xlab("Day") + ylab("Magazine Duration 10s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,8.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

CI_Stage1_10s_Dur <- shift_xaxis(Acqsuisition_Stage1_MagDur)


## 5s Data
### Frequency

Acqsuisition_Stage1_MagFreq_5s <- data_PerSession_last5s_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(Day), y = MagEntries, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Stage 1") + xlab("Day") + ylab("Magazine Entry 5s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,8.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

CI_Stage1_5s_Freq <- shift_xaxis(Acqsuisition_Stage1_MagFreq_5s)

## 5s Data
### Duration

Acqsuisition_Stage1_MagDur_5s <- data_PerSession_last5s_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(Day), y = MagDuration, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Stage 1") + xlab("Day") + ylab("Magazine Duration 5s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,8.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

CI_Stage1_5s_Dur <- shift_xaxis(Acqsuisition_Stage1_MagDur_5s)

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



# Load Data - Stage 2 Feature Negative ---------------------------------------------------------------

folderpath <- here("rawdata","Marios","1_SpecificCI","CombinedData")
filename <- "CI_Stage2_ProcessedData_pertrial_1sbins.csv"

rawdata <- read_csv(here(folderpath,filename))


# Fix Day factor to numeric
rawdata <- rawdata %>% 
  mutate(Day = as.numeric(str_remove(Day, "Day")))

CS_name <- c("A_O1",
             "B_O2",
             "AX_",
             "BY_")

Outcome <- c("O1",
             "O2",
             "O1",
             "O2")

CSPair <- c("AB",
            "AB",
            "AXBY",
            "AXBY")

newfactorlookup <- data.frame(CS_name, Outcome, CSPair)
rawdata <- left_join(rawdata, newfactorlookup, by = "CS_name")

data_PerSession <- rawdata %>% 
  group_by(Day, subject, sex, Outcome, CSPair, CS_name, Period) %>% 
  summarise(MagEntries = mean(A3_freq)*10,
            MagDuration = mean(A3_dur)*10) %>%
  ungroup()

# WithinCS data
data_PerSession_within <- rawdata %>% 
  group_by(Day, subject, sex, Outcome, CSPair, CS_name, Period, bin_timewithin) %>% 
  summarise(MagEntries = mean(A3_freq)*1,
            MagDuration = mean(A3_dur)*1) %>%
  ungroup()
#

data_PerSession_CSPre <- data_PerSession %>% 
  pivot_wider(names_from = Period,values_from = c(MagEntries, MagDuration)) %>% 
  mutate(MagEntries_CSPre = MagEntries_CS - MagEntries_Pre,
         MagDuration_CSPre = MagDuration_CS - MagDuration_Pre) %>% 
  pivot_longer(c(MagEntries_CS, MagEntries_Post, MagEntries_Pre, MagDuration_CS, MagDuration_Post, MagDuration_Pre, MagEntries_CSPre, MagDuration_CSPre), names_to = c("Measure", "Period"), names_sep = "_", values_to = "Mag") %>% 
  pivot_wider(names_from = Measure, values_from = Mag)


data_PerSession_last5s <- rawdata %>% 
  filter(bin_timewithin > 5) %>% 
  group_by(Day, subject, sex, Outcome, CSPair, CS_name, Period) %>% 
  summarise(MagEntries = mean(A3_freq)*5,
            MagDuration = mean(A3_dur)*5) %>%
  ungroup()


data_PerSession_last5s_CSPre <- data_PerSession_last5s %>% 
  pivot_wider(names_from = Period,values_from = c(MagEntries, MagDuration)) %>% 
  mutate(MagEntries_CSPre = MagEntries_CS - MagEntries_Pre,
         MagDuration_CSPre = MagDuration_CS - MagDuration_Pre) %>% 
  pivot_longer(c(MagEntries_CS, MagEntries_Post, MagEntries_Pre, MagDuration_CS, MagDuration_Post, MagDuration_Pre, MagEntries_CSPre, MagDuration_CSPre), names_to = c("Measure", "Period"), names_sep = "_", values_to = "Mag") %>% 
  pivot_wider(names_from = Measure, values_from = Mag)

#  Plots Stage 2 ----------------------------------------------------------

FeatureNegative_Stage2_MagFreq_within <- data_PerSession_within %>% 
  filter(Period == "CS") %>%
  ggplot(mapping = aes(x = as.factor(bin_timewithin), y = MagEntries, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  facet_wrap(vars(Day), ncol = 8) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,.2)) +
  ggtitle("Stage 2") + xlab("Time (s)") + ylab("Magazine Entry 1s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,1.4001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))
FeatureNegative_Stage2_MagFreq_within

FeatureNegative_Stage2_MagDur_within <- data_PerSession_within %>% 
  filter(Period == "CS") %>%
  ggplot(mapping = aes(x = as.factor(bin_timewithin), y = MagDuration, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  facet_wrap(vars(Day), ncol = 8) +
   # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,.2)) +
  ggtitle("Stage 2") + xlab("Time (s)") + ylab("Magazine Entry 1s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,1.4001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))
FeatureNegative_Stage2_MagDur_within

## 10s Data
### Frequency

FeatureNegative_Stage2_MagFreq <- data_PerSession_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(Day), y = MagEntries, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Stage 2") + xlab("Day") + ylab("Magazine Entry 10s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,8.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

CI_Stage2_10s_Freq <- shift_xaxis(FeatureNegative_Stage2_MagFreq)

## 10s Data
### Duration

FeatureNegative_Stage2_MagDur <- data_PerSession_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(Day), y = MagDuration, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Stage 2") + xlab("Day") + ylab("Magazine Duration 10s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,8.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

CI_Stage2_10s_Dur <- shift_xaxis(FeatureNegative_Stage2_MagDur)

## 5s Data
### Frequency

FeatureNegative_Stage2_MagFreq_5s <- data_PerSession_last5s_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(Day), y = MagEntries, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Stage 2") + xlab("Day") + ylab("Magazine Entry 5s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,8.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

CI_Stage2_5s_Freq <- shift_xaxis(FeatureNegative_Stage2_MagFreq_5s)

## 5s Data
### Duration

FeatureNegative_Stage2_MagDur_5s <- data_PerSession_last5s_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(Day), y = MagDuration, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Stage 2") + xlab("Day") + ylab("Magazine Duration 5s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,8.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

CI_Stage2_5s_Dur <- shift_xaxis(FeatureNegative_Stage2_MagDur_5s)


# Stage2_PostCS period ----------------------------------------------------



## 10s Data
### Frequency

FeatureNegative_Stage2_MagFreq_Post <- data_PerSession_CSPre %>% 
  filter(Period == "Post") %>%
  ggplot(mapping = aes(x = as.factor(Day), y = MagEntries, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Stage 2") + xlab("Day") + ylab("Magazine Entry 10s (Post)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,8.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

CI_Stage2_10s_Freq_Post <- shift_xaxis(FeatureNegative_Stage2_MagFreq_Post)

## 10s Data
### Duration

FeatureNegative_Stage2_MagDur_Post <- data_PerSession_CSPre %>% 
  filter(Period == "Post") %>%
  ggplot(mapping = aes(x = as.factor(Day), y = MagDuration, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Stage 2") + xlab("Day") + ylab("Magazine Duration 10s (Post)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,8.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

CI_Stage2_10s_Dur_Post <- shift_xaxis(FeatureNegative_Stage2_MagDur_Post)

## 5s Data
### Frequency

FeatureNegative_Stage2_MagFreq_5s_Post <- data_PerSession_last5s_CSPre %>% 
  filter(Period == "Post") %>%
  ggplot(mapping = aes(x = as.factor(Day), y = MagEntries, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Stage 2") + xlab("Day") + ylab("Magazine Entry 5s (Post)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,8.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

CI_Stage2_5s_Freq_Post <- shift_xaxis(FeatureNegative_Stage2_MagFreq_5s_Post)

## 5s Data
### Duration

FeatureNegative_Stage2_MagDur_5s_Post <- data_PerSession_last5s_CSPre %>% 
  filter(Period == "Post") %>%
  ggplot(mapping = aes(x = as.factor(Day), y = MagDuration, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Stage 2") + xlab("Day") + ylab("Magazine Duration 5s (Post)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-1,8.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

CI_Stage2_5s_Dur_Post <- shift_xaxis(FeatureNegative_Stage2_MagDur_5s_Post)


# Save Stage 2 Figures ----------------------------------------------------
# 
# # CI_Stage2_10s_Freq
# # CI_Stage2_10s_Dur
# # CI_Stage2_5s_Freq
# # CI_Stage2_5s_Dur
# 
# 
# filename = here("figures", "CI_Stage2_10s_Freq.png")
# ggsave(filename, CI_Stage2_10s_Freq, width = 80, height = 80, units = "mm", dpi = 1200)
# filename = here("figures", "CI_Stage2_10s_Dur.png")
# ggsave(filename, CI_Stage2_10s_Dur, width = 80, height = 80, units = "mm", dpi = 1200)
# filename = here("figures", "CI_Stage2_5s_Freq.png")
# ggsave(filename, CI_Stage2_5s_Freq, width = 80, height = 80, units = "mm", dpi = 1200)
# filename = here("figures", "CI_Stage2_5s_Dur.png")
# ggsave(filename, CI_Stage2_5s_Dur, width = 80, height = 80, units = "mm", dpi = 1200)
# 
# 
# # CI_Stage2_10s_Freq_Post
# # CI_Stage2_10s_Dur_Post
# # CI_Stage2_5s_Freq_Post
# # CI_Stage2_5s_Dur_Post
# 
# 
# 
# filename = here("figures", "CI_Stage2_10s_Freq_Post.png")
# ggsave(filename, CI_Stage2_10s_Freq_Post, width = 80, height = 80, units = "mm", dpi = 1200)
# filename = here("figures", "CI_Stage2_10s_Dur_Post.png")
# ggsave(filename, CI_Stage2_10s_Dur_Post, width = 80, height = 80, units = "mm", dpi = 1200)
# filename = here("figures", "CI_Stage2_5s_Freq_Post.png")
# ggsave(filename, CI_Stage2_5s_Freq_Post, width = 80, height = 80, units = "mm", dpi = 1200)
# filename = here("figures", "CI_Stage2_5s_Dur_Post.png")
# ggsave(filename, CI_Stage2_5s_Dur_Post, width = 80, height = 80, units = "mm", dpi = 1200)

# Save Stage 2 Data for analysis ------------------------------------------


savefile <- "CI_Stage2_CSPre.csv"
write_csv(data_PerSession_CSPre, here("figures", "figure_data", savefile))


savefile <- "CI_Stage2_CSPre_last5s.csv"
write_csv(data_PerSession_last5s_CSPre, here("figures", "figure_data", savefile))


# Stage 3 -----------------------------------------------------------------


  # Load Data - Stage 3 ---------------------------------------------------------------
  
  folderpath <- here("rawdata","Marios","1_SpecificCI","CombinedData")
  filename <- "CI_Stage3_ProcessedData_pertrial_1sbins.csv"
  
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
  
  data_PerSession <- rawdata %>% 
    group_by(Day, subject, Outcome, CSPair, CS_name, Period) %>% 
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
    group_by(Day, subject, Outcome, CSPair, CS_name, Period) %>% 
    summarise(MagEntries = mean(A3_freq)*5,
              MagDuration = mean(A3_dur)*5) %>%
    ungroup()
  
  
  data_PerSession_last5s_CSPre <- data_PerSession_last5s %>% 
    pivot_wider(names_from = Period,values_from = c(MagEntries, MagDuration)) %>% 
    mutate(MagEntries_CSPre = MagEntries_CS - MagEntries_Pre,
           MagDuration_CSPre = MagDuration_CS - MagDuration_Pre) %>% 
    pivot_longer(c(MagEntries_CS, MagEntries_Post, MagEntries_Pre, MagDuration_CS, MagDuration_Post, MagDuration_Pre, MagEntries_CSPre, MagDuration_CSPre), names_to = c("Measure", "Period"), names_sep = "_", values_to = "Mag") %>% 
    pivot_wider(names_from = Measure, values_from = Mag)
  
  
  #  Plots Stage 3 ----------------------------------------------------------
  
  ## 10s Data
  ### Frequency
  
  Acqsuisition_Stage3_MagFreq <- data_PerSession_CSPre %>% 
    filter(Period == "CSPre") %>%
    ggplot(mapping = aes(x = as.factor(Day), y = MagEntries, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
    stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
    ggtitle("Stage 3") + xlab("Day") + ylab("Magazine Entry 10s (CS-Pre)") +
    theme_cowplot(11) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(ylim = c(-1,8.0001)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_linetype_manual(name = "", values = linetypes)  +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_shape_manual(name = "", values = pointshapes) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(1,"line"))
  
  CI_Stage3_10s_Freq <- shift_xaxis(Acqsuisition_Stage3_MagFreq)
  
  ## 10s Data
  ### Duration
  
  Acqsuisition_Stage3_MagDur <- data_PerSession_CSPre %>% 
    filter(Period == "CSPre") %>%
    ggplot(mapping = aes(x = as.factor(Day), y = MagDuration, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
    stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
    ggtitle("Stage 3") + xlab("Day") + ylab("Magazine Duration 10s (CS-Pre)") +
    theme_cowplot(11) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(ylim = c(-1,8.0001)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_linetype_manual(name = "", values = linetypes)  +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_shape_manual(name = "", values = pointshapes) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(1,"line"))
  
  CI_Stage3_10s_Dur <- shift_xaxis(Acqsuisition_Stage3_MagDur)
  
  
  ## 5s Data
  ### Frequency
  
  Acqsuisition_Stage3_MagFreq_5s <- data_PerSession_last5s_CSPre %>% 
    filter(Period == "CSPre") %>%
    ggplot(mapping = aes(x = as.factor(Day), y = MagEntries, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
    stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
    ggtitle("Stage 3") + xlab("Day") + ylab("Magazine Entry 5s (CS-Pre)") +
    theme_cowplot(11) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(ylim = c(-1,8.0001)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_linetype_manual(name = "", values = linetypes)  +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_shape_manual(name = "", values = pointshapes) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(1,"line"))
  
  CI_Stage3_5s_Freq <- shift_xaxis(Acqsuisition_Stage3_MagFreq_5s)
  
  ## 5s Data
  ### Duration
  
  Acqsuisition_Stage3_MagDur_5s <- data_PerSession_last5s_CSPre %>% 
    filter(Period == "CSPre") %>%
    ggplot(mapping = aes(x = as.factor(Day), y = MagDuration, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
    stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
    ggtitle("Stage 3") + xlab("Day") + ylab("Magazine Duration 5s (CS-Pre)") +
    theme_cowplot(11) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(ylim = c(-1,8.0001)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_linetype_manual(name = "", values = linetypes)  +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_shape_manual(name = "", values = pointshapes) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(1,"line"))
  
  CI_Stage3_5s_Dur <- shift_xaxis(Acqsuisition_Stage3_MagDur_5s)
  
  
  
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
  #   select(-MagDuration, -Period) %>%
  #   pivot_wider(names_from = subject, values_from = MagEntries) %>% 
  #   kable()
  # 
  
  
  # Save Stage 3 Figures ----------------------------------------------------
  # 
  # # CI_Stage3_10s_Freq
  # # CI_Stage3_10s_Dur
  # # CI_Stage3_5s_Freq
  # # CI_Stage3_5s_Dur
  # 
  # 
  # filename = here("figures", "CI_Stage3_10s_Freq.png")
  # ggsave(filename, CI_Stage3_10s_Freq, width = 80, height = 80, units = "mm", dpi = 1200)
  # filename = here("figures", "CI_Stage3_10s_Dur.png")
  # ggsave(filename, CI_Stage3_10s_Dur, width = 80, height = 80, units = "mm", dpi = 1200)
  # filename = here("figures", "CI_Stage3_5s_Freq.png")
  # ggsave(filename, CI_Stage3_5s_Freq, width = 80, height = 80, units = "mm", dpi = 1200)
  # filename = here("figures", "CI_Stage3_5s_Dur.png")
  # ggsave(filename, CI_Stage3_5s_Dur, width = 80, height = 80, units = "mm", dpi = 1200)
  # 
  # 
  
  # Save Stage 3 Data for analysis ------------------------------------------
  
  
  savefile <- "CI_Stage3_CSPre.csv"
  write_csv(data_PerSession_CSPre, here("figures", "figure_data",savefile))
  
  
  savefile <- "CI_Stage3_CSPre_last5s.csv"
  write_csv(data_PerSession_last5s_CSPre, here("figures", "figure_data",savefile))
  

# Stage 4: Summation Test -------------------------------------------------

  # Load Data - Stage 4 ---------------------------------------------------------------
  
  folderpath <- here("rawdata","Marios","1_SpecificCI","CombinedData")
  filename <- "CI_Stage4_ProcessedData_pertrial_1sbins.csv"
  
  rawdata <- read_csv(here(folderpath,filename))
  
  # Fix Day factor to numeric
  rawdata <- rawdata %>% 
    mutate(Day = as.numeric(str_remove(Day, "Day")))
  
  # Add Trial Numbers for probes
  bin_trial <- c("1",
                 "2",
                 "3",
                 "4",
                 "5",
                 "6",
                 "7",
                 "8",
                 "9",
                 "10",
                 "11",
                 "12",
                 "13",
                 "14",
                 "15",
                 "16",
                 "17",
                 "18",
                 "19",
                 "20",
                 "21",
                 "22",
                 "23",
                 "24",
                 "25",
                 "26",
                 "27",
                 "28",
                 "29",
                 "30",
                 "31",
                 "32")
  
  
  TrialNumber_Probes <- c(NA,
                          NA,
                          1,
                          1,
                          1,
                          1,
                          1,
                          1,
                          NA,
                          NA,
                          2,
                          2,
                          2,
                          2,
                          2,
                          2,
                          NA,
                          NA,
                          3,
                          3,
                          3,
                          3,
                          3,
                          3,
                          NA,
                          NA,
                          4,
                          4,
                          4,
                          4,
                          4,
                          4)
  
  TrialNumber_CS <- c(1,
                      1,
                      NA,
                      NA,
                      NA,
                      NA,
                      NA,
                      NA,
                      2,
                      2,
                      NA,
                      NA,
                      NA,
                      NA,
                      NA,
                      NA,
                      3,
                      3,
                      NA,
                      NA,
                      NA,
                      NA,
                      NA,
                      NA,
                      4,
                      4,
                      NA,
                      NA,
                      NA,
                      NA,
                      NA,
                      NA)

  
  # Create counterbalancing lookup table
  lookup_TrialNum <- data.frame(bin_trial, TrialNumber_Probes, TrialNumber_CS)
  
  rawdata <- rawdata %>% 
    mutate(bin_trial = as.character(bin_trial))
  # Combine with rawdata
  rawdata <- left_join(rawdata, lookup_TrialNum, by = c("bin_trial"))
  
  
  
  # Add new factors to represent predicted outcome identity and Cue pairs
  
  CS_name <- c("A_O1",
               "B_O2",
               "C_O1",
               "D_O2",
               "AX_",
               "BY_",
               "CX_",
               "DX_",
               "CY_",
               "DY_")
  
  Outcome <- c("O1",
               "O2",
               "O1",
               "O2",
               "O1",
               "O2",
               "O1",
               "O2",
               "O1",
               "O2")
  
  CSPair <- c("AB",
              "AB",
              "CD",
              "CD",
              "AXBY",
              "AXBY",
              "CXDY",
              "CYDX",
              "CYDX",
              "CXDY")
  
  newfactorlookup <- data.frame(CS_name, Outcome, CSPair)
  rawdata <- left_join(rawdata, newfactorlookup, by = "CS_name")
  
  
  
  data_pertrial <- rawdata %>% 
    group_by(Day, subject, Outcome, CSPair, CS_name, Period, bin_trial, TrialNumber_CS, TrialNumber_Probes) %>% 
    summarise(MagEntries = mean(A3_freq)*10,
              MagDuration = mean(A3_dur)*10) %>%
    ungroup()

  
  data_pertrial_CSPre <- data_pertrial %>% 
    pivot_wider(names_from = Period,values_from = c(MagEntries, MagDuration)) %>% 
    mutate(MagEntries_CSPre = MagEntries_CS - MagEntries_Pre,
           MagDuration_CSPre = MagDuration_CS - MagDuration_Pre) %>% 
    pivot_longer(c(MagEntries_CS, MagEntries_Post, MagEntries_Pre, MagDuration_CS, MagDuration_Post, MagDuration_Pre, MagEntries_CSPre, MagDuration_CSPre), names_to = c("Measure", "Period"), names_sep = "_", values_to = "Mag") %>% 
    pivot_wider(names_from = Measure, values_from = Mag)
  
  
  data_pertrial_last5s <- rawdata %>% 
    filter(bin_timewithin > 5) %>% 
    group_by(Day, subject, Outcome, CSPair, CS_name, Period, bin_trial, TrialNumber_CS, TrialNumber_Probes) %>% 
    summarise(MagEntries = mean(A3_freq)*10,
              MagDuration = mean(A3_dur)*10) %>%
    ungroup()
  
  
  data_pertrial_last5s_CSPre <- data_pertrial_last5s %>% 
    pivot_wider(names_from = Period,values_from = c(MagEntries, MagDuration)) %>% 
    mutate(MagEntries_CSPre = MagEntries_CS - MagEntries_Pre,
           MagDuration_CSPre = MagDuration_CS - MagDuration_Pre) %>% 
    pivot_longer(c(MagEntries_CS, MagEntries_Post, MagEntries_Pre, MagDuration_CS, MagDuration_Post, MagDuration_Pre, MagEntries_CSPre, MagDuration_CSPre), names_to = c("Measure", "Period"), names_sep = "_", values_to = "Mag") %>% 
    pivot_wider(names_from = Measure, values_from = Mag)
  
  
  data_pertrial_first5s <- rawdata %>% 
    filter(bin_timewithin < 6) %>% 
    group_by(Day, subject, Outcome, CSPair, CS_name, Period, bin_trial, TrialNumber_CS, TrialNumber_Probes) %>% 
    summarise(MagEntries = mean(A3_freq)*10,
              MagDuration = mean(A3_dur)*10) %>%
    ungroup()
  
  
  data_pertrial_first5s_CSPre <- data_pertrial_first5s %>% 
    pivot_wider(names_from = Period,values_from = c(MagEntries, MagDuration)) %>% 
    mutate(MagEntries_CSPre = MagEntries_CS - MagEntries_Pre,
           MagDuration_CSPre = MagDuration_CS - MagDuration_Pre) %>% 
    pivot_longer(c(MagEntries_CS, MagEntries_Post, MagEntries_Pre, MagDuration_CS, MagDuration_Post, MagDuration_Pre, MagEntries_CSPre, MagDuration_CSPre), names_to = c("Measure", "Period"), names_sep = "_", values_to = "Mag") %>% 
    pivot_wider(names_from = Measure, values_from = Mag)
  
  
  data_PerSession <- rawdata %>% 
    group_by(Day, subject, Outcome, CSPair, CS_name, Period) %>% 
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
    group_by(Day, subject, Outcome, CSPair, CS_name, Period) %>% 
    summarise(MagEntries = mean(A3_freq)*5,
              MagDuration = mean(A3_dur)*5) %>%
    ungroup()
  
  
  data_PerSession_last5s_CSPre <- data_PerSession_last5s %>% 
    pivot_wider(names_from = Period,values_from = c(MagEntries, MagDuration)) %>% 
    mutate(MagEntries_CSPre = MagEntries_CS - MagEntries_Pre,
           MagDuration_CSPre = MagDuration_CS - MagDuration_Pre) %>% 
    pivot_longer(c(MagEntries_CS, MagEntries_Post, MagEntries_Pre, MagDuration_CS, MagDuration_Post, MagDuration_Pre, MagEntries_CSPre, MagDuration_CSPre), names_to = c("Measure", "Period"), names_sep = "_", values_to = "Mag") %>% 
    pivot_wider(names_from = Measure, values_from = Mag)
  

  #  Plots Stage 4 ----------------------------------------------------------
  
  ## PLot of C and D Reinforced trials
  
  ## 10s
  ### Frequency
  Acqsuisition_Stage4_MagFreq_CD <- data_PerSession_CSPre %>% 
    filter(Period == "CSPre",
           CS_name == "C_O1"  | CS_name == "D_O2" ) %>%
    ggplot(mapping = aes(x = as.factor(Day), y = MagEntries, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
    # stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
    ggtitle("Stage 4") + xlab("Day") + ylab("Magazine Entry 10s (CS-Pre)") +
    theme_cowplot(11) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(ylim = c(-1,8.0001)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_linetype_manual(name = "", values = linetypes)  +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_shape_manual(name = "", values = pointshapes) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(1,"line"))
  
  CI_Stage4_10s_Freq <- shift_xaxis(Acqsuisition_Stage4_MagFreq_CD)
  
  
  
  ## 10s
  ### Duration 
  Acqsuisition_Stage4_MagDur_CD <- data_PerSession_CSPre %>% 
    filter(Period == "CSPre",
           CS_name == "C_O1"  | CS_name == "D_O2" ) %>%
    ggplot(mapping = aes(x = as.factor(Day), y = MagDuration, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
    # stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
    ggtitle("Stage 4") + xlab("Day") + ylab("Magazine Duration 10s (CS-Pre)") +
    theme_cowplot(11) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(ylim = c(-1,8.0001)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_linetype_manual(name = "", values = linetypes)  +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_shape_manual(name = "", values = pointshapes) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(1,"line"))
  
  
  CI_Stage4_10s_Dur <- shift_xaxis(Acqsuisition_Stage4_MagDur_CD)
  
  
  
  
  ## 5s
  ### Frequency
  Acqsuisition_Stage4_MagFreq_CD_5s <- data_PerSession_last5s_CSPre %>% 
    filter(Period == "CSPre",
           CS_name == "C_O1"  | CS_name == "D_O2" ) %>%
    ggplot(mapping = aes(x = as.factor(Day), y = MagEntries, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
    # stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
    ggtitle("Stage 4") + xlab("Day") + ylab("Magazine Entry 5s (CS-Pre)") +
    theme_cowplot(11) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(ylim = c(-1,8.0001)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_linetype_manual(name = "", values = linetypes)  +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_shape_manual(name = "", values = pointshapes) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(1,"line"))
  
  
  CI_Stage4_5s_Freq <- shift_xaxis(Acqsuisition_Stage4_MagFreq_CD_5s)

  
  
  ## 10s
  ### Duration 
  Acqsuisition_Stage4_MagDur_CD_5s <- data_PerSession_last5s_CSPre %>% 
    filter(Period == "CSPre",
           CS_name == "C_O1"  | CS_name == "D_O2" ) %>%
    ggplot(mapping = aes(x = as.factor(Day), y = MagDuration, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
    # stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
    ggtitle("Stage 4") + xlab("Day") + ylab("Magazine Duration 5s (CS-Pre)") +
    theme_cowplot(11) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(ylim = c(-1,8.0001)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_linetype_manual(name = "", values = linetypes)  +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_shape_manual(name = "", values = pointshapes) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(1,"line"))
  
  
  CI_Stage4_5s_Dur <- shift_xaxis(Acqsuisition_Stage4_MagDur_CD_5s)
  

  # Summation Probe Trials - Per Trial  -----------------------------------------------------------------------
  ## 10s Data
  ### Frequency
  ### Per Trial 
  Acqsuisition_Stage4_MagFreq_PerTrial <- data_pertrial_CSPre %>% 
    filter(Period == "CSPre",
           CS_name != "C_O1"  & CS_name != "D_O2" ) %>%
    ggplot(mapping = aes(x = as.factor(TrialNumber_Probes), y = MagEntries, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
    stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
    ggtitle("Stage 4") + xlab("Trial") + ylab("Magazine Entry 10s (CS-Pre)") +
    theme_cowplot(11) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(ylim = c(-2,2.0001)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_linetype_manual(name = "", values = linetypes)  +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_shape_manual(name = "", values = pointshapes) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(1,"line"))


  CI_Stage4_10s_Freq_PerTrial <- shift_xaxis(Acqsuisition_Stage4_MagFreq_PerTrial)

  
  ## 10s Data
  ### Duration
  ### Per Trial 
  Acqsuisition_Stage4_MagDur_PerTrial <- data_pertrial_CSPre %>% 
    filter(Period == "CSPre",
           CS_name != "C_O1"  & CS_name != "D_O2" ) %>%
    ggplot(mapping = aes(x = as.factor(TrialNumber_Probes), y = MagDuration, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
    stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
    ggtitle("Stage 4") + xlab("Trial") + ylab("Magazine Duration 10s (CS-Pre)") +
    theme_cowplot(11) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(ylim = c(-2,2.0001)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_linetype_manual(name = "", values = linetypes)  +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_shape_manual(name = "", values = pointshapes) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(1,"line"))
  
  CI_Stage4_10s_Dur_PerTrial <- shift_xaxis(Acqsuisition_Stage4_MagDur_PerTrial)

  
  ### 
  # Summation Probe Trials - Combined congruent/incongruent
  ## 10s Data
  ### Frequency
  ### Per Trial 
  
  Acqsuisition_Stage4_MagFreq_PerTrial_combined <- data_pertrial_CSPre %>% 
    filter(Period == "CSPre",
           CS_name != "C_O1"  & CS_name != "D_O2" ) %>% 
  group_by(Day, subject, CSPair, Period, TrialNumber_Probes) %>% 
    summarise(MagEntries = mean(MagEntries),
              MagDuration = mean(MagDuration)) %>%
    ungroup() %>%
    ggplot(mapping = aes(x = as.factor(TrialNumber_Probes), y = MagEntries, group = CSPair, colour = CSPair, fill = CSPair, shape = CSPair,linetype = CSPair)) +
    stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
    ggtitle("Stage 4") + xlab("Trial") + ylab("Magazine Entry 10s (CS-Pre)") +
    theme_cowplot(11) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(ylim = c(-2,2.0001)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_linetype_manual(name = "", values = linetypes)  +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_shape_manual(name = "", values = pointshapes) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(1,"line"))
  
  CI_Stage4_10s_Freq_PerTrial_Combined <- shift_xaxis(Acqsuisition_Stage4_MagFreq_PerTrial_combined)
  
  
  ## 10s Data
  ### Duration
  ### Per Trial 
  
  Acqsuisition_Stage4_MagDur_PerTrial_combined <- data_pertrial_CSPre %>% 
    filter(Period == "CSPre",
           CS_name != "C_O1"  & CS_name != "D_O2" ) %>% 
    group_by(Day, subject, CSPair, Period, TrialNumber_Probes) %>% 
    summarise(MagEntries = mean(MagEntries),
              MagDuration = mean(MagDuration)) %>%
    ungroup() %>%
    ggplot(mapping = aes(x = as.factor(TrialNumber_Probes), y = MagDuration, group = CSPair, colour = CSPair, fill = CSPair, shape = CSPair,linetype = CSPair)) +
    stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
    ggtitle("Stage 4") + xlab("Trial") + ylab("Magazine Duration 10s (CS-Pre)") +
    theme_cowplot(11) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(ylim = c(-2,2.0001)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_linetype_manual(name = "", values = linetypes)  +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_shape_manual(name = "", values = pointshapes) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(1,"line"))
  
  CI_Stage4_10s_Dur_PerTrial_Combined <- shift_xaxis(Acqsuisition_Stage4_MagDur_PerTrial_combined)
  
  
  ## FIRST 5s Data
  ### Frequency
  ### Per Trial 
  
  Acqsuisition_Stage4_MagFreq_PerTrial_combined_1st5s <- data_pertrial_first5s_CSPre %>% 
    filter(Period == "CSPre",
           CS_name != "C_O1"  & CS_name != "D_O2" ) %>% 
    group_by(Day, subject, CSPair, Period, TrialNumber_Probes) %>% 
    summarise(MagEntries = mean(MagEntries),
              MagDuration = mean(MagDuration)) %>%
    ungroup() %>%
    ggplot(mapping = aes(x = as.factor(TrialNumber_Probes), y = MagEntries, group = CSPair, colour = CSPair, fill = CSPair, shape = CSPair,linetype = CSPair)) +
    stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
    ggtitle("Stage 4") + xlab("Trial") + ylab("Magazine Entry first 5s (CS-Pre)") +
    theme_cowplot(11) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(ylim = c(-2,2.0001)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_linetype_manual(name = "", values = linetypes)  +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_shape_manual(name = "", values = pointshapes) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(1,"line"))
  
  CI_Stage4_1st5s_Freq_PerTrial_Combined <- shift_xaxis(Acqsuisition_Stage4_MagFreq_PerTrial_combined_1st5s)
  
  
  ## FIRST 5s Data
  ### Duration
  ### Per Trial 
  
  Acqsuisition_Stage4_MagDur_PerTrial_combined_1st5s <- data_pertrial_first5s_CSPre %>% 
    filter(Period == "CSPre",
           CS_name != "C_O1"  & CS_name != "D_O2" ) %>% 
    group_by(Day, subject, CSPair, Period, TrialNumber_Probes) %>% 
    summarise(MagEntries = mean(MagEntries),
              MagDuration = mean(MagDuration)) %>%
    ungroup() %>%
    ggplot(mapping = aes(x = as.factor(TrialNumber_Probes), y = MagDuration, group = CSPair, colour = CSPair, fill = CSPair, shape = CSPair,linetype = CSPair)) +
    stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
    ggtitle("Stage 4") + xlab("Trial") + ylab("Magazine Duration first 5s (CS-Pre)") +
    theme_cowplot(11) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(ylim = c(-2,2.0001)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_linetype_manual(name = "", values = linetypes)  +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_shape_manual(name = "", values = pointshapes) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(1,"line"))
  
  CI_Stage4_1st5s_Dur_PerTrial_Combined <- shift_xaxis(Acqsuisition_Stage4_MagDur_PerTrial_combined_1st5s)
  
  
  
  ## LAST 5s Data
  ### Frequency
  ### Per Trial 
  
  Acqsuisition_Stage4_MagFreq_PerTrial_combined_2nd5s <- data_pertrial_last5s_CSPre %>% 
    filter(Period == "CSPre",
           CS_name != "C_O1"  & CS_name != "D_O2" ) %>% 
    group_by(Day, subject, CSPair, Period, TrialNumber_Probes) %>% 
    summarise(MagEntries = mean(MagEntries),
              MagDuration = mean(MagDuration)) %>%
    ungroup() %>%
    ggplot(mapping = aes(x = as.factor(TrialNumber_Probes), y = MagEntries, group = CSPair, colour = CSPair, fill = CSPair, shape = CSPair,linetype = CSPair)) +
    stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
    ggtitle("Stage 4") + xlab("Trial") + ylab("Magazine Entry last 5s (CS-Pre)") +
    theme_cowplot(11) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(ylim = c(-2,2.0001)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_linetype_manual(name = "", values = linetypes)  +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_shape_manual(name = "", values = pointshapes) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(1,"line"))
  
  CI_Stage4_2nd5s_Freq_PerTrial_Combined <- shift_xaxis(Acqsuisition_Stage4_MagFreq_PerTrial_combined_2nd5s)
  
  
  ## LAST 5s Data
  ### Duration
  ### Per Trial 
  
  Acqsuisition_Stage4_MagDur_PerTrial_combined_2nd5s <- data_pertrial_last5s_CSPre %>% 
    filter(Period == "CSPre",
           CS_name != "C_O1"  & CS_name != "D_O2" ) %>% 
    group_by(Day, subject, CSPair, Period, TrialNumber_Probes) %>% 
    summarise(MagEntries = mean(MagEntries),
              MagDuration = mean(MagDuration)) %>%
    ungroup() %>%
    ggplot(mapping = aes(x = as.factor(TrialNumber_Probes), y = MagDuration, group = CSPair, colour = CSPair, fill = CSPair, shape = CSPair,linetype = CSPair)) +
    stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
    ggtitle("Stage 4") + xlab("Trial") + ylab("Magazine Duration last 5s (CS-Pre)") +
    theme_cowplot(11) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(ylim = c(-2,2.0001)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_linetype_manual(name = "", values = linetypes)  +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_shape_manual(name = "", values = pointshapes) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(1,"line"))
  
  CI_Stage4_2nd5s_Dur_PerTrial_Combined <- shift_xaxis(Acqsuisition_Stage4_MagDur_PerTrial_combined_2nd5s)
  
  
  
  
  ## Within CS 1s bins
  Acqsuisition_Stage4_MagFreq_1sBins <- rawdata %>% 
    filter(Period == "CS",
           CS_name != "C_O1" & CS_name != "D_O2" ) %>% 
    group_by(Day, subject, CSPair, Period, TrialNumber_Probes, bin_timewithin) %>% 
    summarise(MagEntries = mean(A3_freq),
              MagDuration = mean(A3_dur)) %>%
    ungroup() %>%
    ggplot(mapping = aes(x = as.factor(bin_timewithin), y = MagEntries, group = CSPair, colour = CSPair, fill = CSPair, shape = CSPair,linetype = CSPair)) +
    stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
    facet_wrap(~TrialNumber_Probes, nrow = 1) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,.2)) +
    ggtitle("Stage 4") + xlab("Trial") + ylab("Magazine Frequency (1s) ") +
    theme_cowplot(11) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(ylim = c(-0.2,0.6001)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_linetype_manual(name = "", values = linetypes)  +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_shape_manual(name = "", values = pointshapes) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(1,"line"))
  
  CI_Stage4_10s_Freq_WithinCS <- Acqsuisition_Stage4_MagFreq_1sBins
  
  Acqsuisition_Stage4_MagDur_1sBins <- rawdata %>% 
    filter(Period == "CS",
           CS_name != "C_O1" & CS_name != "D_O2" ) %>% 
    group_by(Day, subject, CSPair, Period, TrialNumber_Probes, bin_timewithin) %>% 
    summarise(MagEntries = mean(A3_freq),
              MagDuration = mean(A3_dur)) %>%
    ungroup() %>%
    ggplot(mapping = aes(x = as.factor(bin_timewithin), y = MagDuration, group = CSPair, colour = CSPair, fill = CSPair, shape = CSPair,linetype = CSPair)) +
    stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
    facet_wrap(~TrialNumber_Probes, nrow = 1) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,.2)) +
    ggtitle("Stage 4") + xlab("Trial") + ylab("Magazine Duration (1s) ") +
    theme_cowplot(11) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(ylim = c(-0.2,0.6001)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_linetype_manual(name = "", values = linetypes)  +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_shape_manual(name = "", values = pointshapes) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(1,"line"))
  
  CI_Stage4_10s_Dur_WithinCS <- Acqsuisition_Stage4_MagDur_1sBins

# Summation Test Trial 1 --------------------------------------------------

  ## 10s Data
  ### Frequency
  Acqsuisition_Stage4_MagFreq_Combined_trial1 <- data_pertrial_CSPre %>% 
    filter(Period == "CSPre",
           CS_name != "C_O1"  & CS_name != "D_O2",
           TrialNumber_Probes == 1) %>%
    group_by(Day, subject, CSPair, Period) %>% 
    summarise(MagEntries = mean(MagEntries),
              MagDuration = mean(MagDuration)) %>%
    ungroup() %>% 
    ggplot(mapping = aes(x = as.factor(CSPair), y = MagEntries, group = CSPair, colour = CSPair, fill = CSPair)) +
    stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,.5)) +
    ggtitle("Summation Test") + xlab("Compound") + ylab("Magazine Entry 10s (CS-Pre)") +
    theme_cowplot(11) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(ylim = c(-1,1.0001)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(0.5,"line"))

  
  CI_Stage4_10s_Freq_1stTrial_Combined <- shift_xaxis(Acqsuisition_Stage4_MagFreq_Combined_trial1)

  
  ## 10s Data
  ### Duration
  Acqsuisition_Stage4_MagDur_Combined_trial1 <- data_pertrial_CSPre %>% 
    filter(Period == "CSPre",
           CS_name != "C_O1"  & CS_name != "D_O2",
           TrialNumber_Probes == 1) %>%
    group_by(Day, subject, CSPair, Period) %>% 
    summarise(MagEntries = mean(MagEntries),
              MagDuration = mean(MagDuration)) %>%
    ungroup() %>% 
    ggplot(mapping = aes(x = as.factor(CSPair), y = MagDuration, group = CSPair, colour = CSPair, fill = CSPair)) +
    stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,.5)) +
    ggtitle("Summation Test") + xlab("Compound") + ylab("Magazine Duration 10s (CS-Pre)") +
    theme_cowplot(11) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(ylim = c(-1,1.0001)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(0.5,"line"))
  
  
  CI_Stage4_10s_Dur_1stTrial_Combined <- shift_xaxis(Acqsuisition_Stage4_MagDur_Combined_trial1)

  


  
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
  #   select(-MagDuration, -Period) %>%
  #   pivot_wider(names_from = subject, values_from = MagEntries) %>% 
  #   kable()
  # 
  
  
  # Save Stage 4 Figures ----------------------------------------------------
  
  # CI_Stage4_10s_Freq
  # CI_Stage4_10s_Dur
  # CI_Stage4_5s_Freq
  # CI_Stage4_5s_Dur
  
  
  # filename = here("figures", "CI_Stage4_10s_Freq.png")
  # ggsave(filename, CI_Stage4_10s_Freq, width = 80, height = 80, units = "mm", dpi = 1200)
  # filename = here("figures", "CI_Stage4_10s_Dur.png")
  # ggsave(filename, CI_Stage4_10s_Dur, width = 80, height = 80, units = "mm", dpi = 1200)
  # filename = here("figures", "CI_Stage4_5s_Freq.png")
  # ggsave(filename, CI_Stage4_5s_Freq, width = 80, height = 80, units = "mm", dpi = 1200)
  # filename = here("figures", "CI_Stage4_5s_Dur.png")
  # ggsave(filename, CI_Stage4_5s_Dur, width = 80, height = 80, units = "mm", dpi = 1200)  
  
  
  
  
  
  
  
  
  # Load Data - Stage 5 ---------------------------------------------------------------
  
  folderpath <- here("rawdata","Marios","1_SpecificCI","CombinedData")
  filename <- "CI_Stage5_ProcessedData_pertrial_1sbins.csv"
  
  rawdata <- read_csv(here(folderpath,filename))
  
  # Fix Day factor to numeric
  rawdata <- rawdata %>% 
    mutate(Day = as.numeric(str_remove(Day, "Day")))
  
  # Add new factors to represent predicted outcome identity and Cue pairs
  
  CS_name <- c("X_O1",
               "Y_O2",
               "Y_O1",
               "X_O2")
  
  Outcome <- c("O1",
               "O2",
               "O1",
               "O2")
  CSPair <- c("Congruent",
              "Congruent",
              "Incongruent",
              "Incongruent")
  
  newfactorlookup <- data.frame(CS_name, Outcome, CSPair)
  rawdata <- left_join(rawdata, newfactorlookup, by = "CS_name")
  
  data_PerSession <- rawdata %>% 
    group_by(Day, subject, Outcome, CSPair, CS_name, Period) %>% 
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
    group_by(Day, subject, Outcome, CSPair, CS_name, Period) %>% 
    summarise(MagEntries = mean(A3_freq)*5,
              MagDuration = mean(A3_dur)*5) %>%
    ungroup()
  
  
  data_PerSession_last5s_CSPre <- data_PerSession_last5s %>% 
    pivot_wider(names_from = Period,values_from = c(MagEntries, MagDuration)) %>% 
    mutate(MagEntries_CSPre = MagEntries_CS - MagEntries_Pre,
           MagDuration_CSPre = MagDuration_CS - MagDuration_Pre) %>% 
    pivot_longer(c(MagEntries_CS, MagEntries_Post, MagEntries_Pre, MagDuration_CS, MagDuration_Post, MagDuration_Pre, MagEntries_CSPre, MagDuration_CSPre), names_to = c("Measure", "Period"), names_sep = "_", values_to = "Mag") %>% 
    pivot_wider(names_from = Measure, values_from = Mag)
  
  
  #  Plots Stage 5 ----------------------------------------------------------
  
  ## 10s Data
  ### Frequency
  
  Acqsuisition_Stage5_MagFreq <- data_PerSession_CSPre %>% 
    filter(Period == "CSPre") %>%
    ggplot(mapping = aes(x = as.factor(Day), y = MagEntries, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
    stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
    ggtitle("Stage 5: Retardation") + xlab("Day") + ylab("Magazine Entry 10s (CS-Pre)") +
    theme_cowplot(11) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(ylim = c(-1,4.0001)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_linetype_manual(name = "", values = linetypes)  +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_shape_manual(name = "", values = pointshapes) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(1,"line"))
  
  CI_Stage5_10s_Freq <- shift_xaxis(Acqsuisition_Stage5_MagFreq)
  
  ## 10s Data
  ### Duration
  
  Acqsuisition_Stage5_MagDur <- data_PerSession_CSPre %>% 
    filter(Period == "CSPre") %>%
    ggplot(mapping = aes(x = as.factor(Day), y = MagDuration, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
    stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
    ggtitle("Stage 5: Retardation") + xlab("Day") + ylab("Magazine Duration 10s (CS-Pre)") +
    theme_cowplot(11) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(ylim = c(-1,4.0001)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_linetype_manual(name = "", values = linetypes)  +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_shape_manual(name = "", values = pointshapes) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(1,"line"))
  
  CI_Stage5_10s_Dur <- shift_xaxis(Acqsuisition_Stage5_MagDur)
  
  
  ## 5s Data
  ### Frequency
  
  Acqsuisition_Stage5_MagFreq_5s <- data_PerSession_last5s_CSPre %>% 
    filter(Period == "CSPre") %>%
    ggplot(mapping = aes(x = as.factor(Day), y = MagEntries, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
    stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
    ggtitle("Stage 5: Retardation") + xlab("Day") + ylab("Magazine Entry 5s (CS-Pre)") +
    theme_cowplot(11) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(ylim = c(-1,4.0001)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_linetype_manual(name = "", values = linetypes)  +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_shape_manual(name = "", values = pointshapes) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(1,"line"))
  
  CI_Stage5_5s_Freq <- shift_xaxis(Acqsuisition_Stage5_MagFreq_5s)
  
  ## 5s Data
  ### Duration
  
  Acqsuisition_Stage5_MagDur_5s <- data_PerSession_last5s_CSPre %>% 
    filter(Period == "CSPre") %>%
    ggplot(mapping = aes(x = as.factor(Day), y = MagDuration, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
    stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
    ggtitle("Stage 5: Retardation") + xlab("Day") + ylab("Magazine Duration 5s (CS-Pre)") +
    theme_cowplot(11) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(ylim = c(-1,4.0001)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_linetype_manual(name = "", values = linetypes)  +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_shape_manual(name = "", values = pointshapes) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(1,"line"))
  
  CI_Stage5_5s_Dur <- shift_xaxis(Acqsuisition_Stage5_MagDur_5s)
  
  

# Retardation Combined Conditions ----------------------------------------------------

    
    ## 10s Data
    ### Frequency
    
    Acqsuisition_Stage5_MagFreq_combined <- data_PerSession_CSPre %>% 
    filter(Period == "CSPre") %>% 
    group_by(Day, subject, CSPair, Period) %>% 
    summarise(MagEntries = mean(MagEntries),
              MagDuration = mean(MagDuration)) %>%
    ungroup() %>%
    ggplot(mapping = aes(x = as.factor(Day), y = MagEntries, group = CSPair, colour = CSPair, fill = CSPair, shape = CSPair,linetype = CSPair)) +
    stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
    ggtitle("Stage 5: Retardation") + xlab("Day") + ylab("Magazine Entry 10s (CS-Pre)") +
    theme_cowplot(11) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(ylim = c(-1,4.0001)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_linetype_manual(name = "", values = linetypes)  +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_shape_manual(name = "", values = pointshapes) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(1,"line"))
  
  CI_Stage5_10s_Freq_combined <- shift_xaxis(Acqsuisition_Stage5_MagFreq_combined)
  
  ## 10s Data
  ### Duration
  
  Acqsuisition_Stage5_MagDur_combined <- data_PerSession_CSPre %>% 
    filter(Period == "CSPre") %>% 
    group_by(Day, subject, CSPair, Period) %>% 
    summarise(MagEntries = mean(MagEntries),
              MagDuration = mean(MagDuration)) %>%
    ungroup() %>%
    ggplot(mapping = aes(x = as.factor(Day), y = MagDuration, group = CSPair, colour = CSPair, fill = CSPair, shape = CSPair,linetype = CSPair)) +
    stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
    ggtitle("Stage 5: Retardation") + xlab("Day") + ylab("Magazine Entry 10s (CS-Pre)") +
    theme_cowplot(11) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(ylim = c(-1,4.0001)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_linetype_manual(name = "", values = linetypes)  +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_shape_manual(name = "", values = pointshapes) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(1,"line"))
  
  CI_Stage5_10s_Dur_combined <- shift_xaxis(Acqsuisition_Stage5_MagDur_combined)
  
  
    ## 5s Data
    ### Frequency
    
    Acqsuisition_Stage5_MagFreq_combined_5s <- data_PerSession_last5s_CSPre %>% 
    filter(Period == "CSPre") %>% 
    group_by(Day, subject, CSPair, Period) %>% 
    summarise(MagEntries = mean(MagEntries),
              MagDuration = mean(MagDuration)) %>%
    ungroup() %>%
    ggplot(mapping = aes(x = as.factor(Day), y = MagEntries, group = CSPair, colour = CSPair, fill = CSPair, shape = CSPair,linetype = CSPair)) +
    stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
    ggtitle("Stage 5: Retardation") + xlab("Day") + ylab("Magazine Entry 10s (CS-Pre)") +
    theme_cowplot(11) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(ylim = c(-1,4.0001)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_linetype_manual(name = "", values = linetypes)  +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_shape_manual(name = "", values = pointshapes) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(1,"line"))
  
  CI_Stage5_5s_Freq_combined <- shift_xaxis(Acqsuisition_Stage5_MagFreq_combined_5s)
  
  ## 5s Data
  ### Duration
  
  Acqsuisition_Stage5_MagDur_combined_5s <- data_PerSession_last5s_CSPre %>% 
    filter(Period == "CSPre") %>% 
    group_by(Day, subject, CSPair, Period) %>% 
    summarise(MagEntries = mean(MagEntries),
              MagDuration = mean(MagDuration)) %>%
    ungroup() %>%
    ggplot(mapping = aes(x = as.factor(Day), y = MagDuration, group = CSPair, colour = CSPair, fill = CSPair, shape = CSPair,linetype = CSPair)) +
    stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
    ggtitle("Stage 5: Retardation") + xlab("Day") + ylab("Magazine Entry 10s (CS-Pre)") +
    theme_cowplot(11) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(ylim = c(-1,4.0001)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_linetype_manual(name = "", values = linetypes)  +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_shape_manual(name = "", values = pointshapes) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(1,"line"))
  
  CI_Stage5_5s_Dur_combined <- shift_xaxis(Acqsuisition_Stage5_MagDur_combined_5s)
  
  
  # Combined Figure Panels: Stages 1-3 ---------------------------------------------------
  
  ## 10s 
  A <- CI_Stage1_10s_Freq + theme(legend.position= c(0.05,.90), 
                                  legend.justification='left',
                                  legend.direction='vertical')
  
  B <- CI_Stage2_10s_Freq + theme(legend.position= c(0.05,.90), 
                                  legend.justification='left',
                                  legend.direction='vertical', 
                                  axis.title.y = element_blank())
  
  C <- CI_Stage3_10s_Freq + theme(legend.position= c(0.05,.90), 
                                  legend.justification='left',
                                  legend.direction='vertical', 
                                  axis.title.y = element_blank())
  

  
  ## 10s Duration
  D <- CI_Stage1_10s_Dur + theme(legend.position= c(0.05,.90), 
                                 legend.justification='left',
                                 legend.direction='vertical')
  
  E <- CI_Stage2_10s_Dur + theme(legend.position= c(0.05,.90), 
                                 legend.justification='left',
                                 legend.direction='vertical', 
                                 axis.title.y = element_blank())
  
  F1 <- CI_Stage3_10s_Dur + theme(legend.position= c(0.05,.90), 
                                 legend.justification='left',
                                 legend.direction='vertical', 
                                 axis.title.y = element_blank())
  
  CI_Stages123_10s_Combined <- (A + B + C + D + E +F1) + plot_annotation(tag_levels = 'A') + plot_layout(nrow = 2, ncol = 3, widths = c(1, 2, .5, 1, 2, .5))
  
  filename = here("figures", "CI_Stages123_10s_Combined.png")
  ggsave(filename, CI_Stages123_10s_Combined, width = 160, height = 160, units = "mm", dpi = 1200)
  
  
  ## 5s Combined
  A <- CI_Stage1_5s_Freq + theme(legend.position= c(0.05,.90), 
                                 legend.justification='left',
                                 legend.direction='vertical')
  
  B <- CI_Stage2_5s_Freq + theme(legend.position= c(0.05,.90), 
                                 legend.justification='left',
                                 legend.direction='vertical', 
                                 axis.title.y = element_blank())
  
  C <- CI_Stage3_5s_Freq + theme(legend.position= c(0.05,.90), 
                                 legend.justification='left',
                                 legend.direction='vertical', 
                                 axis.title.y = element_blank())

  ## 5s
  D <- CI_Stage1_5s_Dur + theme(legend.position= c(0.05,.90), 
                                legend.justification='left',
                                legend.direction='vertical')
  
  E <- CI_Stage2_5s_Dur + theme(legend.position= c(0.05,.90), 
                                legend.justification='left',
                                legend.direction='vertical', 
                                axis.title.y = element_blank())
  
  F1 <- CI_Stage3_5s_Dur + theme(legend.position= c(0.05,.90), 
                                legend.justification='left',
                                legend.direction='vertical', 
                                axis.title.y = element_blank())

  CI_Stages123_5s_Combined <- (A + B + C + D + E +F1) + plot_annotation(tag_levels = 'A') + plot_layout(nrow = 2, ncol = 3, widths = c(1, 2, .5, 1, 2, .5))
  
  filename = here("figures", "CI_Stages123_5s_Combined.png")
  ggsave(filename, CI_Stages123_5s_Combined, width = 160, height = 160, units = "mm", dpi = 1200)

# Combined Figure Panels: Test Sessions -----------------------------------
  # CI_Stage4_10s_Freq
  # CI_Stage4_10s_Dur
  # CI_Stage4_5s_Freq
  # CI_Stage4_5s_Dur
  # 
  # CI_Stage4_10s_Freq_PerTrial
  # CI_Stage4_10s_Dur_PerTrial
  # 
  # CI_Stage4_10s_Freq_PerTrial_Combined
  # CI_Stage4_10s_Dur_PerTrial_Combined
  # CI_Stage4_1st5s_Freq_PerTrial_Combined
  # CI_Stage4_1st5s_Dur_PerTrial_Combined
  # CI_Stage4_2nd5s_Freq_PerTrial_Combined
  # CI_Stage4_2nd5s_Dur_PerTrial_Combined
  # 
  # CI_Stage4_10s_Freq_WithinCS
  # CI_Stage4_10s_Dur_WithinCS
  # 
  # CI_Stage4_10s_Freq_1stTrial_Combined
  # CI_Stage4_10s_Dur_1stTrial_Combined
  # 
  # CI_Stage5_10s_Freq
  # CI_Stage5_10s_Dur
  # CI_Stage5_5s_Freq
  # CI_Stage5_5s_Dur
  # 
  # CI_Stage5_10s_Freq_combined
  # CI_Stage5_10s_Dur_combined
  # CI_Stage5_5s_Freq_combined
  # CI_Stage5_5s_Dur_combined
  
  
  ## Stage 4: Summation - Per Trial
  A <- CI_Stage4_10s_Freq_PerTrial + theme(legend.position= c(0.8,.90), 
                                  legend.justification='left',
                                  legend.direction='vertical')
  
  B <- CI_Stage4_10s_Dur_PerTrial + theme(legend.position= c(0.8,.90), 
                                  legend.justification='left',
                                  legend.direction='vertical')
  
  C <- CI_Stage4_10s_Freq_PerTrial_Combined + theme(legend.position= c(0.8,.90), 
                                  legend.justification='left',
                                  legend.direction='vertical')
  
  D <- CI_Stage4_10s_Dur_PerTrial_Combined + theme(legend.position= c(0.8,.90), 
                                                    legend.justification='left',
                                                    legend.direction='vertical')
  
 CI_Stage4_PerTrial_Analyses <- (A + B + C + D) + plot_annotation(tag_levels = 'A') + plot_layout(nrow = 2, ncol = 2, widths = c(1,1,1,1))
  
  filename = here("figures", "CI_Stage4_PerTrial_Analyses.png")
  ggsave(filename, CI_Stage4_PerTrial_Analyses, width = 180, height = 180, units = "mm", dpi = 1200)
  
## Stage 4: Summation - Separate Cues at Test Per Trial
  A <- CI_Stage4_10s_Freq_WithinCS + theme(legend.position= c(0.8,.90), 
                                           legend.justification='left',
                                           legend.direction='vertical')
  
  B <- CI_Stage4_10s_Dur_WithinCS + theme(legend.position= c(0.8,.90), 
                                          legend.justification='left',
                                          legend.direction='vertical')
  
  CI_Stage4_WithinCS_Analyses <- (A + B ) + plot_annotation(tag_levels = 'A') + plot_layout(nrow = 2, ncol = 1, widths = c(1,1))
  
  filename = here("figures", "CI_Stage4_WithinCS_Analyses.png")
  ggsave(filename, CI_Stage4_WithinCS_Analyses, width = 180, height = 180, units = "mm", dpi = 1200)
  
  ## Stage 4: Summation - 1st Trial Test
  

  
  A <- CI_Stage4_10s_Freq_1stTrial_Combined + theme(legend.position= c(0.8,.90), 
                                           legend.justification='left',
                                           legend.direction='vertical')
  
  B <- CI_Stage4_10s_Dur_1stTrial_Combined + theme(legend.position= c(0.8,.90), 
                                          legend.justification='left',
                                          legend.direction='vertical')
  
  CI_Stage4_1stTrial_Analyses <- (A + B ) + plot_annotation(tag_levels = 'A') + plot_layout(nrow = 2, ncol = 1, widths = c(1,1))
  
  filename = here("figures", "CI_Stage4_1stTrial_Analyses.png")
  ggsave(filename, CI_Stage4_1stTrial_Analyses, width = 90, height = 180, units = "mm", dpi = 1200)
  
  

  
  ## Stage 5: Retardation - 10s
  A <- CI_Stage5_10s_Freq + theme(legend.position= c(0.8,.90), 
                                           legend.justification='left',
                                           legend.direction='vertical')
  
  B <- CI_Stage5_10s_Dur + theme(legend.position= c(0.8,.90), 
                                          legend.justification='left',
                                          legend.direction='vertical')
  
  C <- CI_Stage5_10s_Freq_combined + theme(legend.position= c(0.8,.90), 
                                                    legend.justification='left',
                                                    legend.direction='vertical')
  
  D <- CI_Stage5_10s_Dur_combined + theme(legend.position= c(0.8,.90), 
                                                   legend.justification='left',
                                                   legend.direction='vertical')
  
  CI_Stage5_10s_Freq_Analyses <- (A + B + C + D) + plot_annotation(tag_levels = 'A') + plot_layout(nrow = 2, ncol = 2, widths = c(1,1,1,1))
  
  filename = here("figures", "CI_Stage5_10s_Freq_Analyses.png")
  ggsave(filename, CI_Stage5_10s_Freq_Analyses, width = 180, height = 180, units = "mm", dpi = 1200)
  
  ## Stage 5: Retardation - 10s 
  A <- CI_Stage5_10s_Freq + theme(legend.position= c(0.8,.90), 
                                  legend.justification='left',
                                  legend.direction='vertical')
  
  B <- CI_Stage5_10s_Dur + theme(legend.position= c(0.8,.90), 
                                 legend.justification='left',
                                 legend.direction='vertical')
  
  C <- CI_Stage5_10s_Freq_combined + theme(legend.position= c(0.8,.90), 
                                           legend.justification='left',
                                           legend.direction='vertical')
  
  D <- CI_Stage5_10s_Dur_combined + theme(legend.position= c(0.8,.90), 
                                          legend.justification='left',
                                          legend.direction='vertical')
  
  CI_Stage5_10s_Analyses <- (A + B + C + D) + plot_annotation(tag_levels = 'A') + plot_layout(nrow = 2, ncol = 2, widths = c(1,1,1,1))
  
  filename = here("figures", "CI_Stage5_10s_Freq_Analyses.png")
  ggsave(filename, CI_Stage5_10s_Freq_Analyses, width = 180, height = 180, units = "mm", dpi = 1200)
  

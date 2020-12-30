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
                 "BY_" = White)

linecolours <- c("A_O1" = DarkRed,
                 "B_O2" = DarkBlue, 
                 "C_O1" = MediumRed,
                 "D_O2" = MediumBlue,
                 "AX_" = LightRed,
                 "BY_" = LightBlue)


linetypes <- c("A_O1" = "solid",
               "B_O2" = "solid", 
               "C_O1" = "solid",
               "D_O2" = "solid",
               "AX_" = "dotted",
               "BY_" = "dotted")


pointshapes <- c("A_O1" = square,
                    "B_O2" = circle, 
                    "C_O1" = square,
                    "D_O2" = circle,
                    "AX_" = triangleUp,
                    "BY_" = triangleDown)


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
  coord_cartesian(ylim = c(-1,6.0001)) +
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
  coord_cartesian(ylim = c(-1,6.0001)) +
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
  coord_cartesian(ylim = c(-1,6.0001)) +
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
  coord_cartesian(ylim = c(-1,6.0001)) +
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


filename = here("figures", "CI_Stage1_10s_Freq.png")
ggsave(filename, CI_Stage1_10s_Freq, width = 80, height = 80, units = "mm", dpi = 1200)
filename = here("figures", "CI_Stage1_10s_Dur.png")
ggsave(filename, CI_Stage1_10s_Dur, width = 80, height = 80, units = "mm", dpi = 1200)
filename = here("figures", "CI_Stage1_5s_Freq.png")
ggsave(filename, CI_Stage1_5s_Freq, width = 80, height = 80, units = "mm", dpi = 1200)
filename = here("figures", "CI_Stage1_5s_Dur.png")
ggsave(filename, CI_Stage1_5s_Dur, width = 80, height = 80, units = "mm", dpi = 1200)



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
  coord_cartesian(ylim = c(-1,6.0001)) +
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
  coord_cartesian(ylim = c(-1,6.0001)) +
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
  coord_cartesian(ylim = c(-1,6.0001)) +
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
  coord_cartesian(ylim = c(-1,6.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

CI_Stage2_5s_Dur <- shift_xaxis(FeatureNegative_Stage2_MagDur_5s)

# Save Stage 2 Figures ----------------------------------------------------

# CI_Stage2_10s_Freq
# CI_Stage2_10s_Dur
# CI_Stage2_5s_Freq
# CI_Stage2_5s_Dur


filename = here("figures", "CI_Stage2_10s_Freq.png")
ggsave(filename, CI_Stage2_10s_Freq, width = 80, height = 80, units = "mm", dpi = 1200)
filename = here("figures", "CI_Stage2_10s_Dur.png")
ggsave(filename, CI_Stage2_10s_Dur, width = 80, height = 80, units = "mm", dpi = 1200)
filename = here("figures", "CI_Stage2_5s_Freq.png")
ggsave(filename, CI_Stage2_5s_Freq, width = 80, height = 80, units = "mm", dpi = 1200)
filename = here("figures", "CI_Stage2_5s_Dur.png")
ggsave(filename, CI_Stage2_5s_Dur, width = 80, height = 80, units = "mm", dpi = 1200)



# Save Stage 2 Data for analysis ------------------------------------------


savefile <- "CI_Stage2_CSPre.csv"
write_csv(data_PerSession_CSPre, here("figures", "figure_data", savefile))


savefile <- "CI_Stage2_CSPre_last5s.csv"
write_csv(data_PerSession_last5s_CSPre, here("figures", "figure_data", savefile))



# Combined Figure Panels ---------------------------------------------------

## 10s Frequency
A <- CI_Stage1_10s_Freq + theme(legend.position= c(0.05,.90), 
                                legend.justification='left',
                                legend.direction='vertical')

B <- CI_Stage2_10s_Freq + theme(legend.position= c(0.05,.90), 
                                legend.justification='left',
                                legend.direction='vertical', 
                                axis.title.y = element_blank())

CI_10s_Freq_Combined <- (A + B) + plot_annotation(tag_levels = 'A') + plot_layout(nrow = 1, widths = c(1, 1))

filename = here("figures", "CI_10s_Freq_Combined.png")
ggsave(filename, CI_10s_Freq_Combined, width = 120, height = 80, units = "mm", dpi = 1200)

## 10s Duration
  A <- CI_Stage1_10s_Dur + theme(legend.position= c(0.05,.90), 
                                  legend.justification='left',
                                  legend.direction='vertical')
  
  B <- CI_Stage2_10s_Dur + theme(legend.position= c(0.05,.90), 
                                  legend.justification='left',
                                  legend.direction='vertical', 
                                  axis.title.y = element_blank())
  
  CI_10s_Dur_Combined <- (A + B) + plot_annotation(tag_levels = 'A') + plot_layout(nrow = 1, widths = c(1, 1))
  
  filename = here("figures", "CI_10s_Dur_Combined.png")
  ggsave(filename, CI_10s_Dur_Combined, width = 120, height = 80, units = "mm", dpi = 1200)
  
  
  ## 5s Frequency
  A <- CI_Stage1_5s_Freq + theme(legend.position= c(0.05,.90), 
                                 legend.justification='left',
                                 legend.direction='vertical')
  
  B <- CI_Stage2_5s_Freq + theme(legend.position= c(0.05,.90), 
                                 legend.justification='left',
                                 legend.direction='vertical', 
                                 axis.title.y = element_blank())
  
  CI_5s_Freq_Combined <- (A + B) + plot_annotation(tag_levels = 'A') + plot_layout(nrow = 1, widths = c(1, 1))
  
  filename = here("figures", "CI_5s_Freq_Combined.png")
  ggsave(filename, CI_5s_Freq_Combined, width = 120, height = 80, units = "mm", dpi = 1200)
  
  ## 5s Duration
  A <- CI_Stage1_5s_Dur + theme(legend.position= c(0.05,.90), 
                                legend.justification='left',
                                legend.direction='vertical')
  
  B <- CI_Stage2_5s_Dur + theme(legend.position= c(0.05,.90), 
                                legend.justification='left',
                                legend.direction='vertical', 
                                axis.title.y = element_blank())
  
  CI_5s_Dur_Combined <- (A + B) + plot_annotation(tag_levels = 'A') + plot_layout(nrow = 1, widths = c(1, 1))
  
  filename = here("figures", "CI_5s_Dur_Combined.png")
  ggsave(filename, CI_5s_Dur_Combined, width = 120, height = 80, units = "mm", dpi = 1200)
  


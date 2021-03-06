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



fillcolours <- c("A+" = DarkRed,
                 "A-" = LightRed,
                 "B+" = DarkBlue,
                 "B-" = LightBlue)

linecolours <- c("A+" = DarkRed,
                 "A-" = MediumRed,
                 "B+" = DarkBlue,
                 "B-" = MediumBlue)


linetypes <- c("A+" = "solid",
               "A-" = "dotted",
               "B+" = "solid",
               "B-" = "dotted")


pointshapes <- c("A+" = square,
                 "A-" = circle,
                 "B+" = square,
                 "B-" = circle)


# Load Data ---------------------------------------------------------------

#  Acquisition Data -------------------------------------------------------

folderpath <- here("rawdata","Marios","2_ConditionedReinforcement","CombinedData")
filename <- "CRF_NovelLightAcquisition_ProcessedData_pertrial_1sbins.csv"

rawdata <- read_csv(here(folderpath,filename))

# Fix Day factor to numeric
rawdata <- rawdata %>% 
  mutate(Day = as.numeric(str_remove(Day, "Day")))

data_PerTrial <- rawdata %>% 
  mutate(trialnumber = ceiling(bin_trial/4)) %>% 
  group_by(Day, counterbalancing, subject, sex, trialnumber,state_ID, CS_name, Period) %>% 
  summarise(MagEntries = mean(A3_freq)*1,
            MagDuration = mean(A3_dur)*1) %>%
  ungroup()


data_PerTrial_CSPre <- data_PerTrial %>% 
  pivot_wider(names_from = Period,values_from = c(MagEntries, MagDuration)) %>% 
  mutate(MagEntries_CSPre = MagEntries_CS - MagEntries_Pre,
         MagDuration_CSPre = MagDuration_CS - MagDuration_Pre) %>% 
  pivot_longer(c(MagEntries_CS, MagEntries_Post, MagEntries_Pre, MagDuration_CS, MagDuration_Post, MagDuration_Pre, MagEntries_CSPre, MagDuration_CSPre), names_to = c("Measure", "Period"), names_sep = "_", values_to = "Mag") %>% 
  pivot_wider(names_from = Measure, values_from = Mag)


data_PerSession <- rawdata %>% 
  group_by(Day, counterbalancing, subject, sex, state_ID, CS_name, Period) %>% 
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
  group_by(Day, counterbalancing, subject,sex,state_ID, CS_name, Period) %>% 
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
  filter(Period == "CS")  %>%
  na.omit() %>% 
  ggplot(mapping = aes(x = as.factor(Day), y = MagEntries, group = state_ID, colour = state_ID, fill = state_ID, shape = state_ID,linetype = state_ID)) +
  # facet_wrap(~ sex) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # facet_wrap(~counterbalancing, ) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Acquisition") + xlab("Day") + ylab("Magazine Entry 10s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-2,4.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  # scale_linetype_manual(name = "", values = linetypes)  +
  # scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  # scale_shape_manual(name = "", values = pointshapes) +
  # scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

Acqsuisition_Stage1_MagFreq <- shift_xaxis(Acqsuisition_Stage1_MagFreq)
Acqsuisition_Stage1_MagFreq

Acqsuisition_Stage1_MagDur <- data_PerSession_CSPre %>% 
  filter(Period == "CS") %>%
  na.omit() %>% 
  ggplot(mapping = aes(x = as.factor(Day), y = MagDuration, group = state_ID, colour = state_ID, fill = state_ID, shape = state_ID,linetype = state_ID)) +
  # facet_wrap(~ sex) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # facet_wrap(~counterbalancing, ) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Acquisition") + xlab("Day") + ylab("Magazine Durations 10s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-2,4.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  # scale_linetype_manual(name = "", values = linetypes)  +
  # scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  # scale_shape_manual(name = "", values = pointshapes) +
  # scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

Acqsuisition_Stage1_MagDur <- shift_xaxis(Acqsuisition_Stage1_MagDur)
Acqsuisition_Stage1_MagDur


Acqsuisition_Stage1_MagFreq_Cbx <- data_PerSession_CSPre %>% 
  filter(Period == "CS")  %>%
  na.omit() %>% 
  ggplot(mapping = aes(x = as.factor(Day), y = MagEntries, group = state_ID, colour = state_ID, fill = state_ID, shape = state_ID,linetype = state_ID)) +
  # facet_wrap(~ sex) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  facet_wrap(~counterbalancing, ) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Acquisition") + xlab("Day") + ylab("Magazine Entry 10s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-2,4.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  # scale_linetype_manual(name = "", values = linetypes)  +
  # scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  # scale_shape_manual(name = "", values = pointshapes) +
  # scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

Acqsuisition_Stage1_MagFreq_Cbx <- shift_xaxis_facet(Acqsuisition_Stage1_MagFreq_Cbx)
Acqsuisition_Stage1_MagFreq_Cbx

Acqsuisition_Stage1_MagDur_Cbx <- data_PerSession_CSPre %>% 
  filter(Period == "CS") %>%
  na.omit() %>% 
  ggplot(mapping = aes(x = as.factor(Day), y = MagDuration, group = state_ID, colour = state_ID, fill = state_ID, shape = state_ID,linetype = state_ID)) +
  # facet_wrap(~ sex) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  facet_wrap(~counterbalancing, ) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Acquisition") + xlab("Day") + ylab("Magazine Durations 10s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-2,4.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  # scale_linetype_manual(name = "", values = linetypes)  +
  # scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  # scale_shape_manual(name = "", values = pointshapes) +
  # scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

Acqsuisition_Stage1_MagDur_Cbx <- shift_xaxis_facet(Acqsuisition_Stage1_MagDur_Cbx)
Acqsuisition_Stage1_MagDur_Cbx

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
  coord_cartesian(ylim = c(-2,3.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

Acqsuisition_Stage1_MagFreq_last5s <- shift_xaxis(Acqsuisition_Stage1_MagFreq_last5s)
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
  coord_cartesian(ylim = c(-2,3.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

Acqsuisition_Stage1_MagDur_last5s <- shift_xaxis(Acqsuisition_Stage1_MagDur_last5s)
Acqsuisition_Stage1_MagDur_last5s

# ACquisition - Per Trial  ------------------------------------------------

# Acquisition Plots -------------------------------------------------------

Acqsuisition_Stage1_PerTrial_MagFreq <- data_PerTrial_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(trialnumber), y = MagEntries, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  # facet_wrap(~ sex) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  facet_wrap(~Day, ) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,0.2)) +
  ggtitle("Acquisition") + xlab("Trial Number") + ylab("Magazine Entry 1s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-0.4,0.4001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

Acqsuisition_Stage1_PerTrial_MagFreq <- shift_xaxis_facet(Acqsuisition_Stage1_PerTrial_MagFreq)
Acqsuisition_Stage1_PerTrial_MagFreq

Acqsuisition_Stage1_PerTrial_MagDur <- data_PerTrial_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(trialnumber), y = MagDuration, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
  # facet_wrap(~ sex) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  facet_wrap(~Day, ) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,0.2)) +
  ggtitle("Acquisition") + xlab("Trial Number") + ylab("Magazine Duration 1s (CS-Pre)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-0.4,0.4001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

Acqsuisition_Stage1_PerTrial_MagDur <- shift_xaxis_facet(Acqsuisition_Stage1_PerTrial_MagDur)
Acqsuisition_Stage1_PerTrial_MagDur


# Save Stage 1 Data for analysis ------------------------------------------


savefile <- "CRF_NovelLightAcquisition_CSPre.csv"
write_csv(data_PerSession_CSPre, here("figures", "figure_data", savefile))


savefile <- "CRF_NovelLightAcquisition_CSPre_Last5s.csv"
write_csv(data_PerSession_last5s_CSPre, here("figures", "figure_data", savefile))


# Acquisition Data


A <- Acqsuisition_Stage1_MagFreq + theme(legend.position= c(0.05,.90), 
                                    legend.justification='left',
                                    legend.direction='vertical')

B <- Acqsuisition_Stage1_MagDur + theme(legend.position= c(0.05,.90), 
                                   legend.justification='left',
                                   legend.direction='vertical')


CRF_NovelLightAcquisition_10s_combined <- (A + B) + plot_annotation(tag_levels = 'A') + plot_layout(nrow = 2, ncol = 1, widths = c(1, 1))

filename = here("figures", "CRF_NovelLightAcquisition_10s_combined.png")
ggsave(filename, CRF_NovelLightAcquisition_10s_combined, width = 100, height = 160, units = "mm", dpi = 1200)

# Acquisition Data Cbx


A <- Acqsuisition_Stage1_MagFreq_Cbx + theme(legend.position= c(0.05,.90), 
                                         legend.justification='left',
                                         legend.direction='vertical')

B <- Acqsuisition_Stage1_MagDur_Cbx + theme(legend.position= c(0.05,.90), 
                                        legend.justification='left',
                                        legend.direction='vertical')


CRF_NovelLightAcquisition_10s_combined_Cbx <- (A + B) + plot_annotation(tag_levels = 'A') + plot_layout(nrow = 2, ncol = 1, widths = c(1, 1))

filename = here("figures", "CRF_NovelLightAcquisition_10s_combined_Cbx.png")
ggsave(filename, CRF_NovelLightAcquisition_10s_combined_Cbx, width = 160, height = 280, units = "mm", dpi = 1200)

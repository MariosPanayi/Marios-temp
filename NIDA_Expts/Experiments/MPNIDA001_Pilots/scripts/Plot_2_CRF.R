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



# Load Data ---------------------------------------------------------------

folderpath <- here("rawdata","Marios","2_ConditionedReinforcement","CombinedData")
filename <- "CRF_ProcessedData_pertrial_1sbins.csv"

rawdata <- read_csv(here(folderpath,filename))

data_PerSession <- rawdata %>% 
  group_by(Day, subject, CS_name, Period) %>% 
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
  group_by(Day, subject, CS_name, Period) %>% 
  summarise(MagEntries = mean(A3_freq)*5,
            MagDuration = mean(A3_dur)*5) %>%
  ungroup()


data_PerSession_last5s_CSPre <- data_PerSession_last5s %>% 
  pivot_wider(names_from = Period,values_from = c(MagEntries, MagDuration)) %>% 
  mutate(MagEntries_CSPre = MagEntries_CS - MagEntries_Pre,
         MagDuration_CSPre = MagDuration_CS - MagDuration_Pre) %>% 
  pivot_longer(c(MagEntries_CS, MagEntries_Post, MagEntries_Pre, MagDuration_CS, MagDuration_Post, MagDuration_Pre, MagEntries_CSPre, MagDuration_CSPre), names_to = c("Measure", "Period"), names_sep = "_", values_to = "Mag") %>% 
  pivot_wider(names_from = Measure, values_from = Mag)



Acqsuisition_Stage1_MagFreq <- data_PerSession_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(Day), y = MagEntries, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
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
  # scale_linetype_manual(name = "", values = linetypes)  +
  # scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  # scale_shape_manual(name = "", values = pointshapes) +
  # scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

Acqsuisition_Stage1_MagFreq


Acqsuisition_Stage1_MagDur <- data_PerSession_CSPre %>% 
  filter(Period == "CSPre") %>%
  ggplot(mapping = aes(x = as.factor(Day), y = MagDuration, group = CS_name, colour = CS_name, fill = CS_name, shape = CS_name,linetype = CS_name)) +
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
  # scale_linetype_manual(name = "", values = linetypes)  +
  # scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  # scale_shape_manual(name = "", values = pointshapes) +
  # scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

Acqsuisition_Stage1_MagDur

# #Inspect individual animals
# 
# data_PerSession_CSPre %>% 
#   filter(Period == "CS") %>% 
#   select(-MagEntries, -Period) %>%
#   pivot_wider(names_from = subject, values_from = MagDuration) %>% 
#   kable()
# 
data_PerSession_CSPre %>%
  filter(Period == "CS") %>%
  select(-MagDuration, -Period) %>%
  pivot_wider(names_from = subject, values_from = MagEntries) %>%
  kable()


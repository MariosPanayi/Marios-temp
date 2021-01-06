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

folderpath <- here("rawdata","Marios","3_LeverPressingForLights","CombinedData")
filename <- "LPL_ProcessedData_WithinSession7_5minBins.csv"

rawdata <- read_csv(here(folderpath,filename))

## Rceode lever identity based on counterbalancing
data_recode <- rawdata %>% 
  group_by(Day, subject) %>% 
  mutate(LP_Freq_Flash  = ifelse(FLash_leverCbx == "Left", A1_freq, ifelse(FLash_leverCbx == "Right", A2_freq, NA)),
         LP_Dur_Flash  = ifelse(FLash_leverCbx == "Left", A1_dur, ifelse(FLash_leverCbx == "Right", A2_dur, NA)),
         LP_Freq_Steady  = ifelse(Steady_levercbx == "Left", A1_freq, ifelse(Steady_levercbx == "Right", A2_freq, NA)),
         LP_Dur_Steady  = ifelse(Steady_levercbx == "Left", A1_dur, ifelse(Steady_levercbx == "Right", A2_dur, NA)) 
         ) %>% 
  ungroup()

## relabel data
data_bin <- data_recode %>%
  group_by(Day, counterbalancing, timebins, subject) %>%
  summarise(LPFreq_Flash = sum(LP_Freq_Flash),
            LPDur_Flash = sum(LP_Dur_Flash),
            LPFreq_Steady = sum(LP_Freq_Steady),
            LPDur_Steady = sum(LP_Dur_Steady),
            Reinforcer_Flash = sum(Flash_freq),
            Reinforcer_Steady = sum(Steady_freq),
            
  ) %>%
  ungroup()

#Long format  
data_bin_long <- data_bin %>% 
  pivot_longer(c(LPFreq_Flash,LPDur_Flash, LPFreq_Steady,LPDur_Steady, Reinforcer_Flash, Reinforcer_Steady), names_to = c("Measure","Stimulus"), names_sep = "_", values_to = "LP") %>% 
  pivot_wider(names_from = Measure, values_from = LP)

# Summarise at total presses per day
data_PerSession <- data_recode %>%
  group_by(Day, counterbalancing, subject) %>%
  summarise(LPFreq_Flash = sum(LP_Freq_Flash),
            LPDur_Flash = sum(LP_Dur_Flash),
            LPFreq_Steady = sum(LP_Freq_Steady),
            LPDur_Steady = sum(LP_Dur_Steady),
            Reinforcer_Flash = sum(Flash_freq),
            Reinforcer_Steady = sum(Steady_freq),
            
) %>%
  ungroup()

#Long format
data_PerSession_long <- data_PerSession %>% 
  pivot_longer(c(LPFreq_Flash,LPDur_Flash, LPFreq_Steady,LPDur_Steady, Reinforcer_Flash, Reinforcer_Steady), names_to = c("Measure","Stimulus"), names_sep = "_", values_to = "LP") %>% 
  pivot_wider(names_from = Measure, values_from = LP)


Acquisition_PerSession <- data_PerSession_long %>% 
  ggplot(mapping = aes(x = as.factor(Day), y = Reinforcer, group = Stimulus, colour = Stimulus, fill = Stimulus, shape = Stimulus, linetype = Stimulus)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,2)) +
  ggtitle("Acquisition") + xlab("Day") + ylab("Total Reinforcers (30 mins)") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,14.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  # scale_linetype_manual(name = "", values = linetypes)  +
  # scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  # scale_shape_manual(name = "", values = pointshapes) +
  # scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

Acquisition_PerSession



Acquisition_PerBin <- data_bin_long %>% 
  ggplot(mapping = aes(x = as.factor(timebins), y = Reinforcer, group = Stimulus, colour = Stimulus, fill = Stimulus, shape = Stimulus, linetype = Stimulus)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  facet_wrap(~Day, nrow = 1) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-100,100,1)) +
  ggtitle("Acquisition") + xlab("Bin 7.5 mins") + ylab("Total Reinforcers") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,5.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  # scale_linetype_manual(name = "", values = linetypes)  +
  # scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  # scale_shape_manual(name = "", values = pointshapes) +
  # scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))

Acquisition_PerBin



## Packages for data organisation and plotting
library(tidyverse)
# Package for relative file paths
library(here)
library(ggpubr)
library(cowplot)
library(ggsignif)
library(patchwork)
library(RColorBrewer)
library(magick)
################################################################################
## Packages for Data analysis
library(afex)
afex_options(emmeans_model = "multivariate")# use multivariate model for all follow-up tests.
library(emmeans)
# install.packages("devtools")
# devtools::install_github("crsh/papaja")
library(papaja)
library(knitr)

# 
# 


## Behavioural Data ----
# Total Mag Entries
full_data_TotMagEntries <-read_csv(here("rawdata", "/AwakeVarReward_behaviour_rawdata.csv"))
#Pre filter data for any exclusions
full_data_TotMagEntries <- full_data_TotMagEntries %>% 
  filter(include == TRUE,
         uniqueID %in% c('29_20130227_1_SAL',
                         '29_20130301_1_LY',
                         '32_20130227_1_LY',
                         '32_20130301_1_SAL',
                         '34_20130227_1_LY',
                         '34_20130301_1_SAL',
                         '52_20130911_1_SAL',
                         '52_20130913_1_LY',
                         '54_20140617_1_LY',
                         '54_20140619_1_SAL',
                         '55_20140617_1_SAL',
                         '55_20140619_1_LY',
                         '56_20140617_0_LY',
                         '56_20140619_0_SAL',
                         '57_20140618_1_SAL',
                         '57_20140620_1_LY',
                         '69_20141209_1_LY',
                         '69_20141211_1_SAL',
                         '72_20141210_1_LY',
                         '72_20141212_1_SAL'))

# Re order and rename levels for plotting
full_data_TotMagEntries$drug <- fct_relevel(full_data_TotMagEntries$drug, c("SAL", "LY"))
levels <- c("Saline" = "SAL", "LY354740" = "LY")
full_data_TotMagEntries$drug <- fct_recode(full_data_TotMagEntries$drug, !!!levels)


fillcolours <- c( "Saline" = "#D9D9D9",  "LY354740" = "#B2182B")

# Plot for fun
Expt5_awakeFCV_TotMagEntries <- ggplot(data = full_data_TotMagEntries, mapping = aes(x = drug, y = MagFrequency, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,1000,50)) +
  ggtitle("") + xlab("") + ylab("Total magazine entries") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,300)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill"))


# Latency/mag entry Pre reward data
full_data_latencies <-read_csv(here("rawdata", "/LY354740_Rat_awake_behaviour_latencies.csv"))
#Pre filter data for any exclusions
full_data_latencies <- full_data_latencies %>% 
  mutate(responselatency = responselatency/10) %>% 
  filter(includeSubj == TRUE,
         trialexclude == FALSE,
         uniqueID %in% c('29_20130227_1_SAL',
                         '29_20130301_1_LY',
                         '32_20130227_1_LY',
                         '32_20130301_1_SAL',
                         '34_20130227_1_LY',
                         '34_20130301_1_SAL',
                         '52_20130911_1_SAL',
                         '52_20130913_1_LY',
                         '54_20140617_1_LY',
                         '54_20140619_1_SAL',
                         '55_20140617_1_SAL',
                         '55_20140619_1_LY',
                         '56_20140617_0_LY',
                         '56_20140619_0_SAL',
                         '57_20140618_1_SAL',
                         '57_20140620_1_LY',
                         '69_20141209_1_LY',
                         '69_20141211_1_SAL',
                         '72_20141210_1_LY',
                         '72_20141212_1_SAL'))

# Summarise individual trial data into average measures 
total_latencies <- full_data_latencies %>% 
  group_by(subject, drug) %>% 
  summarise(Latency_median = median(responselatency, na.rm=TRUE),
            MagEntry10sPre = mean(MagEntry10sPre, na.rm=TRUE),
            MagEntry10sPost = mean(MagEntry10sPost, na.rm=TRUE))

# Re order and rename levels for plotting
total_latencies$drug <- fct_relevel(total_latencies$drug, c("SAL", "LY"))
levels <- c("Saline" = "SAL", "LY354740" = "LY")
total_latencies$drug <- fct_recode(total_latencies$drug, !!!levels)


fillcolours <- c( "Saline" = "#D9D9D9",  "LY354740" = "#B2182B")

# Plot for fun
Expt5_awakeFCV_MagLatency <- ggplot(data = total_latencies, mapping = aes(x = drug, y = Latency_median, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,10,.2)) +
  ggtitle("") + xlab("") + ylab("Latency (s)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,1.2001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill"))


# Plot for fun
Expt5_awakeFCV_MagEntryPreReward <- ggplot(data = total_latencies, mapping = aes(x = drug, y = MagEntry10sPre, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,10,.5)) +
  ggtitle("") + xlab("") + ylab("Magazine entries \n pre-reward (10s)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,3)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill"))

# # Plot for fun
# Expt5_awakeFCV_MagEntrPostReward <- ggplot(data = total_latencies, mapping = aes(x = drug, y = MagEntry10sPost, group = drug, fill = drug)) +
#   stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
#   stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
#   # Make Pretty
#   scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,10,.2)) +
#   ggtitle("") + xlab("") + ylab("Magazine entries \n post-reward (10s)") +
#   theme_cowplot(10) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(plot.title = element_text(size=8)) +
#   coord_cartesian(ylim = c(0,4)) +
#   theme(axis.title.x=element_text(face = "bold")) +
#   theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
#   scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill"))




## Avg FCV Reward Traces for each reward Magnitude ----

full_data_AVgTraces <- read_csv(here("rawdata", "/AwakeVarReward_Traces.csv"))

plot_data_AVgTraces <- full_data_AVgTraces %>% 
  filter(Time >= -2 & Time < 5.1) %>% 
  mutate(RewardMagnitude = as.factor(RewardMagnitude))

# Re order and rename levels for plotting
plot_data_AVgTraces$drug <- fct_relevel(plot_data_AVgTraces$drug, c("SAL", "LY"))
levels <- c("Saline" = "SAL", "LY354740" = "LY")
plot_data_AVgTraces$drug <- fct_recode(plot_data_AVgTraces$drug, !!!levels)

plot_data_AVgTraces$RewardMagnitude <- fct_relevel(plot_data_AVgTraces$RewardMagnitude, c("1", "2", "3"))
levels <- c("Small" = "1", "Medium" = "2", "Large" = "3")
plot_data_AVgTraces$RewardMagnitude <- fct_recode(plot_data_AVgTraces$RewardMagnitude, !!!levels)


linecolours <- c( "Saline" = "#000000",  "LY354740" = "#B2182B")
fillcolours <- c( "Saline" = "#D9D9D9",  "LY354740" = "#B2182B")

# Expt5_awakeFCV_AvgTraces <- ggplot(data = plot_data_AVgTraces, mapping = aes(x = as.factor(Time), y = DA, group = interaction(drug,RewardMagnitude), colour = drug, fill = drug)) +
#   # stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
#   stat_summary_bin(fun.data = "mean_se", geom = "ribbon", colour = NA,  show.legend = FALSE, alpha = .4) +
#   stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
#   facet_grid(. ~ RewardMagnitude) +
#   # Make Pretty
#   scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-10,10,.1)) +
#   ggtitle("") + xlab("") + ylab("") +
#   theme_cowplot(8) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(plot.title = element_text(size=8)) +
#   coord_cartesian(ylim = c(-.2,.75), xlim = c(10, 70)) +
#   theme(axis.title.x=element_text(face = "bold")) +
#   scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
#   scale_fill_manual(name = "", values = fillcolours) +
#   theme(legend.key.width=unit(1.5,"line")) +
#   geom_signif(y_position = c(-.1),xmin = c("0"), xmax = c("1"), annotation = c("1s"), tip_length = c(.0, .0), size = .5, hjust = 0.5, vjust = 2,linetype = 1, colour = "black") +
#   geom_signif(y_position = c(.1),xmin = c("0"), xmax = c("0"), annotation = c("0.1 nA"), tip_length = c(.05, .0), size = .5, hjust = 1.1, vjust = 4,linetype = 1, colour = "black") 
# 
#  
#   
#   
#   TopPanel <-  Expt5_awakeFCV_AvgTraces + theme(strip.background = element_rect(color= NA, fill= NA), 
#                                                 strip.text.x = element_text(size = 8, color = "black", face = "bold.italic", angle = 0, hjust = 0), strip.placement = "outside") + 
#                 theme(axis.title.x = element_blank(), axis.text.x = element_blank(),axis.ticks.x = element_blank())+ 
#                 theme(axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank())+
#                 theme(line = element_blank()) +
#                 theme(panel.spacing = unit(2, "lines"))
 
  
  Expt5_awakeFCV_AvgTraces_small <- plot_data_AVgTraces %>% 
    filter(RewardMagnitude == "Small",
           Time > -1.1 & Time < 5.1) %>% 
    ggplot(mapping = aes(x = as.factor(Time), y = DA, group = interaction(drug,RewardMagnitude), colour = drug, fill = drug)) +
    # stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    stat_summary_bin(fun.data = "mean_se", geom = "ribbon", colour = NA,  show.legend = FALSE, alpha = .6) +
    stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-.2,10,.1)) +
    ggtitle("Small") + xlab("") + ylab("") +
    theme_cowplot(8) +
    theme(plot.title = element_text(hjust = 0)) +
    theme(plot.title = element_text(size=8)) +
    coord_cartesian(ylim = c(-.2,.75)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(1.5,"line")) +
    geom_signif(y_position = c(-.12),xmin = c("0"), xmax = c("1"), annotation = c("1s"), tip_length = c(.0, .0), size = .3, hjust = 0.5, vjust = 2,linetype = 1, colour = "black",  textsize=4) +
    geom_signif(y_position = c(.5),xmin = c("0"), xmax = c("0"), annotation = c("0.5 nA"), tip_length = c(.25, .0), size = .3, hjust = .1, vjust = 4,linetype = 1, colour = "black",  textsize=4) +
    theme(strip.background = element_rect(color= NA, fill= NA), 
            strip.text.x = element_text(size = 8, color = "black", face = "bold.italic", angle = 0, hjust = 0), strip.placement = "outside") + 
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(),axis.ticks.x = element_blank())+ 
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank())+
    theme(line = element_blank()) +
    theme(panel.spacing = unit(2, "lines")) 
  
  Expt5_awakeFCV_AvgTraces_medium <- plot_data_AVgTraces %>% 
    filter(RewardMagnitude == "Medium",
           Time > -1.1 & Time < 5.1) %>%  
    ggplot(mapping = aes(x = as.factor(Time), y = DA, group = interaction(drug,RewardMagnitude), colour = drug, fill = drug)) +
    # stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    stat_summary_bin(fun.data = "mean_se", geom = "ribbon", colour = NA,  show.legend = FALSE, alpha = .6) +
    stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-10,10,.1)) +
    ggtitle("Medium") + xlab("") + ylab("") +
    theme_cowplot(8) +
    theme(plot.title = element_text(hjust = 0)) +
    theme(plot.title = element_text(size=8)) +
    coord_cartesian(ylim = c(-.2,.75)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(1.5,"line")) +
    geom_signif(y_position = c(-.12),xmin = c("0"), xmax = c("1"), annotation = c("1s"), tip_length = c(.0, .0), size = .3, hjust = 0.5, vjust = 2,linetype = 1, colour = "black",  textsize=4) +
    geom_signif(y_position = c(.5),xmin = c("0"), xmax = c("0"), annotation = c("0.5 nA"), tip_length = c(.25, .0), size = .3, hjust = .1, vjust = 4,linetype = 1, colour = "black",  textsize=4) +
    theme(strip.background = element_rect(color= NA, fill= NA), 
            strip.text.x = element_text(size = 8, color = "black", face = "bold.italic", angle = 0, hjust = 0), strip.placement = "outside") + 
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(),axis.ticks.x = element_blank())+ 
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank())+
    theme(line = element_blank()) +
    theme(panel.spacing = unit(2, "lines")) 
  
  Expt5_awakeFCV_AvgTraces_large <- plot_data_AVgTraces %>% 
    filter(RewardMagnitude == "Large",
           Time > -1.1 & Time < 5.1) %>% 
    ggplot(mapping = aes(x = as.factor(Time), y = DA, group = interaction(drug,RewardMagnitude), colour = drug, fill = drug)) +
    # stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    stat_summary_bin(fun.data = "mean_se", geom = "ribbon", colour = NA,  show.legend = FALSE, alpha = .6) +
    stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-10,10,.1)) +
    ggtitle("Large") + xlab("") + ylab("") +
    theme_cowplot(8) +
    theme(plot.title = element_text(hjust = 0)) +
    theme(plot.title = element_text(size=8)) +
    coord_cartesian(ylim = c(-.2,.75)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(1.5,"line")) +
    geom_signif(y_position = c(-.12),xmin = c("0"), xmax = c("1"), annotation = c("1s"), tip_length = c(.0, .0), size = .3, hjust = 0.5, vjust = 2,linetype = 1, colour = "black",  textsize=4) +
    geom_signif(y_position = c(.5),xmin = c("0"), xmax = c("0"), annotation = c("0.5 nA"), tip_length = c(.25, .0), size = .3, hjust = .1, vjust = 4,linetype = 1, colour = "black",  textsize=4) +
    theme(strip.background = element_rect(color= NA, fill= NA), 
            strip.text.x = element_text(size = 8, color = "black", face = "bold.italic", angle = 0, hjust = 0), strip.placement = "outside") + 
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(),axis.ticks.x = element_blank())+ 
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank())+
    theme(line = element_blank()) +
    theme(panel.spacing = unit(2, "lines"))
  
  
  

  
# Representative CV and trace for plotting with Tarheel colourplot ----
  Expt5_awakeFCV_RepresentativeCV_data <- read_csv(here("rawdata", "/LY354740_Rat69_Ch0_LY_representativeCV.csv"))
  # Re order and rename levels for plotting
  Expt5_awakeFCV_RepresentativeCV_data$drug <- fct_relevel(Expt5_awakeFCV_RepresentativeCV_data$drug, c("Sal", "LY"))
  levels <- c("Saline" = "Sal", "LY354740" = "LY")
  Expt5_awakeFCV_RepresentativeCV_data$drug <- fct_recode(Expt5_awakeFCV_RepresentativeCV_data$drug, !!!levels)
  
  
  
  linecolours <- c( "Saline" = "#000000",  "LY354740" = "#B2182B")
  fillcolours <- c( "Saline" = "#D9D9D9",  "LY354740" = "#B2182B")
  
  Expt5_awakeFCV_RepresentativeCV_Saline <- Expt5_awakeFCV_RepresentativeCV_data %>% 
    filter(drug == "Saline") %>% 
  ggplot(mapping = aes(x = as.numeric(Voltage), y = nA, group = drug, colour = drug, fill = drug)) +
    # stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    geom_path() +
    # Make Pretty
  # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-10,10,.1)) +
    ggtitle("") + xlab("") + ylab("") +
    theme_cowplot(8) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=8)) +
    coord_cartesian(ylim = c(-.6,1.8)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_fill_manual(name = "", values = fillcolours) +
    geom_signif(y_position = c(.9),xmin = c("0.3"), xmax = c("0.3"), annotation = c("0.5 nA"), textsize = 3, tip_length = c(.25, .0), size = .2, hjust = 1.1, vjust = 4,linetype = 1, colour = "black") +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(),axis.ticks.x = element_blank())+ 
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank())+
    # theme(line = element_blank())+ 
    theme(legend.position="none") 
  
  Expt5_awakeFCV_RepresentativeCV_LY <- Expt5_awakeFCV_RepresentativeCV_data %>% 
    filter(drug == "LY354740") %>% 
    ggplot(mapping = aes(x = as.numeric(Voltage), y = nA, group = drug, colour = drug, fill = drug)) +
    # stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    geom_path() +
    # Make Pretty
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-10,10,.1)) +
    ggtitle("") + xlab("") + ylab("") +
    theme_cowplot(8) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=8)) +
    coord_cartesian(ylim = c(-.6,1.8)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_fill_manual(name = "", values = fillcolours) +
    geom_signif(y_position = c(.9),xmin = c("0.3"), xmax = c("0.3"), annotation = c("0.5 nA"), textsize = 3, tip_length = c(.25, .0), size = .2, hjust = 1.1, vjust = 4,linetype = 1, colour = "black") +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(),axis.ticks.x = element_blank())+ 
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank())+
    # theme(line = element_blank())+ 
    theme(legend.position="none") 
  
  
# Representative traces ----  
  # Load representative Trace data
  Expt5_awakeFCV_RepresentativeTrace_data <-read_csv(here("rawdata", "/LY354740_Rat69_Ch0_LY_representativeTraces.csv"))
  # Re order and rename levels for plotting
  Expt5_awakeFCV_RepresentativeTrace_data$drug <- fct_relevel(Expt5_awakeFCV_RepresentativeTrace_data$drug, c("Sal", "LY"))
  levels <- c("Saline" = "Sal", "LY354740" = "LY")
  Expt5_awakeFCV_RepresentativeTrace_data$drug <- fct_recode(Expt5_awakeFCV_RepresentativeTrace_data$drug, !!!levels)

  
  linecolours <- c( "Saline" = "#000000",  "LY354740" = "#B2182B")
  fillcolours <- c( "Saline" = "#D9D9D9",  "LY354740" = "#B2182B")
  
  Expt5_awakeFCV_RepresentativeTrace_Saline <- Expt5_awakeFCV_RepresentativeTrace_data %>% 
    filter(drug == "Saline") %>% 
    ggplot(mapping = aes(x = as.numeric(Time), y = nA, group = drug, colour = drug, fill = drug)) +
    # stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    geom_path() +
    # Make Pretty
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-10,10,.1)) +
    ggtitle("") + xlab("") + ylab("") +
    theme_cowplot(8) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=8)) +
    coord_cartesian(ylim = c(-.8,2)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_fill_manual(name = "", values = fillcolours) +
    geom_signif(y_position = c(-.2),xmin = c("0"), xmax = c("1"), annotation = c("1s"), tip_length = c(.0, .0), size = .5, hjust = 0.5, vjust = 2,linetype = 1, colour = "black") +
    geom_signif(y_position = c(.5),xmin = c("0"), xmax = c("0"), annotation = c("0.5 nA"), tip_length = c(.25, .0), size = .5, hjust = 1.1, vjust = 4,linetype = 1, colour = "black") +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(),axis.ticks.x = element_blank())+
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank())+
    theme(line = element_blank())+
    theme(legend.position="none") 
  
  Expt5_awakeFCV_RepresentativeTrace_LY <- Expt5_awakeFCV_RepresentativeTrace_data %>% 
    filter(drug == "LY354740") %>% 
    ggplot(mapping = aes(x = as.numeric(Time), y = nA, group = drug, colour = drug, fill = drug)) +
    # stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
    geom_path() +
    # Make Pretty
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-10,10,.1)) +
    ggtitle("") + xlab("") + ylab("") +
    theme_cowplot(8) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=8)) +
    coord_cartesian(ylim = c(-.8,2)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_fill_manual(name = "", values = fillcolours) +
    geom_signif(y_position = c(-.2),xmin = c("0"), xmax = c("1"), annotation = c("1s"), tip_length = c(.0, .0), size = .5, hjust = 0.5, vjust = 2,linetype = 1, colour = "black") +
    geom_signif(y_position = c(.5),xmin = c("0"), xmax = c("0"), annotation = c("0.5 nA"), tip_length = c(.25, .0), size = .5, hjust = 1.1, vjust = 4,linetype = 1, colour = "black") +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(),axis.ticks.x = element_blank())+ 
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank())+
    theme(line = element_blank())+ 
    theme(legend.position="none") 
  


  

  
  
  
# Summary stats ----
#Read in the data for Awake FCV DA responses
full_data_awake <- read_csv("./rawdata/AwakeVarReward_rawdata.csv")
#Pre filter data for any exclusions
full_data_awake <- full_data_awake %>% 
  filter(include == TRUE) %>% 
  mutate(rewardMagnitude = as.factor(rewardMagnitude))

# Re order and rename levels for plotting
full_data_awake$drug <- fct_relevel(full_data_awake$drug, c("SAL", "LY"))
levels <- c("Saline" = "SAL", "LY354740" = "LY")
full_data_awake$drug <- fct_recode(full_data_awake$drug, !!!levels)


full_data_awake$rewardMagnitude <- fct_relevel(full_data_awake$rewardMagnitude, c("1", "2", "3"))
levels <- c("Small" = "1", "Medium" = "2", "Large" = "3")
full_data_awake$rewardMagnitude <- fct_recode(full_data_awake$rewardMagnitude, !!!levels)

fillcolours <- c( "Saline" = "#D9D9D9",  "LY354740" = "#B2182B")


        
# Plot for fun
Expt5_awakeFCV_Peak <- ggplot(data = full_data_awake, mapping = aes(x = rewardMagnitude, y = peak, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,50,.2)) +
  ggtitle("") + xlab("Reward Magnitude") + ylab("Peak DA (nA)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,1)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) +
  geom_signif(y_position = .94, xmin = "Small", xmax ="Large", annotation = "**", tip_length = c(.00, 0.00), size = .5,  vjust = 0.5 )


# Plot for fun
Expt5_awakeFCV_AUC <- ggplot(data = full_data_awake, mapping = aes(x = rewardMagnitude, y = AUC, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(0,100,5))  +
  ggtitle("") + xlab("Reward Magnitude") + ylab("AUC") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,35)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) +
  geom_signif(y_position = c(27, 29, 31), xmin = c(.75, 1.75, 2.75), xmax = c( 1.25, 2.25, 3.25), annotation = c("*","**","**"), tip_length = c(.01, 0.01), size = .5,  vjust = 0.5 )


# Plot for fun
Expt5_awakeFCV_latency <- ggplot(data = full_data_awake, mapping = aes(x = rewardMagnitude, y = latency2peak, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(0,100,1)) +
  ggtitle("") + xlab("Reward Magnitude") + ylab("Latency (s)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,4)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) +
  geom_signif(y_position = c(3.5, 3.2), xmin = c("Small", "Medium"), xmax = c( "Large", "Large"), annotation = c("**","**"), tip_length = c(.01, 0.01), size = .5,  vjust = 0.5 )



# Combined plotting layouts ----
traceLegend <- get_legend(Expt5_awakeFCV_AvgTraces_small)
TopPanel <- plot_grid(Expt5_awakeFCV_AvgTraces_small,
                      Expt5_awakeFCV_AvgTraces_medium, 
                      Expt5_awakeFCV_AvgTraces_large,
                      ncol = 1, rel_widths = c(1,1,1,.4))

A<-Expt5_awakeFCV_Peak  + 
  theme(legend.position="none") + 
  theme(axis.title.x = element_blank())


B<-Expt5_awakeFCV_AUC + 
  theme(legend.position="none") 


C<-Expt5_awakeFCV_latency + 
  theme(axis.title.x = element_blank())



D<-Expt5_awakeFCV_TotMagEntries + 
  theme(legend.position="none") + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),axis.ticks.x = element_blank())

E<-Expt5_awakeFCV_MagEntryPreReward + 
  theme(legend.position="none") + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),axis.ticks.x = element_blank())

F1<-Expt5_awakeFCV_MagLatency + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),axis.ticks.x = element_blank())


blank <- ggplot() + geom_blank() 



layout <- "
AABB
AABB
CDE#
FGH#
"
Fig3 <- TopPanel + blank + A + B + C + D + E + F1 + plot_annotation(tag_levels = 'A') +
  plot_layout(design = layout)

# Fig3 <- (TopPanel + blank)/(A + B + C)/(D + E + F1 ) + plot_annotation(tag_levels = 'A')


filename = here("figures", "Fig3.png")
ggsave(filename, Fig3, width =  6, height = 8, units = "in", dpi = 1200)
filename = here("figures", "Fig3.pdf")
ggsave(filename, Fig3, width =  6, height = 8, units = "in")






representativeTraces <- Expt5_awakeFCV_RepresentativeTrace_LY + Expt5_awakeFCV_RepresentativeTrace_Saline
representativeCVs <- Expt5_awakeFCV_RepresentativeCV_LY + Expt5_awakeFCV_RepresentativeCV_Saline

filename = here("figures", "Fig3_Manual_Traces.pdf")
ggsave(filename, representativeTraces, width =  3, height = 1.5, units = "in")

filename = here("figures", "Fig3_Manual_CVs.pdf")
ggsave(filename, representativeCVs, width =  2, height = 1, units = "in")

# Figures exported and modified manually in inkscape
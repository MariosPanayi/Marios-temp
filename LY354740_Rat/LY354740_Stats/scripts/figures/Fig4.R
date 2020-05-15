# Load Packages ----
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
## Packages for Data analysis
library(afex)
afex_options(emmeans_model = "multivariate")# use multivariate model for all follow-up tests.
library(emmeans)
# install.packages("devtools")
# devtools::install_github("crsh/papaja")
library(papaja)
library(knitr)
#

# Avg Traces ----

## Avg FCV Reward Traces for each reward Magnitude

full_data_AVgTraces <- read_csv(here("rawdata", "/Anaesthetized_Traces.csv"))

plot_data_AVgTraces <- full_data_AVgTraces %>% 
  filter(Time >= -2 & Time < 5.1)


# Re order and rename levels for plotting
plot_data_AVgTraces$drug <- fct_relevel(plot_data_AVgTraces$drug, c("SAL", "LY"))
levels <- c("Saline" = "SAL", "LY354740" = "LY")
plot_data_AVgTraces$drug <- fct_recode(plot_data_AVgTraces$drug, !!!levels)

plot_data_AVgTraces$Period <- fct_relevel(plot_data_AVgTraces$Period, c("Pre", "Post"))



linecolours <- c( "Saline" = "#000000",  "LY354740" = "#B2182B")
fillcolours <- c( "Saline" = "#D9D9D9",  "LY354740" = "#B2182B")



Expt6_aFCV_AvgTraces_Pre <- plot_data_AVgTraces %>% 
  filter(Period == "Pre",
         Time > -1.1 & Time < 5.1) %>% 
  ggplot(mapping = aes(x = as.factor(Time), y = DA, group = interaction(drug,Period), colour = drug, fill = drug)) +
  # stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "ribbon", colour = NA,  show.legend = FALSE, alpha = .6) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-2,10,.1)) +
  ggtitle("Pre") + xlab("") + ylab("") +
  theme_cowplot(8) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(-2,8)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1.5,"line")) +
  geom_signif(y_position = c(-.7),xmin = c("0"), xmax = c("1"), annotation = c("1s"), tip_length = c(.0, .0), size = .3, hjust = 0.5, vjust = 2,linetype = 1, colour = "black",  textsize=4) +
  geom_signif(y_position = c(2),xmin = c("0"), xmax = c("0"), annotation = c("1 nA"), tip_length = c(.1, .0), size = .3, hjust = .1, vjust = 4,linetype = 1, colour = "black",  textsize=4) +
  theme(strip.background = element_rect(color= NA, fill= NA),
        strip.text.x = element_text(size = 8, color = "black", face = "bold.italic", angle = 0, hjust = 0), strip.placement = "outside") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),axis.ticks.x = element_blank())+
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank())+
  theme(line = element_blank())



Expt6_aFCV_AvgTraces_Post <- plot_data_AVgTraces %>% 
  filter(Period == "Post",
         Time > -1.1 & Time < 5.1) %>% 
  ggplot(mapping = aes(x = as.factor(Time), y = DA, group = interaction(drug,Period), colour = drug, fill = drug)) +
  # stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "ribbon", colour = NA,  show.legend = FALSE, alpha = .6) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-2,10,.1)) +
  ggtitle("Post") + xlab("") + ylab("") +
  theme_cowplot(8) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(-2,8)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1.5,"line")) +
  geom_signif(y_position = c(-.7),xmin = c("0"), xmax = c("1"), annotation = c("1s"), tip_length = c(.0, .0), size = .3, hjust = 0.5, vjust = 2,linetype = 1, colour = "black",  textsize=4) +
  geom_signif(y_position = c(2),xmin = c("0"), xmax = c("0"), annotation = c("1 nA"), tip_length = c(.1, .0), size = .3, hjust = .1, vjust = 4,linetype = 1, colour = "black",  textsize=4) +
  theme(strip.background = element_rect(color= NA, fill= NA),
        strip.text.x = element_text(size = 8, color = "black", face = "bold.italic", angle = 0, hjust = 0), strip.placement = "outside") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),axis.ticks.x = element_blank())+
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank())+
  theme(line = element_blank())



# Representative traces/CVs ----

# Representative CV and trace for plotting with Tarheel colourplot
Expt6_aFCV_RepresentativeCV_data <- read_csv(here("rawdata", "/LY354740_Anaesthetized_representativeCVs.csv"))
# Re order and rename levels for plotting
Expt6_aFCV_RepresentativeCV_data$drug <- fct_relevel(Expt6_aFCV_RepresentativeCV_data$drug, c("SAL", "LY"))
levels <- c("Saline" = "SAL", "LY354740" = "LY")
Expt6_aFCV_RepresentativeCV_data$drug <- fct_recode(Expt6_aFCV_RepresentativeCV_data$drug, !!!levels)



linecolours <- c( "Saline" = "#000000",  "LY354740" = "#B2182B")
fillcolours <- c( "Saline" = "#D9D9D9",  "LY354740" = "#B2182B")

Expt6_aFCV_RepresentativeCV_Saline <- Expt6_aFCV_RepresentativeCV_data %>% 
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
  coord_cartesian(ylim = c(-2,6)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  geom_signif(y_position = c(.9),xmin = c("0.3"), xmax = c("0.3"), annotation = c("0.5 nA"), textsize = 3, tip_length = c(.25, .0), size = .2, hjust = 1.1, vjust = 4,linetype = 1, colour = "black") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),axis.ticks.x = element_blank())+ 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank())+
  # theme(line = element_blank())+ 
  theme(legend.position="none") 

Expt6_aFCV_RepresentativeCV_LY <- Expt6_aFCV_RepresentativeCV_data %>% 
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
  coord_cartesian(ylim = c(-2,6)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  geom_signif(y_position = c(.9),xmin = c("0.3"), xmax = c("0.3"), annotation = c("0.5 nA"), textsize = 3, tip_length = c(.25, .0), size = .2, hjust = 1.1, vjust = 4,linetype = 1, colour = "black") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),axis.ticks.x = element_blank())+ 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank())+
  # theme(line = element_blank())+ 
  theme(legend.position="none") 



# Load representative Trace data
Expt6_aFCV_RepresentativeTrace_data <-read_csv(here("rawdata", "/LY354740_Anaesthetized_representativeTraces.csv"))
# Re order and rename levels for plotting
Expt6_aFCV_RepresentativeTrace_data$drug <- fct_relevel(Expt6_aFCV_RepresentativeTrace_data$drug, c("SAL", "LY"))
levels <- c("Saline" = "SAL", "LY354740" = "LY")
Expt6_aFCV_RepresentativeTrace_data$drug <- fct_recode(Expt6_aFCV_RepresentativeTrace_data$drug, !!!levels)


linecolours <- c( "Saline" = "#000000",  "LY354740" = "#B2182B")
fillcolours <- c( "Saline" = "#D9D9D9",  "LY354740" = "#B2182B")

Expt6_aFCV_RepresentativeTrace_Saline <- Expt6_aFCV_RepresentativeTrace_data %>% 
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
  coord_cartesian(ylim = c(-2,6)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  geom_signif(y_position = c(-.2),xmin = c("0"), xmax = c("1"), annotation = c("1s"), tip_length = c(.0, .0), size = .5, hjust = 0.5, vjust = 2,linetype = 1, colour = "black") +
  geom_signif(y_position = c(.5),xmin = c("0"), xmax = c("0"), annotation = c("0.5 nA"), tip_length = c(.25, .0), size = .5, hjust = 1.1, vjust = 4,linetype = 1, colour = "black") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),axis.ticks.x = element_blank())+
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank())+
  theme(line = element_blank())+
  theme(legend.position="none") 

Expt6_aFCV_RepresentativeTrace_LY <- Expt6_aFCV_RepresentativeTrace_data %>% 
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
  coord_cartesian(ylim = c(-2,6)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  geom_signif(y_position = c(-.2),xmin = c("0"), xmax = c("1"), annotation = c("1s"), tip_length = c(.0, .0), size = .5, hjust = 0.5, vjust = 2,linetype = 1, colour = "black") +
  geom_signif(y_position = c(.5),xmin = c("0"), xmax = c("0"), annotation = c("0.5 nA"), tip_length = c(.25, .0), size = .5, hjust = 1.1, vjust = 4,linetype = 1, colour = "black") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),axis.ticks.x = element_blank())+ 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank())+
  theme(line = element_blank())+ 
  theme(legend.position="none") 


# Stim Response Curves ----

#Read in the data for aFCV DA responses
Expt6_aFCV_StimResponse_data <- read_csv("./rawdata/Anaesthetized_rawdata.csv")
Expt6_aFCV_StimResponse_data <- Expt6_aFCV_StimResponse_data %>% 
  rename(drug = drugs,
         period = ExperimentalStage,
         Subj = subj) %>% 
  filter(period == "02_StimIntensity_Pre" | period == "03_StimPulse_Pre" | period == "06_StimIntensity_Post" | period == "07_StimPulses_Post")

# Re order and rename levels for plotting
Expt6_aFCV_StimResponse_data$drug <- fct_relevel(Expt6_aFCV_StimResponse_data$drug, c("SAL", "LY"))
levels <- c("Saline" = "SAL", "LY354740" = "LY")
Expt6_aFCV_StimResponse_data$drug <- fct_recode(Expt6_aFCV_StimResponse_data$drug, !!!levels)


fillcolours <- c( "Saline" = "#D9D9D9",  "LY354740" = "#B2182B")





#Read in the data for aFCV DA responses
Expt6_aFCV_PrePost_data <- read_csv("./rawdata/Anaesthetized_rawdata.csv")
Expt6_aFCV_PrePost_data <- Expt6_aFCV_PrePost_data %>% 
  mutate(period = ExperimentalStage) %>% 
  separate(ExperimentalStage, c("stagenumber", "stagename", "injectiontime")) %>% 
  rename(drug = drugs,
         Subj = subj) %>% 
  filter(period == "04_Baseline2_Pre" | period == "05_Baseline_Post")

# Re order and rename levels for plotting
Expt6_aFCV_PrePost_data$drug <- fct_relevel(Expt6_aFCV_PrePost_data$drug, c("SAL", "LY"))
levels <- c("Saline" = "SAL", "LY354740" = "LY")
Expt6_aFCV_PrePost_data$drug <- fct_recode(Expt6_aFCV_PrePost_data$drug, !!!levels)

Expt6_aFCV_PrePost_data$injectiontime <- fct_relevel(Expt6_aFCV_PrePost_data$injectiontime, c("Pre", "Post"))

# Pre-Post Summary statistics ----

fillcolours <- c( "Saline" = "#D9D9D9",  "LY354740" = "#B2182B")

Expt6_aFCV_PrePost_DA_max <-  Expt6_aFCV_PrePost_data %>% 
  ggplot( mapping = aes(x = as.factor(injectiontime), y = DA_max, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,50,1)) +
  ggtitle("Peak") +  xlab("Period")  + ylab("Peak DA (nA)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,8)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) 


Expt6_aFCV_PrePost_DA_AUC <-  Expt6_aFCV_PrePost_data %>% 
  ggplot( mapping = aes(x = as.factor(injectiontime), y = DA_AUC, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,600,50)) +
  ggtitle("AUC") +  xlab("Period")  + ylab("AUC") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,150)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) 

Expt6_aFCV_PrePost_DA_latency <-  Expt6_aFCV_PrePost_data %>% 
  ggplot( mapping = aes(x = as.factor(injectiontime), y = DA_Latency_s, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,10,0.2)) +
  ggtitle("Latency") +  xlab("Period")  + ylab("Latency (s)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,2)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) 


# T50 anbalysis - don't include
# #Load t50 data - only including extimates where Rsquared > 0.8
# Expt6_aFCV_PrePost_t50_data <- read_csv("./rawdata/Anaesthetized_rawdata_t50.csv")
# #Filter the relevant stage of data for analysis
# Expt6_aFCV_PrePost_t50_data <- Expt6_aFCV_PrePost_t50_data %>% 
#   rename(drug = drugs) %>% 
#   filter(ExperimentalStage == "04_Baseline2_Pre" | ExperimentalStage == "05_Baseline_Post") %>% 
#   separate(ExperimentalStage, c("stagenumber", "stagename", "injectiontime"))
# 
# # Re order and rename levels for plotting
# Expt6_aFCV_PrePost_t50_data$drug <- fct_relevel(Expt6_aFCV_PrePost_t50_data$drug, c("SAL", "LY"))
# levels <- c("Saline" = "SAL", "LY354740" = "LY")
# Expt6_aFCV_PrePost_t50_data$drug <- fct_recode(Expt6_aFCV_PrePost_t50_data$drug, !!!levels)
# 
# Expt6_aFCV_PrePost_t50_data$injectiontime <- fct_relevel(Expt6_aFCV_PrePost_t50_data$injectiontime, c("Pre", "Post"))
# 
# 
# Expt6_aFCV_PrePost_DA_t50 <-  Expt6_aFCV_PrePost_t50_data %>% 
#   ggplot( mapping = aes(x = as.factor(injectiontime), y = t50_s, group = drug, fill = drug)) +
#   stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
#   stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
#   # Make Pretty
#   scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,10,0.2)) +
#   ggtitle("Decay") +  xlab("Period")  + ylab("t50 (s)") +
#   theme_cowplot(10) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(plot.title = element_text(size=10)) +
#   coord_cartesian(ylim = c(0,2)) +
#   theme(axis.title.x=element_text(face = "bold")) +
#   theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
#   scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) 

## StimCurve Pre Raw ====
### Intensity ####

# Plot for fun
Expt6_StimIntensityPre_DA_max <-
  Expt6_aFCV_StimResponse_data %>% 
  filter(period == "02_StimIntensity_Pre") %>% 
  ggplot( mapping = aes(x = as.factor(stimStrength), y = DA_max, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,50,1)) +
  ggtitle("Peak") + xlab(expression("Intensity" ~(mu*A)))  + ylab("Peak DA (nA)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,12)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) 

Expt6_StimIntensityPre_DA_AUC <-
  Expt6_aFCV_StimResponse_data %>% 
  filter(period == "02_StimIntensity_Pre") %>% 
  ggplot( mapping = aes(x = as.factor(stimStrength), y = DA_AUC, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,600,50)) +
  ggtitle("AUC") + xlab(expression("Intensity" ~(mu*A)))  + ylab("AUC") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,200)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) 

Expt6_StimIntensityPre_DA_Latency <-
  Expt6_aFCV_StimResponse_data %>% 
  filter(period == "02_StimIntensity_Pre") %>% 
  ggplot( mapping = aes(x = as.factor(stimStrength), y = DA_Latency_s, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,10,0.2)) +
  ggtitle("Latency") + xlab(expression("Intensity" ~(mu*A)))  + ylab("Latency (s)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,2)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) 


### Pulses ####
Expt6_StimPulsesPre_DA_max <-
  Expt6_aFCV_StimResponse_data %>% 
  filter(period == "03_StimPulse_Pre") %>% 
  ggplot( mapping = aes(x = as.factor(stimPulses), y = DA_max, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,50,1)) +
  ggtitle("Peak") + xlab("Pulses")  + ylab("Peak DA (nA)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,12)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) 

Expt6_StimPulsesPre_DA_AUC <-
  Expt6_aFCV_StimResponse_data %>% 
  filter(period == "03_StimPulse_Pre") %>% 
  ggplot( mapping = aes(x = as.factor(stimPulses), y = DA_AUC, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,600,50)) +
  ggtitle("AUC") +xlab("Pulses")  + ylab("AUC") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,200)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) 

Expt6_StimPulsesPre_DA_Latency <-
  Expt6_aFCV_StimResponse_data %>% 
  filter(period == "03_StimPulse_Pre") %>% 
  ggplot( mapping = aes(x = as.factor(stimPulses), y = DA_Latency_s, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,10,0.2)) +
  ggtitle("Latency") + xlab("Pulses")  + ylab("Latency (s)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,2)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) 

## StimCurve Post Raw ====
### Intensity ####

Expt6_StimIntensityPost_DA_max <-
  Expt6_aFCV_StimResponse_data %>% 
  filter(period == "06_StimIntensity_Post") %>% 
  ggplot( mapping = aes(x = as.factor(stimStrength), y = DA_max, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,50,1)) +
  ggtitle("Peak") + xlab(expression("Intensity" ~(mu*A)))  + ylab("Peak DA (nA)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,12)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) 

Expt6_StimIntensityPost_DA_AUC <-
  Expt6_aFCV_StimResponse_data %>% 
  filter(period == "06_StimIntensity_Post") %>% 
  ggplot( mapping = aes(x = as.factor(stimStrength), y = DA_AUC, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,600,50)) +
  ggtitle("AUC") + xlab(expression("Intensity" ~(mu*A)))  + ylab("AUC") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,200)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) 

Expt6_StimIntensityPost_DA_Latency <-
  Expt6_aFCV_StimResponse_data %>% 
  filter(period == "06_StimIntensity_Post") %>% 
  ggplot( mapping = aes(x = as.factor(stimStrength), y = DA_Latency_s, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,10,0.2)) +
  ggtitle("Latency") + xlab(expression("Intensity" ~(mu*A)))  + ylab("Latency (s)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,2)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) 

### Pulses ####

Expt6_StimPulsesPost_DA_max <-
  Expt6_aFCV_StimResponse_data %>% 
  filter(period == "07_StimPulses_Post") %>% 
  ggplot( mapping = aes(x = as.factor(stimPulses), y = DA_max, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,50,1)) +
  ggtitle("Peak") + xlab("Pulses")  + ylab("Peak DA (nA)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,12)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) 

Expt6_StimPulsesPost_DA_AUC <-
  Expt6_aFCV_StimResponse_data %>% 
  filter(period == "07_StimPulses_Post") %>% 
  ggplot( mapping = aes(x = as.factor(stimPulses), y = DA_AUC, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,600,50)) +
  ggtitle("AUC") + xlab("Pulses")  + ylab("AUC") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,200)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) 

Expt6_StimPulsesPost_DA_Latency <-
  Expt6_aFCV_StimResponse_data %>% 
  filter(period == "07_StimPulses_Post") %>% 
  ggplot( mapping = aes(x = as.factor(stimPulses), y = DA_Latency_s, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,10,0.2)) +
  ggtitle("Latency") +xlab("Pulses")  + ylab("Latency (s)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,2)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) 

## StimCurve Pre %Baseline ====
### Intensity ####

# Plot for fun
Expt6_StimIntensityPre_DA_max_PercBaseline <-
  Expt6_aFCV_StimResponse_data %>% 
  filter(period == "02_StimIntensity_Pre") %>% 
  ggplot( mapping = aes(x = as.factor(stimStrength), y = DA_max_PercBaseline, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,600,50)) +
  ggtitle("Peak") + xlab(expression("Intensity" ~(mu*A)))  + ylab("Peak DA (% baseline)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,200)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) 

Expt6_StimIntensityPre_DA_AUC_PercBaseline <-
  Expt6_aFCV_StimResponse_data %>% 
  filter(period == "02_StimIntensity_Pre") %>% 
  ggplot( mapping = aes(x = as.factor(stimStrength), y = DA_AUC_PercBaseline, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,600,50)) +
  ggtitle("AUC") + xlab(expression("Intensity" ~(mu*A)))  + ylab("AUC (% baseline)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,200)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) 

Expt6_StimIntensityPre_DA_Latency_PercBaseline <-
  Expt6_aFCV_StimResponse_data %>% 
  filter(period == "02_StimIntensity_Pre") %>% 
  ggplot( mapping = aes(x = as.factor(stimStrength), y = DA_Latency_s_PercBaseline, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,600,50)) +
  ggtitle("Latency") + xlab(expression("Intensity" ~(mu*A)))  + ylab("Latency (% baseline)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,200)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) 


### Pulses ####
Expt6_StimPulsesPre_DA_max_PercBaseline <-
  Expt6_aFCV_StimResponse_data %>% 
  filter(period == "03_StimPulse_Pre") %>% 
  ggplot( mapping = aes(x = as.factor(stimPulses), y = DA_max_PercBaseline, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,600,50)) +
  ggtitle("Peak") + xlab("Pulses")  + ylab("Peak DA (% baseline)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,200)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) 

Expt6_StimPulsesPre_DA_AUC_PercBaseline <-
  Expt6_aFCV_StimResponse_data %>% 
  filter(period == "03_StimPulse_Pre") %>% 
  ggplot( mapping = aes(x = as.factor(stimPulses), y = DA_AUC_PercBaseline, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,600,50)) +
  ggtitle("AUC") +xlab("Pulses")  + ylab("AUC (% baseline)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,200)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) 

Expt6_StimPulsesPre_DA_Latency_PercBaseline <-
  Expt6_aFCV_StimResponse_data %>% 
  filter(period == "03_StimPulse_Pre") %>% 
  ggplot( mapping = aes(x = as.factor(stimPulses), y = DA_Latency_s_PercBaseline, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,600,50)) +
  ggtitle("Latency") + xlab("Pulses")  + ylab("Latency (% baseline)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,200)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) 

## StimCurve Post %Baseline ====
### Intensity ####

Expt6_StimIntensityPost_DA_max_PercBaseline <-
  Expt6_aFCV_StimResponse_data %>% 
  filter(period == "06_StimIntensity_Post") %>% 
  ggplot( mapping = aes(x = as.factor(stimStrength), y = DA_max_PercBaseline, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,600,50)) +
  ggtitle("Peak") + xlab(expression("Intensity" ~(mu*A)))  + ylab("Peak DA (% baseline)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,200)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) 

Expt6_StimIntensityPost_DA_AUC_PercBaseline <-
  Expt6_aFCV_StimResponse_data %>% 
  filter(period == "06_StimIntensity_Post") %>% 
  ggplot( mapping = aes(x = as.factor(stimStrength), y = DA_AUC_PercBaseline, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,600,50)) +
  ggtitle("AUC") + xlab(expression("Intensity" ~(mu*A)))  + ylab("AUC (% baseline)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,200)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) 

Expt6_StimIntensityPost_DA_Latency_PercBaseline <-
  Expt6_aFCV_StimResponse_data %>% 
  filter(period == "06_StimIntensity_Post") %>% 
  ggplot( mapping = aes(x = as.factor(stimStrength), y = DA_Latency_s_PercBaseline, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,600,50)) +
  ggtitle("Latency") + xlab(expression("Intensity" ~(mu*A)))  + ylab("Latency (% baseline)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,200)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) 

### Pulses ####

Expt6_StimPulsesPost_DA_max_PercBaseline <-
  Expt6_aFCV_StimResponse_data %>% 
  filter(period == "07_StimPulses_Post") %>% 
  ggplot( mapping = aes(x = as.factor(stimPulses), y = DA_max_PercBaseline, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,600,50)) +
  ggtitle("Peak") + xlab("Pulses")  + ylab("Peak DA (% baseline)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,200)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) 

Expt6_StimPulsesPost_DA_AUC_PercBaseline <-
  Expt6_aFCV_StimResponse_data %>% 
  filter(period == "07_StimPulses_Post") %>% 
  ggplot( mapping = aes(x = as.factor(stimPulses), y = DA_AUC_PercBaseline, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,600,50)) +
  ggtitle("AUC") + xlab("Pulses")  + ylab("AUC (% baseline)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,200)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) 

Expt6_StimPulsesPost_DA_Latency_PercBaseline <-
  Expt6_aFCV_StimResponse_data %>% 
  filter(period == "07_StimPulses_Post") %>% 
  ggplot( mapping = aes(x = as.factor(stimPulses), y = DA_Latency_s_PercBaseline, group = drug, fill = drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=seq(0,600,50)) +
  ggtitle("Latency") +xlab("Pulses")  + ylab("Latency (% baseline)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(0,200)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_colour_manual(name = "", values = fillcolours, aesthetics = c("fill")) 

# Combined Plotting Layouts ----


blank <- ggplot() + geom_blank() 

B1 <- Expt6_aFCV_AvgTraces_Pre 
B2 <- Expt6_aFCV_AvgTraces_Post




C <- Expt6_aFCV_PrePost_DA_max + 
  theme(legend.position="none") + 
  theme(axis.title.x = element_blank())

D <- Expt6_aFCV_PrePost_DA_AUC + 
  theme(legend.position="none") + 
  theme(axis.title.x = element_blank())

E <- Expt6_aFCV_PrePost_DA_latency + 
  theme(legend.position="none") + 
  theme(axis.title.x = element_blank()) 


F1 <- Expt6_StimIntensityPost_DA_max_PercBaseline  + 
  theme(legend.position="none") + 
  theme(plot.title = element_blank()) 


G <-  Expt6_StimIntensityPost_DA_AUC_PercBaseline  + 
  theme(legend.position="none") + 
  theme(plot.title = element_blank())


H <- Expt6_StimIntensityPost_DA_Latency_PercBaseline + 
  theme(legend.position="none") + 
  theme(plot.title = element_blank())



I <- Expt6_StimPulsesPost_DA_max_PercBaseline  + 
  theme(legend.position="none") + 
  theme(plot.title = element_blank())


J <- Expt6_StimPulsesPost_DA_AUC_PercBaseline  + 
  theme(legend.position="none") + 
  theme(plot.title = element_blank())


K <- Expt6_StimPulsesPost_DA_Latency_PercBaseline + 
  theme(legend.position="none") + 
  theme(plot.title = element_blank())




layout <- "
AAA
BC#
DEF
GHI
JKL
"
Fig4 <- blank+B1+B2+C+D+E+F1+G+H+I+J+K + plot_annotation(tag_levels = 'A') +
  plot_layout(design = layout)




filename = here("figures", "Fig4.png")
ggsave(filename, Fig4, width =  6, height = 10, units = "in", dpi = 1200)
filename = here("figures", "Fig4.pdf")
ggsave(filename, Fig4, width =  6, height = 10, units = "in")





representativeTraces <- Expt6_aFCV_RepresentativeTrace_Saline +   Expt6_aFCV_RepresentativeTrace_LY
representativeCVs <- Expt6_aFCV_RepresentativeCV_Saline +   Expt6_aFCV_RepresentativeCV_LY

filename = here("figures", "Fig4_Manual_Traces.pdf")
ggsave(filename, representativeTraces, width =  3, height = 1.5, units = "in")

filename = here("figures", "Fig4_Manual_CVs.pdf")
ggsave(filename, representativeCVs, width =  2, height = 1, units = "in")

# Figures exported and modified manually in inkscape




## Supplementary Figure Plotting arrangement ====
SA1 <-  Expt6_StimIntensityPre_DA_max 
SA2 <-  Expt6_StimIntensityPre_DA_AUC 
SA3 <-  Expt6_StimIntensityPre_DA_Latency

SA4 <-  Expt6_StimPulsesPre_DA_max  + 
  theme(plot.title = element_blank())
SA5 <-  Expt6_StimPulsesPre_DA_AUC  + 
  theme(plot.title = element_blank())
SA6 <-  Expt6_StimPulsesPre_DA_Latency + 
  theme(plot.title = element_blank())

SA7 <-  Expt6_StimIntensityPost_DA_max  + 
  theme(plot.title = element_blank())
SA8 <-  Expt6_StimIntensityPost_DA_AUC  + 
  theme(plot.title = element_blank())
SA9 <-  Expt6_StimIntensityPost_DA_Latency + 
  theme(plot.title = element_blank())

SA10 <-  Expt6_StimPulsesPost_DA_max  + 
  theme(plot.title = element_blank())
SA11 <-  Expt6_StimPulsesPost_DA_AUC  + 
  theme(plot.title = element_blank())
SA12 <-  Expt6_StimPulsesPost_DA_Latency + 
  theme(plot.title = element_blank())


# Expt6_StimIntensityPre_DA_max_PercBaseline + Expt6_StimIntensityPre_DA_AUC_PercBaseline + Expt6_StimIntensityPre_DA_Latency_PercBaseline
# Expt6_StimPulsesPre_DA_max_PercBaseline + Expt6_StimPulsesPre_DA_AUC_PercBaseline + Expt6_StimPulsesPre_DA_Latency_PercBaseline

FigS4 <- (SA1 +SA2 +SA3)/(SA4 +SA5 +SA6)/(SA7 +SA8 +SA9)/(SA10+SA11+SA12) + plot_layout(guides = "collect") + plot_annotation(tag_levels = 'A') 
filename = here("figures", "FigS4.pdf")
ggsave(filename, FigS4, width =  6, height = 10, units = "in")
filename = here("figures", "FigS4.png")
ggsave(filename, FigS4, width =  6, height = 10, units = "in", dpi = 1200)
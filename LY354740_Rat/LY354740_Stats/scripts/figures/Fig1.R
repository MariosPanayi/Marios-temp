## Packages for data organisation and plotting
library(tidyverse)
# Package for relative file paths
library(here)
# library(ggpubr)
library(cowplot)
library(ggsignif)
library(patchwork)
library(RColorBrewer)
################################################################################
## Packages for Data analysis
library(afex)
afex_options(emmeans_model = "multivariate")# use multivariate model for all follow-up tests.
library(emmeans)
# install.packages("devtools")
# devtools::install_github("crsh/papaja")
library(papaja)
library(knitr)

################################################################################
## Experiment 1
# reload data
#Load Data
full_data <- read_csv(here("rawdata", "/LY354740_TMaze_Expt1_rawdata.csv")) 
short_data <- full_data %>% 
  filter(Delay == "0")



# 
# # R Brewer colour package
# # Display all colour blind friendly palettes
# display.brewer.all(colorblindFriendly = TRUE)
# # Display a specific palette
# display.brewer.pal(n = 11, name = "RdBu")
# # Display hexadecimal colour code of the palette
# brewer.pal(n = 11, name = "RdBu")
# # Red-Blue Palette 
# "#67001F" "#B2182B" "#D6604D" "#F4A582" "#FDDBC7" "#F7F7F7" "#D1E5F0" "#92C5DE" "#4393C3" "#2166AC" "#053061"
# 
# # Grey Palette
# display.brewer.pal(n = 6, name = "Greys")
# brewer.pal(n = 6, name = "Greys")
# "#F7F7F7" "#D9D9D9" "#BDBDBD" "#969696" "#636363" "#252525"

# # Blue-Purple Palette
# display.brewer.pal(n = 6, name = "Purples")
# brewer.pal(n = 6, name = "Purples")
# "#F2F0F7" "#DADAEB" "#BCBDDC" "#9E9AC8" "#756BB1" "#54278F"


# Re order and rename levels for plotting
short_data$Drug <- fct_relevel(short_data$Drug, c("NoInj", "Veh", "1mgkg", "10mgkg"))
levels <- c("No Inj" = "NoInj", "Veh" = "Veh", "1 mg/kg" = "1mgkg", "10 mg/kg" = "10mgkg")
short_data$Drug <- fct_recode(short_data$Drug, !!!levels)

fillcolours <- c("No Inj" = "#FFFFFF", "Veh" = "#D9D9D9", "1 mg/kg" = "#F4A582" , "10 mg/kg" = "#B2182B")

# Plots Experiment 1
accuracyplot <- short_data %>%   
  ggplot(mapping = aes(x = as.factor(Drug), y = Accuracy, group = Drug,  fill = Drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0))) +
  ggtitle("Accuracy") + xlab("") + ylab("Accuracy") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0.5,1)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values = fillcolours, aesthetics = c("fill")) +
  geom_signif(y_position = 0.85, xmin = "Veh", xmax ="10 mg/kg", annotation = "**", tip_length = c(.01, 0.01), size = .5,  vjust = 0.5 ) +
  geom_signif(y_position = 0.9, xmin = "No Inj" , xmax ="10 mg/kg", annotation = "*", tip_length = c(.01, 0.01), size = .5, vjust = 0.5  )



latencySampleplot <- short_data %>%   
  ggplot(mapping = aes(x = as.factor(Drug), y = Latency_Sample, group = Drug,  fill = Drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0))) +
  ggtitle("Sample") + xlab("") + ylab("Latency (s)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,5)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values = fillcolours, aesthetics = c("fill")) +
  geom_signif(y_position = c(4.65,4.15,3.7),xmin = c("No Inj", "Veh", "1 mg/kg"), xmax = c("10 mg/kg","10 mg/kg","10 mg/kg"), annotation = c("**", "*", "**"), tip_length = c(.01, 0.01), size = .5, vjust = .5)


latencyChoiceplot <- short_data %>%   
  ggplot(mapping = aes(x = as.factor(Drug), y = Latency_Choice, group = Drug,  fill = Drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0))) +
  ggtitle("Choice") + xlab("") + ylab("Latency (s)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,5)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values = fillcolours, aesthetics = c("fill")) +
  geom_signif(y_position = 4, xmin = "No Inj" , xmax ="10 mg/kg", annotation = "*", tip_length = c(.01, 0.01), size = .5, vjust = 0.5  )


# Plots Experiment 2
short_data <- read_csv(here("rawdata", "/LY354740_Amph_TMaze_Expt2_rawdata.csv")) 

# Re order and rename levels for plotting
short_data$Drug <- fct_relevel(short_data$Drug, c("Veh_Veh","Amph_Veh", "Amph_LY"))
levels <- c("Veh/Veh" = "Veh_Veh", "Amph/Veh" = "Amph_Veh", "Amph/LY354740" = "Amph_LY")
short_data$Drug <- fct_recode(short_data$Drug, !!!levels)

fillcolours <- c("Veh/Veh" =  "#FFFFFF", "Amph/Veh" = "#4393C3", "Amph/LY354740" = "#252525")


accuracyplot2 <- short_data %>%   
  ggplot(mapping = aes(x = as.factor(Drug), y = Accuracy, group = Drug,  fill = Drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0))) +
  ggtitle("") + xlab("") + ylab("Accuracy") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0.5,1)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values = fillcolours, aesthetics = c("fill")) +
  geom_signif(y_position = c(.9,.85),xmin = c("Veh/Veh", "Amph/Veh"), xmax = c("Amph/LY354740","Amph/LY354740"), annotation = c("**", "*"), tip_length = c(.01, 0.01), size = .5, vjust = .5)


# ("Veh/Veh" "Amph/Veh" "Amph/LY354740" )
# Veh_Veh_Amph_Veh Veh_Veh - Amph_Veh     0.06 $[-0.03$, $0.14]$      1.64    .249
# Veh_Veh_Amph_LY   Veh_Veh - Amph_LY     0.11  $[0.04$, $0.19]$      4.03    .001
# Amph_Veh_Amph_LY Amph_Veh - Amph_LY     0.06  $[0.01$, $0.11]$      2.82    .024


latencySampleplot2 <- short_data %>%   
  ggplot(mapping = aes(x = as.factor(Drug), y = Latency_Sample, group = Drug,  fill = Drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0))) +
  ggtitle("") + xlab("") + ylab("Latency (s)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,8)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values = fillcolours, aesthetics = c("fill"))  +
  geom_signif(y_position = c(7.6),xmin = c("Veh/Veh"), xmax = c("Amph/LY354740"), annotation = c("**"), tip_length = c(.01, 0.01), size = .5, vjust = .5)

# Veh_Veh_Amph_Veh Veh_Veh - Amph_Veh    -1.78  $[-3.58$, $0.03]$     -2.45    .054
# Veh_Veh_Amph_LY   Veh_Veh - Amph_LY    -3.19 $[-4.69$, $-1.68]$     -5.27  < .001
# Amph_Veh_Amph_LY Amph_Veh - Amph_LY    -1.41  $[-3.68$, $0.87]$     -1.54    .291

# 85 mm; 1.5 column, 114 mm; and 2 column, 174 mm
# column_1 = 3.34
# column_15 = 4.48
# column_2 = 6.85
#height - 9 inches max


latencyChoiceplot2 <- short_data %>%   
  ggplot(mapping = aes(x = as.factor(Drug), y = Latency_Choice, group = Drug,  fill = Drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0))) +
  ggtitle("") + xlab("") + ylab("Latency (s)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,8)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values = fillcolours, aesthetics = c("fill"))  + 
  geom_signif(y_position = c(5.8, 6.4, 5.2),xmin = c("Veh/Veh", "Veh/Veh", "Amph/Veh"), xmax = c("Amph/Veh", "Amph/LY354740","Amph/LY354740"), annotation = c("*", "**","*"), tip_length = c(.01, 0.01), size = .5, vjust = .5)

# Veh_Veh_Amph_Veh Veh_Veh - Amph_Veh    -1.11 $[-1.99$, $-0.24]$     -3.15    .011
# Veh_Veh_Amph_LY   Veh_Veh - Amph_LY    -2.13 $[-2.84$, $-1.42]$     -7.42  < .001
# Amph_Veh_Amph_LY Amph_Veh - Amph_LY    -1.02 $[-1.86$, $-0.18]$     -3.00    .016

# Tweak plot features before combining in a grid
A1 <- accuracyplot + 
  theme(legend.position="none") + 
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())

B1 <- latencySampleplot + 
  theme(legend.position="none") + 
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())

C1 <- latencyChoiceplot + 
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  labs(fill = "LY354740") + 
  theme(legend.title = element_text(size = 8),legend.text = element_text(size = 8),  legend.key.size = unit(.5, "lines"))

D2 <- accuracyplot2 + 
  theme(legend.position="none") + 
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())
E2 <- latencySampleplot2 + 
  theme(legend.position="none") + 
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())
F2 <- latencyChoiceplot2 + 
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  labs(fill = "Drug") + 
  theme(legend.title = element_text(size = 8),legend.text = element_text(size = 8),  legend.key.size = unit(.5, "lines"))


gridplot <- (A1 + B1 + C1)/(D2 + E2 + F2) + plot_annotation(tag_levels = 'A') 

filename = here("figures", "Fig1.png")
ggsave(filename, gridplot, width = 4.48, height = 4, units = "in", dpi = 1200)
filename = here("figures", "Fig1.pdf")
ggsave(filename, gridplot, width = 4.48, height = 4, units = "in")





#####
## Experiment 1 - 40s Supplementary
# reload data
#Load Data
full_data <- read_csv(here("rawdata", "/LY354740_TMaze_Expt1_rawdata.csv")) 
short_data <- full_data %>% 
  filter(Delay == "40")

# Re order and rename levels for plotting
short_data$Drug <- fct_relevel(short_data$Drug, c("NoInj", "Veh", "1mgkg", "10mgkg"))
levels <- c("No Inj" = "NoInj", "Veh" = "Veh", "1 mg/kg" = "1mgkg", "10 mg/kg" = "10mgkg")
short_data$Drug <- fct_recode(short_data$Drug, !!!levels)

fillcolours <- c("No Inj" = "gray90", "Veh" = "steelblue4", "1 mg/kg" = "darksalmon" , "10 mg/kg" = "darkred")

# Plots Experiment 1
accuracyplot <- short_data %>%   
  ggplot(mapping = aes(x = as.factor(Drug), y = Accuracy, group = Drug,  fill = Drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0))) +
  ggtitle("Accuracy") + xlab("") + ylab("Accuracy") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0.5,1)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values = fillcolours, aesthetics = c("fill"))


latencySampleplot <- short_data %>%   
  ggplot(mapping = aes(x = as.factor(Drug), y = Latency_Sample, group = Drug,  fill = Drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0))) +
  ggtitle("Sample") + xlab("") + ylab("Latency (s)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,5)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values = fillcolours, aesthetics = c("fill")) +
  geom_signif(y_position = c(4.65,4.15,3.7),xmin = c("No Inj", "Veh", "1 mg/kg"), xmax = c("10 mg/kg","10 mg/kg","10 mg/kg"), annotation = c("*", "**", "**"), tip_length = c(.01, 0.01), size = .5, vjust = .5)

# contrast estimate                 ci statistic p.value
# NoInj_Veh           NoInj - Veh     0.14  $[-0.18$, $0.46]$      1.22    .622
# NoInj_X1mgkg     NoInj - X1mgkg     0.11  $[-0.15$, $0.36]$      1.14    .669
# NoInj_X10mgkg   NoInj - X10mgkg    -0.43 $[-0.79$, $-0.07]$     -3.29    .014
# Veh_X1mgkg         Veh - X1mgkg    -0.04  $[-0.30$, $0.23]$     -0.37    .982
# Veh_X10mgkg       Veh - X10mgkg    -0.57 $[-0.99$, $-0.16]$     -3.77    .004
# X1mgkg_X10mgkg X1mgkg - X10mgkg    -0.54 $[-0.95$, $-0.12]$     -3.52    .008

latencyChoiceplot <- short_data %>%   
  ggplot(mapping = aes(x = as.factor(Drug), y = Latency_Choice, group = Drug,  fill = Drug)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9), width = 0,  size = .5) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0))) +
  ggtitle("Choice") + xlab("") + ylab("Latency (s)") +
  theme_cowplot(10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,5)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values = fillcolours, aesthetics = c("fill")) +
  geom_signif(y_position = c(4.65,4.15,3.7),xmin = c("No Inj", "Veh", "1 mg/kg"), xmax = c("10 mg/kg","10 mg/kg","10 mg/kg"), annotation = c("**", "**", "**"), tip_length = c(.01, 0.01), size = .5, vjust = .5)

# contrast estimate                 ci statistic p.value
# NoInj_Veh           NoInj - Veh     0.05  $[-0.12$, $0.23]$      0.83    .841
# NoInj_X1mgkg     NoInj - X1mgkg     0.05  $[-0.12$, $0.23]$      0.83    .841
# NoInj_X10mgkg   NoInj - X10mgkg    -0.36 $[-0.64$, $-0.08]$     -3.49    .009
# Veh_X1mgkg         Veh - X1mgkg     0.00  $[-0.19$, $0.19]$      0.00  > .999
# Veh_X10mgkg       Veh - X10mgkg    -0.41 $[-0.69$, $-0.13]$     -3.99    .002
# X1mgkg_X10mgkg X1mgkg - X10mgkg    -0.41 $[-0.71$, $-0.11]$     -3.76    .004



# Tweak plot features before combining in a grid
A1 <- accuracyplot + 
  theme(legend.position="none") + 
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())

B1 <- latencySampleplot + 
  theme(legend.position="none") + 
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())

C1 <- latencyChoiceplot + 
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  labs(fill = "LY354740") + 
  theme(legend.title = element_text(size = 8),legend.text = element_text(size = 8),  legend.key.size = unit(.5, "lines"))

gridplot <- (A1 + B1 + C1) + plot_annotation(tag_levels = 'A')

filename = here("figures", "FigS1.png")
ggsave(filename, gridplot, width = 4.48, height = 4/2, units = "in", dpi = 1200)
filename = here("figures", "FigS1.pdf")
ggsave(filename, gridplot, width = 4.48, height = 4/2, units = "in")

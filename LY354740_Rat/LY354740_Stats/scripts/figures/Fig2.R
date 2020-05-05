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
## Experiment 3
# reload data

full_data <- read_csv(here("rawdata", "/LY354740_Expt3_Locomotor_FoodDep.csv"))

#####
## Data
plot_data1 <- full_data %>% 
  group_by(Subj, Drug, Amph, LY, Period, bin10mins) %>% 
  summarise(activity = sum(activity)) %>%
  ungroup() %>% 
  group_by(Subj) %>% 
  mutate(activity_perc = activity/activity[bin10mins=="0"]) %>% 
  ungroup() %>% 
  filter(bin10mins < 13) %>% 
  mutate(bin10mins = as.factor(bin10mins))

# Re order and rename levels for plotting
plot_data1$Drug <- fct_relevel(plot_data1$Drug, c("Veh_Veh", "Veh_LY", "Veh_Amph", "LY_Amph"))
levels <- c("Veh/Veh" = "Veh_Veh", "Veh/LY354740" = "Veh_LY", "Amph/Veh" = "Veh_Amph", "Amph/LY354740" = "LY_Amph")
plot_data1$Drug <- fct_recode(plot_data1$Drug, !!!levels)

# 
# fillcolours <- c("No Inj" = "#FFFFFF", "Veh" = "#D9D9D9", "1 mg/kg" = "#F4A582" , "10 mg/kg" = "#B2182B")
# fillcolours <- c("Veh/Veh" =  "#FFFFFF", "Amph/Veh" = "#4393C3", "Amph/LY354740" = "#252525")



fillcolours <- c("Veh/Veh" = "#FFFFFF", "Veh/LY354740" = "#FFFFFF", "Amph/Veh"  = "#FFFFFF", "Amph/LY354740" = "#252525")
linecolours <- c("Veh/Veh" = "#000000", "Veh/LY354740" = "#B2182B", "Amph/Veh"  = "#4393C3" , "Amph/LY354740" = "#252525")
Linetypes <- c("Veh/Veh" = "dotted", "Veh/LY354740" = "dotted", "Amph/Veh"  = "solid" , "Amph/LY354740" = "solid")
pointshapes <- c("Veh/Veh" = 21, "Veh/LY354740" = 21, "Amph/Veh"  = 22 , "Amph/LY354740" = 15)
              
highlightarea <- data.frame(x = c(0, 0, 6, 6), y = c(0,1500, 1500, 0 )) 

#Note to plot the polygon first, you need to create a layer with the aes defined in ggplot(). Then when calling the polygon layer you have to specify that it shouldn't inherit the aes from the ggplot command even though different data are specified
  
# Plot for fun
Expt3Locoplot_10mins <- ggplot(data = plot_data1, mapping = aes(x = bin10mins, y = activity, group = Drug, colour = Drug, linetype = Drug, shape = Drug, fill = Drug)) +
  geom_blank() +
  geom_polygon(data=highlightarea, mapping = aes(x = as.numeric(x), y = as.numeric(y)), fill = "gray95", inherit.aes = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0))) +
  ggtitle("Food Restricted") + xlab("10 mins") + ylab("Total beam breaks") +
  theme_cowplot(8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,1500)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "Drug", values = Linetypes)  +  
  scale_colour_manual(name = "Drug", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "Drug", values = pointshapes) +
  scale_fill_manual(name = "Drug", values = fillcolours) +
  theme(legend.key.width=unit(1.5,"line")) +
  geom_signif(y_position = c(1400),xmin = c(12.5), xmax = c(18.5), annotation = c("**"), tip_length = c(.0, .0), size = .5, vjust = .5,linetype = 1, colour = "black")





# Expt 4
full_data <- read_csv(here("rawdata", "/LY354740_Expt4_Locomotor_AdLib.csv"))

plot_data2 <- full_data %>% 
  group_by(Subj, Drug, Amph, LY, Period, bin10mins) %>% 
  summarise(activity = sum(activity)) %>%
  ungroup() %>% 
  group_by(Subj) %>% 
  mutate(activity_perc = activity/activity[bin10mins=="0"]) %>% 
  ungroup() %>% 
  filter(bin10mins < 13) %>% 
  mutate(bin10mins = as.factor(bin10mins))

# Re order and rename levels for plotting
plot_data2$Drug <- fct_relevel(plot_data2$Drug, c("Veh_Veh", "Veh_LY", "Veh_Amph", "LY_Amph"))
levels <- c("Veh/Veh" = "Veh_Veh", "Veh/LY354740" = "Veh_LY", "Amph/Veh" = "Veh_Amph", "Amph/LY354740" = "LY_Amph")
plot_data2$Drug <- fct_recode(plot_data2$Drug, !!!levels)

fillcolours <- c("Veh/Veh" = "#FFFFFF", "Veh/LY354740" = "#FFFFFF", "Amph/Veh"  = "#FFFFFF", "Amph/LY354740" = "#252525")
linecolours <- c("Veh/Veh" = "#000000", "Veh/LY354740" = "#B2182B", "Amph/Veh"  = "#4393C3" , "Amph/LY354740" = "#252525")
Linetypes <- c("Veh/Veh" = "dotted", "Veh/LY354740" = "dotted", "Amph/Veh"  = "solid" , "Amph/LY354740" = "solid")
pointshapes <- c("Veh/Veh" = 21, "Veh/LY354740" = 21, "Amph/Veh"  = 22 , "Amph/LY354740" = 15)

highlightarea <- data.frame(x = c(0, 0, 6, 6), y = c(0,1500, 1500, 0 )) 

#Note to plot the polygon first, you need to create a layer with the aes defined in ggplot(). Then when calling the polygon layer you have to specify that it shouldn't inherit the aes from the ggplot command even though different data are specified

# Plot for fun
Expt4Locoplot_10mins <- ggplot(data = plot_data2, mapping = aes(x = bin10mins, y = activity, group = Drug, colour = Drug, linetype = Drug, shape = Drug, fill = Drug)) +
  geom_blank() +
  geom_polygon(data=highlightarea, mapping = aes(x = as.numeric(x), y = as.numeric(y)), fill = "gray95", inherit.aes = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0))) +
  ggtitle("Ad libitum") + xlab("10 mins") + ylab("Total beam breaks") +
  theme_cowplot(8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,1500)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "Drug", values = Linetypes)  +  
  scale_colour_manual(name = "Drug", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "Drug", values = pointshapes) +
  scale_fill_manual(name = "Drug", values = fillcolours) +
  theme(legend.key.width=unit(1.5,"line")) +
  geom_signif(y_position = c(1400),xmin = c(6.5), xmax = c(12.5), annotation = c("**"), tip_length = c(.0, .0), size = .5, vjust = .5,linetype = 1, colour = "black")




Fig2 <- (Expt3Locoplot_10mins + Expt4Locoplot_10mins) + plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect") 


filename = here("figures", "Fig2.png")
ggsave(filename, Fig2, width = 7.20472, height = 4/2, units = "in", dpi = 1200)
filename = here("figures", "Fig2.pdf")
ggsave(filename, Fig2, width = 7.20472, height = 4/2, units = "in")







# Experiment 5 - Amph Hunger manipulation
full_data <- read_csv(here("rawdata", "/LY354740_Expt5_Locomotor_FoodDepAmphDose.csv"))

#####
## 10 min data
plot_data3 <- full_data %>% 
  group_by(Subj, Feeding, Amph, bin10mins) %>% 
  summarise(activity = sum(activity)) %>%
  ungroup() %>% 
  group_by(Subj) %>% 
  mutate(activity_perc = activity/activity[bin10mins=="0"]) %>% 
  ungroup() %>% 
  filter(bin10mins < 13) %>% 
  mutate(bin10mins = as.factor(bin10mins))

# Re order and rename levels for plotting
plot_data3$Amph <- fct_relevel(as.factor(plot_data3$Amph), c("0", "1", "2.5", "5"))
levels <- c("Veh" = "0", "1.0 mg/kg" = "1", "2.5 mg/kg" = "2.5", "5.0 mg/kg" = "5")
plot_data3$Amph <- fct_recode(plot_data3$Amph, !!!levels)


plot_data4 <- full_data %>%
  filter(bin60mins < 3 &
           bin60mins > 0) %>% 
  group_by(Subj, Feeding, Amph) %>% 
  summarise(activity = sum(activity)) %>%
  ungroup()

# Re order and rename levels for plotting
plot_data4$Amph <- fct_relevel(as.factor(plot_data4$Amph), c("0", "1", "2.5", "5"))
levels <- c("Veh" = "0", "1.0 mg/kg" = "1", "2.5 mg/kg" = "2.5", "5.0 mg/kg" = "5")
plot_data4$Amph <- fct_recode(plot_data4$Amph, !!!levels)

plot_data4$Feeding <- fct_relevel(as.factor(plot_data4$Feeding), c("Ad Lib", "Food Dep"))



# # Display a specific palette
# display.brewer.pal(n = 11, name = "RdBu")
# # Display hexadecimal colour code of the palette
# brewer.pal(n = 11, name = "RdBu")
# # Red-Blue Palette 
# "#67001F" "#B2182B" "#D6604D" "#F4A582" "#FDDBC7" "#F7F7F7" "#D1E5F0" "#92C5DE" "#4393C3" "#2166AC" "#053061"


fillcolours <- c("Veh.Ad Lib" = "#F7F7F7", "1.0 mg/kg.Ad Lib" = "#D1E5F0", "2.5 mg/kg.Ad Lib" = "#4393C3", "5.0 mg/kg.Ad Lib" = "#053061", "Veh.Food Dep" = "#F7F7F7", "1.0 mg/kg.Food Dep" = "#D1E5F0", "2.5 mg/kg.Food Dep" = "#4393C3", "5.0 mg/kg.Food Dep" = "#053061")
linecolours <- c("Veh.Ad Lib" = "#053061", "1.0 mg/kg.Ad Lib" = "#053061", "2.5 mg/kg.Ad Lib" = "#053061", "5.0 mg/kg.Ad Lib" = "#053061", "Veh.Food Dep" = "#053061", "1.0 mg/kg.Food Dep" = "#053061", "2.5 mg/kg.Food Dep" = "#053061", "5.0 mg/kg.Food Dep" = "#053061")
Linetypes <- c("Veh.Ad Lib" = "solid", "1.0 mg/kg.Ad Lib" = "solid", "2.5 mg/kg.Ad Lib" = "solid", "5.0 mg/kg.Ad Lib" = "solid", "Veh.Food Dep" = "dotted", "1.0 mg/kg.Food Dep" = "dotted", "2.5 mg/kg.Food Dep" = "dotted", "5.0 mg/kg.Food Dep" = "dotted")
pointshapes <- c("Veh.Ad Lib" = 21, "1.0 mg/kg.Ad Lib" = 21, "2.5 mg/kg.Ad Lib" = 21, "5.0 mg/kg.Ad Lib" = 21, "Veh.Food Dep" = 22, "1.0 mg/kg.Food Dep" = 22, "2.5 mg/kg.Food Dep" = 22, "5.0 mg/kg.Food Dep" = 22)

highlightarea <- data.frame(x = c(0, 0, 6, 6), y = c(0,6000, 6000, 0 )) 

#Note to plot the polygon first, you need to create a layer with the aes defined in ggplot(). Then when calling the polygon layer you have to specify that it shouldn't inherit the aes from the ggplot command even though different data are specified

# Plot for fun
Expt5Locoplot_10mins <- ggplot(data = plot_data3, mapping = aes(x = bin10mins, y = activity, group = interaction(Amph,Feeding), colour = interaction(Amph,Feeding), linetype = interaction(Amph,Feeding), shape = interaction(Amph,Feeding), fill = interaction(Amph, Feeding))) +
  geom_blank() +
  geom_polygon(data=highlightarea, mapping = aes(x = as.numeric(x), y = as.numeric(y)), fill = "gray95", inherit.aes = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  ggtitle("Amphetamine") + xlab("10 mins") + ylab("Total beam breaks") +
  theme_cowplot(8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,6000)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(strip.background = element_rect(fill=NA )) +
  scale_linetype_manual(name = "", values = Linetypes) + 
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1.5,"line")) 



fillcolours <- c("Ad Lib" = "#FFFFFF", 
                 "Food Dep" = "#4393C3")


linecolours <-  c("Ad Lib" = "#4393C3", 
                  "Food Dep" = "#4393C3")
Linetypes <-  c("Ad Lib" = "solid", 
                "Food Dep" = "solid")

Expt5SumPlot <- plot_data4 %>%   
  ggplot(mapping = aes(x = Amph, y = activity, group =  interaction(Amph,Feeding),  fill = Feeding,  colour = Feeding, linetype = Feeding)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  colour="black", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  colour="black", width = 0,  size = .5, linetype = "solid") +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0))) +
  ggtitle("") + xlab("Amphetamine") + ylab("Total beam breaks \n (120 mins)") +
  theme_cowplot(8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0, 60000)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_linetype_manual(name = "**", values = Linetypes) + 
  scale_colour_manual(name = "**", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "**", values = fillcolours) +
geom_signif(y_position = c(48000, 52000, 56000, 46000),xmin = c("Veh","Veh","Veh","1.0 mg/kg"), xmax = c("1.0 mg/kg","2.5 mg/kg", "5.0 mg/kg","2.5 mg/kg"), annotation = c("**", "**", "**", "**"), tip_length = c(.01, 0.01), size = .5, vjust = .5, colour = "black")



# contrast  estimate                         ci statistic p.value
# 0_1     0 - 1 -1,284.99   $[-2,182.11$, $-387.86]$     -3.77    .002
# 0_25  0 - 2.5 -2,633.72 $[-3,530.85$, $-1,736.59]$     -7.73  < .001
# 0_5     0 - 5 -1,975.52 $[-2,872.65$, $-1,078.40]$     -5.80  < .001
# 1_25  1 - 2.5 -1,348.73   $[-2,178.25$, $-519.21]$     -4.28  < .001
# 1_5     1 - 5   -690.54    $[-1,520.06$, $138.98]$     -2.19    .136
# 25_5  2.5 - 5    658.19    $[-171.33$, $1,487.72]$      2.09    .167




# Experiment 5 - Amph Hunger manipulation - Blood amphetamine levels

full_data <- read_csv(here("rawdata", "/LY354740_Expt5_DBS_FoodDepAmphDose.csv"))


#####
## 1st half data

plot_data5 <- full_data %>% 
  filter(
    Time_hrs < 2
  )

# Re order and rename levels for plotting
plot_data5$Amph <- fct_relevel(as.factor(plot_data5$Amph), c("1", "2.5", "5"))
levels <- c("1.0 mg/kg" = "1", "2.5 mg/kg" = "2.5", "5.0 mg/kg" = "5")
plot_data5$Amph <- fct_recode(plot_data5$Amph, !!!levels)

plot_data5$Feeding <- fct_relevel(as.factor(plot_data5$Feeding), c("Ad Lib", "Food Dep"))


plot_data5$Time_hrs <- fct_relevel(as.factor(plot_data5$Time_hrs), c("0.25", "0.5", "1"))
levels <- c("15" = "0.25", "30" = "0.5", "60" = "1")
plot_data5$Time_hrs <- fct_recode(plot_data5$Time_hrs, !!!levels)


fillcolours <- c("1.0 mg/kg.Ad Lib" = "#D1E5F0", "2.5 mg/kg.Ad Lib" = "#4393C3", "5.0 mg/kg.Ad Lib" = "#053061", "1.0 mg/kg.Food Dep" = "#D1E5F0", "2.5 mg/kg.Food Dep" = "#4393C3", "5.0 mg/kg.Food Dep" = "#053061")
linecolours <- c("1.0 mg/kg.Ad Lib" = "#053061", "2.5 mg/kg.Ad Lib" = "#053061", "5.0 mg/kg.Ad Lib" = "#053061", "1.0 mg/kg.Food Dep" = "#053061", "2.5 mg/kg.Food Dep" = "#053061", "5.0 mg/kg.Food Dep" = "#053061")
Linetypes <- c("1.0 mg/kg.Ad Lib" = "solid", "2.5 mg/kg.Ad Lib" = "solid", "5.0 mg/kg.Ad Lib" = "solid", "1.0 mg/kg.Food Dep" = "dotted", "2.5 mg/kg.Food Dep" = "dotted", "5.0 mg/kg.Food Dep" = "dotted")
pointshapes <- c("1.0 mg/kg.Ad Lib" = 21, "2.5 mg/kg.Ad Lib" = 21, "5.0 mg/kg.Ad Lib" = 21, "1.0 mg/kg.Food Dep" = 22, "2.5 mg/kg.Food Dep" = 22, "5.0 mg/kg.Food Dep" = 22)

highlightarea <- data.frame(x = c(0, 0, 6, 6), y = c(0,6000, 6000, 0 )) 

#Note to plot the polygon first, you need to create a layer with the aes defined in ggplot(). Then when calling the polygon layer you have to specify that it shouldn't inherit the aes from the ggplot command even though different data are specified

# Plot for fun
Expt5DBSplot <- ggplot(data = plot_data5, mapping = aes(x = as.factor(Time_hrs), y = nM, group = interaction(Amph,Feeding), colour = interaction(Amph,Feeding), linetype = interaction(Amph,Feeding), shape = interaction(Amph,Feeding), fill = interaction(Amph, Feeding))) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .5, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  ggtitle("Amphetamine") + xlab("Mins") + ylab("Concentration (nM)") +
  theme_cowplot(8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,8000)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(strip.background = element_rect(fill=NA )) +
  scale_linetype_manual(name = "", values = Linetypes) + 
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1.5,"line")) 





## Supplementary Figure 2


FigS2 <- Expt5Locoplot_10mins / (Expt5SumPlot + Expt5DBSplot) + plot_annotation(tag_levels = 'A') 

filename = here("figures", "FigS2.png")
ggsave(filename, FigS2, width = 5.51181, height = 4, units = "in", dpi = 1200)
filename = here("figures", "FigS2.pdf")
ggsave(filename, FigS2, width = 5.51181, height = 4, units = "in")







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


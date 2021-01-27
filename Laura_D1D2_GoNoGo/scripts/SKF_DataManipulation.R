##### Load relevant packages ----
## Packages for data organisation and plotting
library(tidyverse)
library(knitr)
library(data.table)
library(broom)
# Package for relative file paths
library(here)
# Pretty Plot Stuff
library(ggpubr)
library(cowplot)
library(ggsignif)
library(patchwork)
library(RColorBrewer)

## Packages for Data analysis
library(afex)
afex_options(emmeans_model = "multivariate")# use multivariate model for all follow-up tests.
library(emmeans)

# Packages for markdown 
# install.packages("devtools")
# devtools::install_github("crsh/papaja")
# library(papaja)
# library(knitr)
# remotes::install_github("noamross/redoc")
# library(redoc)

# Packages for parallel computing significantly speeds things up (sometimes)
library(foreach)
library(doParallel)

numCores = 16
registerDoParallel(numCores)


###Get packages needed for GLMM
###install.packages("lme4")
library(lme4)
###install.packages("car")
library(car)
###install.packages("multcomp")
library(multcomp)
###install.packages("emmeans")
library(emmeans)
###install.packages("ggResidpanel")
library(ggResidpanel)


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



fillcolours <- c("Large" = DarkRed,
                 "Small" = LightRed,
                 "Saline" = DarkBlue,
                 "SKF" = LightBlue,
                 "Saline_Large" = MediumBlue,
                 "Saline_Small" = LightBlue,
                 "SKF_Large" = MediumRed,
                 "SKF_Small" = LightRed)

linecolours <- c("Large" = Black,
                 "Small" = Black,
                 "Saline" = Black,
                 "SKF" = Black,
                 "Saline_Large" = Black,
                 "Saline_Small" = Black,
                 "SKF_Large" = Black,
                 "SKF_Small" = Black)

linetypes <- c("Large" = "solid",
               "Small" = "solid",
               "Saline" = "solid",
               "SKF" = "solid",
               "Saline_Large" = "solid",
               "Saline_Small" = "solid",
               "SKF_Large" = "solid",
               "SKF_Small" = "solid")

pointshapes <- c("Large" = circle,
                 "Small" = circle,
                 "Saline" = circle,
                 "SKF" = circle,
                 "Saline_Large" = circle,
                 "Saline_Small" = circle,
                 "SKF_Large" = circle,
                 "SKF_Small" = circle)





# Load Data ---------------------------------------------------------------
datafolder <- "rawdata"
filename <- "SKF_errordata.csv"
rawdata <- fread(here(datafolder,filename))


# Add new columns to data  ------------------------------------------------

rawdata <- rawdata %>% 
  mutate(Prop_NotLevers = 1- Prop_BothLevers,
         Prop_NotLeversOrMag = 1- Prop_LeverOrMag,
         Num_NotLevers = Num_ErrorTialsTotal - Num__BothLevers,
         Num_NotLeverOrMag = Num_ErrorTialsTotal - Num_ErrorTialsLeverOrMag,
         Num_Not_Mag = Num_ErrorTialsTotal - Num__Mag)

# Add congruence of lever to trial type
rawdata <- rawdata %>% 
  mutate(Prop_congruentlever = ifelse(RewardSize == "Small", Prop_Slever, Prop_Llever),
         Prop_incongruentlever = ifelse(RewardSize == "Small", Prop_Llever, Prop_Slever),
         Num_congruentlever = ifelse(RewardSize == "Small", Num__Slever, Num__Llever),
         Num_incongruentlever = ifelse(RewardSize == "Small", Num__Llever, Num__Slever),
         Prop_totalLever_congruentlever = Num_congruentlever/Num__BothLevers,
         Prop_totalLever_incongruentlever = Num_incongruentlever/Num__BothLevers,
         Prop_totalLever_Slever = Num__Slever/Num__BothLevers,
         Prop_totalLever_Llever = Num__Llever/Num__BothLevers,
         Prop_totalLeverMag_Mag = Num__Mag/Num_ErrorTialsLeverOrMag)


# # Run a chi-square for each subject? Nope, massively under-powered!
# 
# chisquaretests_Bothlevers <- rawdata %>% 
#   group_by(SubjID) %>% 
#   summarise(pvalue= chisq.test(Num__BothLevers, Num_NotLevers)$p.value)
# 
# chisquaretests_leverormag

# Quick Plot of data ------------------------------------------------------

# Figure Plotting Function ------------------------------------------------


figure_proportionerrors <- function(rawdata, yvalue, title) {
  
  
  plot <- ggplot(mapping = aes(x = as.factor(RewardSize),y = eval(as.name(yvalue)), group = Condition, colour = Condition, fill = Condition), data = rawdata) +
    stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
    geom_point(aes(group = SubjID), colour = "black" ) +
    geom_line(aes(group = SubjID), colour = "black", alpha = 0.2)+
    facet_wrap(~Drug) +
    # Make Pretty
    scale_y_continuous( expand = expansion(mult = c(0.01, 0.01)), breaks=seq(-1000,1000,0.1)) +
    ggtitle(title) + xlab("Error Trial Reward Value") + ylab("Proportion of error trials") +
    theme_cowplot(11) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(ylim = c(-0.0001,1.0001)) +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_fill_manual(name = "", values = fillcolours) +
    theme(legend.key.width=unit(0.5,"line")) + 
    theme(strip.background = element_rect(fill = "lightgrey"),
          strip.text.x = element_text(face = "bold.italic"))+
    theme(axis.text.x = element_text(face = "bold")) +
    theme(legend.position = "none")
  
  return(plot)
}



# Plot Variable
yvalue1 = "Prop_LeverOrMag"
yvalue2 = "Prop_BothLevers"
yvalue3 = "Prop_Mag"
yvalue4 = "Prop_Llever"
yvalue5 = "Prop_Slever"
yvalue6 = "Prop_congruentlever"
yvalue7 = "Prop_incongruentlever"
yvalue8 = "Prop_totalLever_Llever"
yvalue9 = "Prop_totalLever_Slever"
yvalue10 = "Prop_totalLever_congruentlever"
yvalue11 = "Prop_totalLever_incongruentlever"

# Plot title
title1 = "Both Levers or Magazine/Total Errors"
title2 = "Both Levers/Total Errors"
title3 = "Magazine/Total Errors"
title4 = "Large Lever/Total Errors"
title5 = "Small Lever/Total Errors"
title6 = "Congruent Lever/Total Errors"
title7 = "Incongruent Lever/Total Errors"
title8 = "Large Lever/Lever Errors"
title9 = "Small Lever/Lever Errors"
title10 = "Congruent Lever/Lever Errors"
title11 = "Incongruent Lever/Lever Errors"

# Plot data
Plot_Proportion_visitBothLeversandMag <- figure_proportionerrors(rawdata, yvalue1, title1)
Plot_Proportion_visitBothLevers <- 		figure_proportionerrors(rawdata, yvalue2, title2)
Plot_Proportion_visitMag <- 				figure_proportionerrors(rawdata, yvalue3, title3)
Plot_Proportion_LargeLever_Total <- 		figure_proportionerrors(rawdata, yvalue4, title4)
Plot_Proportion_SmallLever_Total <- 		figure_proportionerrors(rawdata, yvalue5, title5)
Plot_Proportion_Congruent_Total <- 		figure_proportionerrors(rawdata, yvalue6, title6)
Plot_Proportion_InCongruent_Total <- 	figure_proportionerrors(rawdata, yvalue7, title7)
Plot_Proportion_LargeLever_Levers <- 	figure_proportionerrors(rawdata, yvalue8, title8)
Plot_Proportion_SmallLever_Levers <- 	figure_proportionerrors(rawdata, yvalue9, title9)
Plot_Proportion_Congruent_Levers <- 		figure_proportionerrors(rawdata, yvalue10, title10)
Plot_Proportion_InCongruent_Levers <- 	figure_proportionerrors(rawdata, yvalue11, title11)



# # These data are not independent so combining mag and levers not appropriate yet
# Plot_Proportion_visitBothLeversandMag
# Combine relevant plots
Plot_Proportion_visitBothLevers + Plot_Proportion_visitMag
Plot_Proportion_LargeLever_Total + Plot_Proportion_SmallLever_Total + Plot_Proportion_LargeLever_Levers + Plot_Proportion_SmallLever_Levers
Plot_Proportion_Congruent_Total + Plot_Proportion_InCongruent_Total + Plot_Proportion_Congruent_Levers +Plot_Proportion_InCongruent_Levers


# GLMM Binomial -----------------------------------------------------------


# Random effect of Subjects and Drug and Reward Size, but no inetraction because of insufficient data-----------------------------------------------

# Define model - only random effect of Subject
model1 <- glmer(cbind(Num__BothLevers, Num_NotLevers)~ RewardSize*Drug + (1+ RewardSize + Drug |SubjID), data = rawdata, family=binomial)

# Model summary shows actual beta coefficients - interpret with caution!
summary(model1)
# Anova output table to interpret Omnibus main effects/interactions
Anova(model1)

# Plot Residuals and other useful diagnostics about model fit [from "ggResidpanel" package]
resid_panel(model1,  plots = "all")


# SImple effects [if relevant!]
emmeansout<-emmeans(model1,pairwise~RewardSize*Drug, adjust = "tukey")
emmeansout$contrasts

# Random effect of Subjects and Drug -----------------------------------------------

# Define model - only random effect of Subject
model2 <- glmer(cbind(Num__BothLevers, Num_NotLevers)~ RewardSize*Drug + (1 + Drug|SubjID), data = rawdata, family=binomial)

# Model summary shows actual beta coefficients - interpret with caution!
summary(model2)
# Anova output table to interpret Omnibus main effects/interactions
Anova(model2)

# Plot Residuals and other useful diagnostics about model fit [from "ggResidpanel" package]
resid_panel(model2,  plots = "all")


# SImple effects [if relevant!]
emmeansout<-emmeans(model2,pairwise~RewardSize*Drug, adjust = "tukey")
emmeansout$contrasts



# Random effect of Subjects and Drug and Reward Size, but no interaction because of insufficient data-----------------------------------------------
install.packages("optimx")
library(optimx)
# Define model - only random effect of Subject
model3 <- glmer(cbind(Num_Not_Mag,Num__Mag)~ RewardSize*Drug + (1|SubjID), data = rawdata, family=binomial, glmerControl(optimizer="bobyqa"))

# Model summary shows actual beta coefficients - interpret with caution!
summary(model3)
# Anova output table to interpret Omnibus main effects/interactions
Anova(model3)

# Plot Residuals and other useful diagnostics about model fit [from "ggResidpanel" package]
resid_panel(model3,  plots = "all")


# SImple effects [if relevant!]
emmeansout<-emmeans(model3,pairwise~RewardSize*Drug, adjust = "tukey")
emmeansout$contrasts

# Random effect of Subjects and Drug and Reward Size, but no interaction because of insufficient data-----------------------------------------------
# install.packages("optimx")
# library(optimx)
# Define model - only random effect of Subject
model4 <- glmer(cbind(Num__Slever,(Num_ErrorTialsTotal-Num__BothLevers))~ RewardSize*Drug + (1 + RewardSize |SubjID), data = rawdata, family=binomial, glmerControl(optimizer="bobyqa"))

# Model summary shows actual beta coefficients - interpret with caution!
summary(model4)
# Anova output table to interpret Omnibus main effects/interactions
Anova(model4)

# Plot Residuals and other useful diagnostics about model fit [from "ggResidpanel" package]
resid_panel(model4,  plots = "all")


# SImple effects [if relevant!]
emmeansout<-emmeans(model4,pairwise~RewardSize*Drug, adjust = "tukey")
emmeansout$contrasts





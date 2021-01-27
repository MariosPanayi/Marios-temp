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


# Proportions are 

chisquaretests_Bothlevers <- rawdata %>% 
  group_by(SubjID) %>% 
  summarise(pvalue= chisq.test(Num__BothLevers, Num_NotLevers)$p.value)

chisquaretests_leverormag

# Quick Plot of data ------------------------------------------------------

Proportion_visitBothLevers <- rawdata %>% 
  na.omit() %>% 
  ggplot(mapping = aes(x = as.factor(Drug), y = Prop_BothLevers, group = Condition, colour = Condition, fill = Condition)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  geom_point(aes(group = SubjID), colour = "black") +
  geom_line(aes(group = SubjID), colour = "black") +
  # geom_line(aes(group = subject), colour = Black) +
  facet_wrap(~RewardSize,) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,0.1)) +
  ggtitle("Proportion of errors visiting any lever") + xlab("Condition ") + ylab("Proportion of error trials") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-0.0001,1.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  # scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  # scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(0.5,"line")) + 
  # scale_x_discrete(labels = c("P100 - P50\nHigh","P100 - P50\nLow","High - Low\nP100", "High - Low\nP50")) +
  theme(axis.text.x = element_text(face = "bold"))

Proportion_visitBothLevers

Proportion_visitMag <- rawdata %>% 
  na.omit() %>% 
  ggplot(mapping = aes(x = as.factor(Drug), y = Prop_Mag, group = Condition, colour = Condition, fill = Condition)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  geom_point(aes(group = SubjID), colour = "black") +
  geom_line(aes(group = SubjID), colour = "black") +
  # geom_line(aes(group = subject), colour = Black) +
  facet_wrap(~RewardSize,) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-1000,1000,0.1)) +
  ggtitle("Proportion of errors visiting the magazine") + xlab("Condition ") + ylab("Proportion of error trials") +
  theme_cowplot(11) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  coord_cartesian(ylim = c(-0.0001,1.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  # scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  # scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(0.5,"line")) + 
  # scale_x_discrete(labels = c("P100 - P50\nHigh","P100 - P50\nLow","High - Low\nP100", "High - Low\nP50")) +
  theme(axis.text.x = element_text(face = "bold"))

Proportion_visitMag




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




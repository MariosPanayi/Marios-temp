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

# Optimisers for GLMM fitting
library(optimx)
library(dfoptim)


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
# Add congruence of lever to trial type
rawdata <- rawdata %>% 
  mutate(prop_congruentlever = ifelse(RewardSize == "Small", prop_Slever, prop_Llever),
         prop_incongruentlever = ifelse(RewardSize == "Small", prop_Llever, prop_Slever),
         congruentlever = ifelse(RewardSize == "Small", Slever, Llever),
         incongruentlever = ifelse(RewardSize == "Small", Llever, Slever),
         prop_totalLever_congruentlever = congruentlever/BothLevers,
         prop_totalLever_incongruentlever = incongruentlever/BothLevers,
         prop_totalLever_Slever = Slever/BothLevers,
         prop_totalLever_Llever = Llever/BothLevers,
         not_Slever = TotalTrials - Slever,
         not_Llever = TotalTrials - Llever,
         not_Middle = TotalTrials - Middle,
         not_Mag = TotalTrials - Mag,
         not_BothLevers = TotalTrials - BothLevers,
         not_congruentlever = TotalTrials - congruentlever,
         not_incongruentlever = TotalTrials - incongruentlever)

datafolder <- "rawdata"
filename <- "SKF_errordata_clean.csv"
fwrite(rawdata, here(datafolder,filename))


# # Run a chi-square for each subject? Nope, massively under-powered!
# 
# chisquaretests_Bothlevers <- rawdata %>% 
#   group_by(SubjID) %>% 
#   summarise(pvalue= chisq.test(BothLevers, NotLevers)$p.value)
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
yvalue2 = "prop_Bothlevers"
yvalue3 = "prop_Mag"
yvalue4 = "prop_Llever"
yvalue5 = "prop_Slever"
yvalue6 = "prop_congruentlever"
yvalue7 = "prop_incongruentlever"
yvalue8 = "prop_totalLever_Llever"
yvalue9 = "prop_totalLever_Slever"
yvalue10 = "prop_totalLever_congruentlever"
yvalue11 = "prop_totalLever_incongruentlever"

# Plot title
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




Plots_LeversAndMag <- (Plot_Proportion_visitBothLevers + Plot_Proportion_visitMag) + plot_annotation(tag_levels = 'A') + plot_layout(nrow = 1, ncol = 2, widths = c(1, 1))
filename = here("figures", "Plots_LeversAndMag.png")
ggsave(filename, Plots_LeversAndMag, width = 160, height = 120, units = "mm", dpi = 1200)


Plots_LeversLargeVsSmall <- (Plot_Proportion_LargeLever_Total + Plot_Proportion_SmallLever_Total + Plot_Proportion_LargeLever_Levers + Plot_Proportion_SmallLever_Levers) + plot_annotation(tag_levels = 'A') + plot_layout(nrow = 2, ncol = 2, widths = c(1, 1))
filename = here("figures", "Plots_LeversLargeVsSmall.png")
ggsave(filename, Plots_LeversLargeVsSmall, width = 160, height = 240, units = "mm", dpi = 1200)

Plots_CongruentVsIncongruent <- (Plot_Proportion_Congruent_Total + Plot_Proportion_InCongruent_Total + Plot_Proportion_Congruent_Levers +Plot_Proportion_InCongruent_Levers) + plot_annotation(tag_levels = 'A') + plot_layout(nrow = 2, ncol = 2, widths = c(1, 1))
filename = here("figures", "Plots_CongruentVsIncongruent.png")
ggsave(filename, Plots_CongruentVsIncongruent, width = 160, height = 240, units = "mm", dpi = 1200)




# Conclusion -> too many rats with no lever error trials so a lot more missing data when looking at proportions of Lever errors



# GLMM Binomial -----------------------------------------------------------
# Important note on different packages and how they treat testing approach and test statistics for each model
# 1) Using glmer - default contrasts: This leads to contrasts that are treatment coded and are appropriate for Type II models
### - Use of the summary() function will give you estimates of each treatment coded parameter [i.e. one level vs reference level for K-1 predictors]
### - Use of the car::Anova() default: function will give you the Type II omnibus tests (i.e. traditional ANOVA style tests) but since these are Type II tests, 
### you need to know that this is really what you want. This is because John Fox (author of the car package) is a big proponent of Type II as the default ANOVA 
### method. Personally I disagree with this, particularly for experimental designs with factorial structure and a keen interest in interpreting interactions in 
### a particular way. Of course, this is why it is important to know what your tests are actually testing! SOmetime Type II is very appropriate!
### - Use of the car::Anova(model, type = 3): This will give you a Type III approach ANOVA summary table, however!!!! If you have not set your contrasts to sum
### then the main effects will be nonsense! DO not interpret these!
# 2) Use of glmer - "contr.sum" contrasts: Doing this makes it the case that you are running a Type III style ANOVA i.e. it sets things so that the contrasts sum 
### to zero, and model parameters being tested are being tested relative to an intercept that is equal to the Grand Mean!
# - Use of the summary() function will give you estimates of each parameter effect i.e. identical to a Type II ANOVA with sum coding. Not useful to interpret.
### - Use of the car::Anova() default: Again, Type II ANOVA is the default here, so not the point of using sum coding!
### - Use of the car::Anova(model, type = 3): Now you can interpret the Type III tests of the omnibus main effects and interactions in a manner consistent with a 
### "traditional" Type III ANOVA [e.g. the default in SPSS]. This is what most people want, most of the time!
### Summary fo the major conflict among very vocal statisticians about whether to use Type II vs Type III estimation techniques:
### Type III assumes that missing subjects are at random, and therefore gives equal weighting to each grouping/condition/parameter.
### Type II assumes that missing subjects are systematic and therefore the differences are meaningful, therefore it attributes equal weight to each subject i.e. 
### differences in group sizes are represented in parameter estimates. Clearly this is often more desirable in non-experimental design situations.
# 3) Use of afex::mixed() - Uses sum contrast coding by default, and provides Type III tests use with afex::nice() to provide summary table. 
#
# What about the correct type of test? i.e. Wald chi square? Likelihood Ratio? Bootstrapped p-values?
# Here we are limited by the complexity of GLMMs. In LMMs, the option to do F-tests with Satterthwaite/Kenward-Rogers degrees of freedom approximation is great. We
# just don't have that option for GLMMs. So, which of these tests should we use:
# - Wald Chi Square Tests: Reasonable for large data sets, not so great for small data sets (faster to compute so often a default, because it is testing whether the 
#   parameter value in just the full model is equal to zero )
# - Likelihood Ratio Tests: Much better for smaller sample size data than the Wald. Test of whether the full model accounts for more variance than a reduced model with 
#   just that one parameter missing, tetsing whetehr the parameter estimate is above zero.
# - Boot strap P-Values: Also a great option, but can be computationally expensive.
#
# What do the different packages give you in terms of tests?
# summary() and car::Anova() give you Wald chisquare tests. afex::mixed() lets you choose between likelihood ratio tests (method = "LRT" ) and bootstrapped p-values (method = "PB").
# For bootstrapped p-values you need to remember to set the number of samples and it helps a lot to use parallel processing.
# 
# 
# What about fitting the random effects structure? Option 1: Keep it maximal - Barr et al. 2013 "Keep it maximal" Journal of Memory and Language (doi:10.1016/j.jml.2012.11.001) 
# However, consider the rebuttal in this post suggesting that this might under-power low sample sized tests....
# https://vasishth-statistics.blogspot.com/2014/11/should-we-fit-maximal-linear-mixed.html
# 
# Useful GLMM resources: 
# http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html
# https://github.com/singmann/afex
# https://cran.r-project.org/web/packages/afex/vignettes/afex_analysing_accuracy_data.html

# Random effect of Subjects and Drug and Reward Size, but no interaction because of insufficient data-----------------------------------------------
# 
# 
# ### This first section tests standard glmer() analysis approaches
# # Define model - only random effect of Subject
# model1_default<- glmer(cbind(BothLevers, NotLevers)~ RewardSize*Drug + (1|SubjID), data = rawdata ,family=binomial)
# model1_contrSum <- glmer(cbind(BothLevers, NotLevers)~ RewardSize*Drug + (1|SubjID), data = rawdata,  contrasts = list(RewardSize = "contr.sum", RewardSize = "contr.sum") ,family=binomial)
# 
# # Model summary shows actual beta coefficients - interpret with caution!
# summary(model1_default)
# summary(model1_contrSum)
# # Anova output table to interpret Omnibus main effects/interactions
# Anova(model1_default)
# Anova(model1_default,type = 3)
# Anova(model1_contrSum)
# Anova(model1_contrSum,type = 3)
# 
# 
# # Plot Residuals and other useful diagnostics about model fit [from "ggResidpanel" package]
# resid_panel(model1_contrSum,  plots = "all")
# 
# 
# # Simple effects [if relevant!]
# emmeansout<-emmeans(model1_contrSum,pairwise~RewardSize*Drug, adjust = "tukey", type = "response")
# emmeansout$contrasts
# 
# 
# ### Next we use afex::mixed() to help
# 
# library(parallel)
# nc <- detectCores() # number of cores
# cl <- makeCluster(rep("localhost", nc)) # make cluster
# 
# model1_mixed_LRT <- mixed(cbind(BothLevers, NotLevers)~ RewardSize*Drug + (1|SubjID), data = rawdata, family=binomial, method = "LRT", all_fit = TRUE, cl = cl)
# nice(model1_mixed_LRT)
# emmeans_model1_mixed_LRT <- emmeans(model1_mixed_LRT, pairwise~RewardSize*Drug, adjust = "tukey", type = "response")
# emmeans_model1_mixed_LRT$contrasts
# 
# model2_mixed_LRT <- mixed(prop_BothLevers ~ RewardSize*Drug + (1|SubjID), weights = rawdata$ErrorTialsTotal, data = rawdata, family=binomial, method = "LRT", all_fit = TRUE, cl = cl)
# nice(model2_mixed_LRT)
# 


# #  Do not run until you have optimised with the LRT method, this can take a while...
# model1_mixed_PB <- mixed(cbind(BothLevers, NotLevers)~ RewardSize*Drug + (1 |SubjID), data = rawdata, family=binomial, method = "PB", args.test = list(nsim = 1000, cl = cl), all_fit = TRUE, cl = cl)
# nice(model1_mixed_PB)



# Analysis 2: Probability of both levers ----------------------------------
library(parallel)
nc <- detectCores() # number of cores
cl <- makeCluster(rep("localhost", nc)) # make cluster

# Analysis approach - start with maximal random effects model and reduce until fit

model2_bothlevers <- mixed(cbind(BothLevers,not_BothLevers) ~ RewardSize*Drug + (1+RewardSize + Drug|SubjID), 
                           data = rawdata, 
                           family=binomial, 
                           method = "LRT",
                           all_fit = TRUE,
                           cl = cl)


nice(model2_bothlevers)
emmeans_model2_bothlevers <- emmeans(model2_bothlevers, pairwise~RewardSize*Drug, adjust = "tukey", type = "response")
emmeans_model2_bothlevers$contrasts


# Analysis 3: Probability of Mag ----------------------------------
library(parallel)
nc <- detectCores() # number of cores
cl <- makeCluster(rep("localhost", nc)) # make cluster

# Analysis approach - start with maximal random effects model and reduce until fit

model3_Mag <- mixed(cbind(Mag,not_Mag) ~ RewardSize*Drug + ( 1 + RewardSize + Drug|SubjID), 
                           data = rawdata, 
                           family=binomial, 
                           method = "LRT",
                           all_fit = TRUE,
                           cl = cl)


nice(model3_Mag)
emmeans_model3_Mag <- emmeans(model3_Mag, pairwise~RewardSize*Drug, adjust = "tukey", type = "response")
emmeans_model3_Mag$contrasts


# Analysis 4: Probability of Large Lever ----------------------------------
library(parallel)
nc <- detectCores() # number of cores
cl <- makeCluster(rep("localhost", nc)) # make cluster

# Analysis approach - start with maximal random effects model and reduce until fit

model4_Llever <- mixed(cbind(Llever,not_Llever) ~ RewardSize*Drug + ( 1 + RewardSize + Drug|SubjID), 
                    data = rawdata, 
                    family=binomial, 
                    method = "LRT",
                    all_fit = TRUE,
                    cl = cl)


nice(model4_Llever)
emmeans_model4_Llever <- emmeans(model4_Llever, pairwise~RewardSize*Drug, adjust = "tukey", type = "response")
emmeans_model4_Llever$contrasts



# Analysis 5: Probability of Small Lever ----------------------------------
library(parallel)
nc <- detectCores() # number of cores
cl <- makeCluster(rep("localhost", nc)) # make cluster

# Analysis approach - start with maximal random effects model and reduce until fit

model5_Slever <- mixed(cbind(Slever,not_Slever) ~ RewardSize*Drug + ( 1 + RewardSize + Drug|SubjID), 
                       data = rawdata, 
                       family=binomial, 
                       method = "LRT",
                       all_fit = TRUE,
                       cl = cl)


nice(model5_Slever)
emmeans_model5_Slever <- emmeans(model5_Slever, pairwise~RewardSize*Drug, adjust = "tukey", type = "response")
emmeans_model5_Slever$contrasts


# Analysis 6: Probability of COngruent Lever ----------------------------------
library(parallel)
nc <- detectCores() # number of cores
cl <- makeCluster(rep("localhost", nc)) # make cluster

# Analysis approach - start with maximal random effects model and reduce until fit

model6_congruentlever <- mixed(cbind(congruentlever,not_congruentlever) ~ RewardSize*Drug + ( 1 + RewardSize + Drug|SubjID), 
                       data = rawdata, 
                       family=binomial, 
                       method = "LRT",
                       all_fit = TRUE,
                       cl = cl)


nice(model6_congruentlever)
emmeans_model6_congruentlever <- emmeans(model6_congruentlever, pairwise~RewardSize*Drug, adjust = "tukey", type = "response")
emmeans_model6_congruentlever$contrasts


# Analysis 6: Probability of Incongruent Lever ----------------------------------
library(parallel)
nc <- detectCores() # number of cores
cl <- makeCluster(rep("localhost", nc)) # make cluster

# Analysis approach - start with maximal random effects model and reduce until fit

model7_incongruentlever <- mixed(cbind(incongruentlever,not_incongruentlever) ~ RewardSize*Drug + ( 1 + RewardSize + Drug|SubjID), 
                               data = rawdata, 
                               family=binomial, 
                               method = "LRT",
                               all_fit = TRUE,
                               cl = cl)


nice(model7_incongruentlever)
emmeans_model7_incongruentlever <- emmeans(model7_incongruentlever, pairwise~RewardSize*Drug, adjust = "tukey", type = "response")
emmeans_model7_incongruentlever$contrasts

nice(model2_bothlevers)
nice(model3_Mag)
nice(model4_Llever)
nice(model5_Slever)
nice(model6_congruentlever)
nice(model7_incongruentlever)

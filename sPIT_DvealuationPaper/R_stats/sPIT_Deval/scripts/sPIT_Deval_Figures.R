# Title: The effect of Satiety Devaluation on Sensory-Specific Pavlovian-to-Instrumental Transfer
## Author: Marios Panayi
## Date: 16-Nov-2020
## Purpose: Figure plotting
## Notes: Data from SKR214 and SKR216 cohorts
# Load Packagaes ----------------------------------------------------------
## Packages for data organisation and plotting
library(tidyverse)
library(knitr)
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


# Support Functions -------------------------------------------------------
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


fillcolours <- c("Sucrose" = Black,
                 "Maltodextrin" = White, 
                 "Pellet" = DarkGrey,
                 "Non-Devalued" = White,
                 "Devalued" = Black,
                 "General" = DarkGrey,
                 "PreCS" = LightGrey,
                 "Same" = White,
                 "Different" = Black,
                 "LiCl" = Black,
                 "Saline" = White,
                 "Satiety" = MediumGrey)

linecolours <- c("Sucrose" = Black,
                 "Maltodextrin" = Black,
                 "Pellet" = Black,
                 "Non-Devalued" = Black,
                 "Devalued" = Black,
                 "General" = Black,
                 "PreCS" = Black,
                 "Same" = Black,
                 "Different" = Black,
                 "LiCl" = Black,
                 "Saline" = Black,
                 "Satiety" = Black)


linetypes <- c("Sucrose" = "solid",
               "Maltodextrin" = "solid",
               "Pellet" = "dotted",
               "Non-Devalued" = "solid",
               "Devalued" = "solid",
               "General" = "solid",
               "PreCS" = "dotted",
               "Same" = "solid",
               "Different" = "solid",
               "LiCl" = "solid",
               "Saline" = "solid",
               "Satiety" = "solid")


pointshapes <- c("Sucrose" = square,
                 "Maltodextrin" = diamond,
                 "Pellet" = circle,
                 "Non-Devalued" = circle,
                 "Devalued" = circle,
                 "General" = diamond,
                 "PreCS" = triangleDown,
                 "Same" = circle,
                 "Different" = circle,
                 "LiCl" = circle,
                 "Saline" = circle,
                 "Satiety" = circle)


# Expt1. SKR214 - Instrumental Acquisition --------------------------------

# Define rawdata filepath 
data_path <- here("rawdata", "SKR214_Inst_Acquisition.csv")

# Load and organise data 
plot_data <- function(data_path) {
  
  ### 1. Prepare Data
  # Load Data
  full_data <- read_csv(data_path)
  
  # Summarise data into averages per session/day
  summary <- full_data %>% 
    group_by(Experiment, Schedule, Day, Subject, RewardName, LeverName) %>% 
    summarise(LP = sum(`LP Bin`),
              Rewards = sum(`Rewards Bin`),
              SessionLength = median(`Session Length`[`Session Length` != 0]),
              LP_rate_min = (LP/SessionLength) *60,
              MagEntry = sum(`MagEntry Bin`),
              MagEntry_rate_min = (MagEntry/SessionLength) *60,
    ) %>% 
    ungroup()
  
  # Check that there is only a single value per subject/reward/day
  summary %>% 
    group_by(Subject,RewardName, Day) %>% 
    summarise(n = n()) %>% 
    spread(Day, n)%>%
    kable()
  
  ### 2. Fix data Order for plotting
  levelorder <- c("Sucrose", "Maltodextrin")
  summary <- summary %>% 
    mutate(RewardName = str_replace(string = RewardName, pattern = "maltodextrin",replacement = "Maltodextrin"),
      RewardName = factor(RewardName, levels = levelorder)) %>% 
    arrange(RewardName,Day)
  
  # Return Summary Data
return(summary)
}

# Save data for analysis
summary <- plot_data(data_path)
save_path <- here("rawdata", "processed_data", "SKR214_InstrumentalAcquisition.csv")
write_csv(summary, save_path)

# Plot Data
scalelims = c(0,40)
scalelims_breaks = 5
  
  SKR214_Instrumental_acquisition <- summary %>% 
    filter(Day <= 6) %>% 
    ggplot(mapping = aes(x = as.factor(Day), y = LP_rate_min, colour = RewardName, group = RewardName, fill = RewardName, shape = RewardName, linetype = RewardName)) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5, linetype = 1, show.legend = FALSE) +
    stat_summary(fun = "mean", geom = "line", size = 0.5) +
    stat_summary(fun = "mean", geom = "point", size = 3) +
    theme_cowplot() +
    coord_cartesian(ylim = scalelims) + 
    scale_y_continuous(breaks = seq(scalelims[1],scalelims[2],by = scalelims_breaks), expand = c(0.0,0)) +
    # ggtitle("",subtitle = "") + 
    ylab("Lever presses/min") + 
    xlab("Day") +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_linetype_manual(name = "", values = linetypes)+
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_shape_manual(name = "", values = pointshapes) +
    scale_fill_manual(name = "", values = fillcolours)
    
  SKR214_Instrumental_acquisition


# Expt1. SKR214 - Pavlovian Acquisition -----------------------------------

  # Define rawdata filepath 
  data_path <- here("rawdata", "SKR214_Pav_Freq_Acquisition.csv")
  
  # Load and organise data 
  plot_data <- function(data_path) {
    
    ### 1. Prepare Data
    # Load Data
    full_data <- read_csv(data_path)
    
    # Remove unnecessary calculation columns
    full_data <- full_data %>% 
      select(`Day`,                                                    
             `MSN`,                                                    
             `StartDate`,                                              
             `StartTime`,                                              
             `Experiment`,                                             
             `Group`,                                                  
             `Box`,                                                    
             `Subject`,                                                
             `Total MagEntries`,                                       
             `Total MagDuration`,                                      
             `Total Rewards`,                                          
             `Total Session Time`,                                     
             `Trial Number`,                                           
             `ITI_Duration(minus PreCS)`,                              
             `CS_Identity [1 = Noise, 2 = Click, 3 = tone]`,           
             `US_Identity [1 = Sucrose, 2 = Maltodextrin, 3 = Pellet]`,
             `Number of USs`,                                          
             `MagEntry_ITI`,
             `Latency to first magazine Entry`,                        
             `First response was an Exit`,                             
             `MagEntry_PreCS (2mins)`,                                 
             `MagEntry_CS (2 mins)`,                                   
             `CS-PreCS`,                                               
             `Trial Number, 1 CS`,                                     
             `CS_Name`,                                                
             `US_Name`,                                                
             `MagEntriesCS excluding reward delivery 5s`,              
             `CS_Duration (s)_NoReward5s`,                             
             `Devalued Reward 1st test`,                               
             `Devalued?`)
    
    # Summarise data into averages per Outcome ID, rates per trial (1 mins)
    summary <- full_data %>% 
      group_by(Experiment, Day, Subject, CS_Name, US_Name) %>% 
      summarise(AvgRewards = mean(`Number of USs`),
                CS_PreCS = mean(`CS-PreCS`)/2,
                MagEntry_PreCS = mean(`MagEntry_PreCS (2mins)`)/2,
                MagEntry_CS = mean(`MagEntry_CS (2 mins)`)/2
      ) %>% 
      ungroup()
    
    ### 2. Fix data Order for plotting
    levelorder <- c("Sucrose", "Maltodextrin", "Pellet")
    summary <- summary %>% 
      mutate(US_Name = factor(US_Name, levels = levelorder))
    
 
    # Return Summary Data
    return(summary)
  }
  
  # Save data for analysis
  summary <- plot_data(data_path)
  save_path <- here("rawdata", "processed_data", "SKR214_PavlovianAcquisition.csv")
  write_csv(summary, save_path)
  
  # Plot Data
  scalelims = c(0,15)
  scalelims_breaks = 5
  
  SKR214_Pavlovian_acquisition <- summary %>% 
    filter(Day <= 6) %>%
    ggplot(mapping = aes(x = as.factor(Day), y = CS_PreCS, colour = US_Name, group = US_Name, fill = US_Name, shape = US_Name, linetype = US_Name)) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5, linetype = 1, show.legend = FALSE) +
    stat_summary(fun = "mean", geom = "line", size = 0.5) +
    stat_summary(fun = "mean", geom = "point", size = 3) +
    theme_cowplot() +
    coord_cartesian(ylim = scalelims) + 
    scale_y_continuous(breaks = seq(scalelims[1],scalelims[2],by = scalelims_breaks), expand = c(0.0,0)) +
    # ggtitle("",subtitle = "") + 
    ylab("CS-PreCS Entries/min") + 
    xlab("Day") +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_linetype_manual(name = "", values = linetypes)+
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_shape_manual(name = "", values = pointshapes) +
    scale_fill_manual(name = "", values = fillcolours)
  
  SKR214_Pavlovian_acquisition


# SKR214_Satiety_consumption ----------------------------------------------


  # Define rawdata filepath 
  data_path <- here("rawdata", "SKR214_Satiety_consumption.csv")
  
  # Load and organise data 
  plot_data <- function(data_path) {
    
    ### 1. Prepare Data
    # Load Data
    full_data <- read_csv(data_path)
    
    # Check that there are equal numbers of entries per subject/day
    full_data %>% 
      group_by(Rat, TestNumber) %>% 
      summarise(n = n()) %>% 
      spread(TestNumber, n)%>%
      kable()
    
    summary <- full_data
    
    ### 2. Fix data Order for plotting
    levelorder <- c("Sucrose", "Maltodextrin", "Pellet")
    summary <- summary %>% 
      mutate(Liquid = factor(Liquid, levels = levelorder))
    
    
    # Return Summary Data
    return(summary)
  }
  
  # Save data for analysis
  summary <- plot_data(data_path)
  save_path <- here("rawdata", "processed_data", "SKR214_SatietyConsumption.csv")
  write_csv(summary, save_path)
  
  # Plot Data
  scalelims = c(0,24)
  scalelims_breaks = 2
  

  SKR214_Satiety_Consumption <- summary %>% ggplot(mapping = aes(x = Liquid, y = `Consumed (g)`, colour = Liquid, group = Liquid, fill = Liquid)) +
    stat_summary(fun = "mean", geom = "bar", size = 1, position = position_dodge(width = .9)) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.2, position = position_dodge(width = 0.9), colour = "black", linetype = 1, show.legend = FALSE) +
    theme_cowplot() +
    coord_cartesian(ylim = scalelims) + 
    scale_y_continuous(breaks = seq(scalelims[1],scalelims[2],by = scalelims_breaks), expand = c(0.0,0)) +
    # ggtitle("",subtitle = "") + 
    ylab("Consumed (g)") + 
    xlab("") +
    # theme(axis.title.x=element_blank())+
    theme(axis.text.x = element_blank()) +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_fill_manual(name = "", values = fillcolours)
  
  SKR214_Satiety_Consumption

  
  
  # Expt1. SKR214 - Satiety Instrumental Devaluation  ----------------------------------------------  

  # Define rawdata filepath 
  data_path <- here("rawdata", "SKR214_sPIT_Test.csv")
  
  # Load and organise data 
  plot_data <- function(data_path) {
    
    ### 1. Prepare Data
    # Load Data
    full_data <- read_csv(data_path)
    
    summary <- full_data %>% 
      filter(`Trial Structure` == "Baseline Extinction",
             Devaluation == "Satiety") %>% 
      group_by(Devaluation, Subject, `Minute Counter`) %>% 
      summarise(`Devalued Lever` = mean(`Devalued Lever`)*60,
                `Non-Devalued Lever` = mean(`Non-Devalued Lever`)*60,
                MagEntries = mean(MagEntries)*60,
                # MagDuration = mean(MagDuration)*60 # MagDuration not recorded in these data
      ) %>% 
      ungroup()
    
    
    summary_LP <- summary %>% 
      select(Subject, `Minute Counter`, `Devalued Lever`, `Non-Devalued Lever`) %>% 
      gather(key = "Devaluation", value = "lever Presses", `Devalued Lever`, `Non-Devalued Lever`)
    
    ### 2. Fix data Order for plotting
    levelorder <- c("Non-Devalued", "Devalued")
    summary_LP <- summary_LP %>% 
      mutate(Devaluation = str_replace(string = Devaluation, pattern = "Non-Devalued Lever",replacement = "Non-Devalued"),
             Devaluation = str_replace(string = Devaluation, pattern = "Devalued Lever",replacement = "Devalued"),
             Devaluation = factor(Devaluation, levels = levelorder))
      
    
    
    # Check that there are equal numbers of entries per subject/day
    summary_LP %>% 
      group_by(Subject) %>% 
      summarise(n = n()) %>% 
      spread(Subject, n)%>%
      kable()
    
 return(summary_LP)   
    
  }

  
  # Save data for analysis
  summary <- plot_data(data_path)
  save_path <- here("rawdata", "processed_data", "SKR214_SatietyDevalTest.csv")
  write_csv(summary, save_path)
  
  # Plot Data
  scalelims = c(0,14)
  scalelims_breaks = 2
  
  SKR214_Satiety_DevalTest <- summary %>% ggplot(mapping = aes(x = as.factor(`Minute Counter`), y = `lever Presses`, colour = Devaluation, group = Devaluation, fill = Devaluation, shape = Devaluation, linetype = Devaluation)) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5, linetype = 1, show.legend = FALSE) +
    stat_summary(fun = "mean", geom = "line", size = 0.5) +
    stat_summary(fun = "mean", geom = "point", size = 3) +
    theme_cowplot() +
    coord_cartesian(ylim = scalelims) + 
    scale_y_continuous(breaks = seq(scalelims[1],scalelims[2],by = scalelims_breaks), expand = c(0.0,0)) +
    # ggtitle("",subtitle = "") + 
    ylab("Lever presses/min") + 
    xlab("1 minute bins") +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_linetype_manual(name = "", values = linetypes)+
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_shape_manual(name = "", values = pointshapes) +
    scale_fill_manual(name = "", values = fillcolours)
  
  SKR214_Satiety_DevalTest
  
  # Expt1. SKR214 - Satiety Instrumental Devaluation DevalIndex  ----------------------------------------------  
  
  # Define rawdata filepath 
  data_path <- here("rawdata", "SKR214_sPIT_Test.csv")
  
  # Load and organise data 
  plot_data <- function(data_path) {
    
    ### 1. Prepare Data
    # Load Data
    full_data <- read_csv(data_path)
    
    summary <- full_data %>% 
      filter(`Trial Structure` == "Baseline Extinction",
             Devaluation == "Satiety",
             `Minute Counter` <= 4) %>% 
      group_by(Devaluation, Subject) %>% 
      summarise(`Devalued Lever` = mean(`Devalued Lever`)*60,
                `Non-Devalued Lever` = mean(`Non-Devalued Lever`)*60,
                MagEntries = mean(MagEntries)*60,
                # MagDuration = mean(MagDuration)*60 # MagDuration not recorded in these data
      ) %>% 
      ungroup() %>% 
      mutate( Deval_index = `Non-Devalued Lever` - `Devalued Lever`)
    
    
    
    
    
    # Check that there are equal numbers of entries per subject/day
    summary %>% 
      group_by(Subject) %>% 
      summarise(n = n()) %>% 
      spread(Subject, n)%>%
      kable()
    
    return(summary)   
    
  }
  
  
  # Save data for analysis
  summary <- plot_data(data_path)
  save_path <- here("rawdata", "processed_data", "SKR214_SatietyDevalTest_DevalIndex.csv")
  write_csv(summary, save_path)
  
  # Plot Data
  scalelims = c(0,14)
  scalelims_breaks = 2
  
  SKR214_Satiety_DevalTest_Index <- summary %>% ggplot(mapping = aes(x = Devaluation, y = Deval_index, colour = Devaluation, group = Devaluation, fill = Devaluation)) +
    # stat_summary(fun = "mean", geom = "bar", size = 1, position = position_dodge(width = .9)) +
    # stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.2, position = position_dodge(width = 0.9), colour = "black", linetype = 1, show.legend = FALSE) +
    geom_violin(show.legend = FALSE)+
    geom_point(show.legend = FALSE)+
    theme_cowplot() +
    coord_cartesian(ylim = scalelims) + 
    scale_y_continuous(breaks = seq(scalelims[1],scalelims[2],by = scalelims_breaks), expand = c(0.0,0)) +
    # ggtitle("",subtitle = "") + 
    ylab("Non-Devalued - Devalued \n Lever presses/min") + 
    xlab("Devaluation") +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_fill_manual(name = "", values = fillcolours)
  
  SKR214_Satiety_DevalTest_Index
  
  

  
  
  # Expt1. SKR214 - Satiety sPIT Test  ----------------------------------------------  
  
  # Define rawdata filepath 
  data_path <- here("rawdata", "SKR214_sPIT_Test.csv")
  
  # Load and organise data 
  plot_data <- function(data_path) {
    
    ### 1. Prepare Data
    # Load Data
    full_data <- read_csv(data_path)
    
    summary <- full_data %>% 
      filter(
        `Trial Structure` == "PreCS" | `Trial Structure` == "CS",
         # `trial period split minutes [1,2]` == "PreCS2"|`trial period split minutes [1,2]` == "CS1",
        # `trial period split minutes [1,2]` == "PreCS2"|`trial period split minutes [1,2]` == "CS2",
        # `trial period split minutes [1,2]` == "PreCS1" | `trial period split minutes [1,2]` == "PreCS2"|`trial period split minutes [1,2]` == "CS1"|`trial period split minutes [1,2]` == "CS2",
        # `Trial Number` == 1|`Trial Number` == 2,
        Devaluation == "Satiety"
      ) %>% 
      group_by(Devaluation, Group, Session, Subject, `Trial Structure`, `Devalued Pavlovian US?`) %>% 
      summarise(`Devalued Lever` = mean(`Devalued Lever`)*60,
                `Non-Devalued Lever` = mean(`Non-Devalued Lever`)*60,
                MagEntries = mean(MagEntries)*60,
                # MagDuration = mean(MagDuration)*60 # MagDuration not recorded in these data
      ) %>% 
      ungroup()
    

    
    summary_LP <- summary %>% 
      select(-MagEntries, -Devaluation) %>% 
      pivot_longer( cols =  `Devalued Lever`:`Non-Devalued Lever`, names_to = "Devaluation", values_to = "lever Presses") 

    
    summary_LP_wide <- summary_LP %>% 
      pivot_wider(names_from = `Trial Structure`, values_from = `lever Presses`) 
    
    summary_LP_PreCSonly <- summary_LP_wide %>% 
      select(-CS) %>% 
      group_by(Group, Session, Subject, Devaluation) %>% 
      summarise(PreCS = mean(PreCS)) %>%
      ungroup()
    
    summary_LP_PreCSonly <- summary_LP_PreCSonly %>% 
      pivot_longer(cols = PreCS, names_to = "Trial Structure", values_to = "lever Presses") %>% 
      mutate("Devalued Pavlovian US?" = "PreCS")
    
    summary_LP_commonPreCS <- summary_LP %>% 
      filter(`Trial Structure` == "CS") %>% 
      full_join(summary_LP_PreCSonly)
    
    summary_LP_commonPreCS <- summary_LP_commonPreCS %>% 
      unite("same_diff",c("Devalued Pavlovian US?", "Devaluation"), sep = "_", remove = FALSE) %>% 
      mutate(same_diff = str_replace(string = same_diff, pattern = "Non-Devalued_Non-Devalued Lever",replacement = "Same"),
             same_diff = str_replace(string = same_diff, pattern = "Devalued_Non-Devalued Lever",replacement = "Different"),
             same_diff = str_replace(string = same_diff, pattern = "Non-Devalued_Devalued Lever",replacement = "Different"),
             same_diff = str_replace(string = same_diff, pattern = "Devalued_Devalued Lever",replacement = "Same"),
             same_diff = str_replace(string = same_diff, pattern = "General_Devalued Lever",replacement = "General"),
             same_diff = str_replace(string = same_diff, pattern = "General_Non-Devalued Lever",replacement = "General"),
             same_diff = str_replace(string = same_diff, pattern = "PreCS_Devalued Lever",replacement = "PreCS"),
             same_diff = str_replace(string = same_diff, pattern = "PreCS_Non-Devalued Lever",replacement = "PreCS"))
    
    
    # Average across repeated session
    summary_LP_commonPreCS <- summary_LP_commonPreCS %>% 
      group_by(Subject, Devaluation,  `Trial Structure`, same_diff, `Devalued Pavlovian US?`) %>% 
      summarise(`lever Presses` = mean(`lever Presses`)) %>% 
      ungroup()
    
    
    levelorder <- c("Non-Devalued", "Devalued")
    levelorderCS <- c("PreCS", "Non-Devalued", "Devalued", "General")
    leverlorderCS2 <- c("PreCS", "Same", "Different", "General")
    summary_LP_commonPreCS <- summary_LP_commonPreCS %>% 
      mutate(Devaluation = str_replace(string = Devaluation, pattern = "Non-Devalued Lever",replacement = "Non-Devalued"),
             Devaluation = str_replace(string = Devaluation, pattern = "Devalued Lever",replacement = "Devalued"),
             Devaluation = factor(Devaluation, levels = levelorder),
             `Devalued Pavlovian US?` = factor(`Devalued Pavlovian US?`, levels = levelorderCS),
             same_diff = factor(same_diff, levels = leverlorderCS2))
    
    
    return(summary_LP_commonPreCS)
    
  }
  
  
  
  # Save data for analysis
  summary <- plot_data(data_path)
  save_path <- here("rawdata", "processed_data", "SKR214_SatietysPITTest.csv")
  write_csv(summary, save_path)
  
  # Plot Data
  scalelims = c(0,6)
  scalelims_breaks = 1
  
  # SKR214_Satiety_sPITTest_splitDeval <- summary %>% ggplot(mapping = aes(x = Devaluation, y = `lever Presses`, colour = `Devalued Pavlovian US?`, group = `Devalued Pavlovian US?`, fill = `Devalued Pavlovian US?`)) +
  #   stat_summary(fun = "mean", geom = "bar", size = 1, position = position_dodge(width = .9)) +
  #   stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.2, position = position_dodge(width = 0.9), colour = "black", linetype = 1, show.legend = FALSE) +
  #   theme_cowplot() +
  #   coord_cartesian(ylim = scalelims) + 
  #   scale_y_continuous(breaks = seq(scalelims[1],scalelims[2],by = scalelims_breaks), expand = c(0.0,0)) +
  #   # ggtitle("",subtitle = "") + 
  #   ylab("Lever presses/min") + 
  #   xlab("Lever") +
  #   theme(axis.title.x=element_text(face = "bold")) +
  #   scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  #   scale_fill_manual(name = "", values = fillcolours)
  # 
  # SKR214_Satiety_sPITTest_splitDeval
  
  
  SKR214_Satiety_sPITTest_splitSameDiff <- summary %>% ggplot(mapping = aes(x = Devaluation, y = `lever Presses`, colour = same_diff, group = same_diff, fill = same_diff)) +
    stat_summary(fun = "mean", geom = "bar", size = 1, position = position_dodge(width = .9)) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.2, position = position_dodge(width = 0.9), colour = "black", linetype = 1, show.legend = FALSE) +
    theme_cowplot() +
    coord_cartesian(ylim = scalelims) + 
    scale_y_continuous(breaks = seq(scalelims[1],scalelims[2],by = scalelims_breaks), expand = c(0.0,0)) +
    # ggtitle("",subtitle = "") + 
    ylab("Lever presses/min") + 
    xlab("Lever") +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_fill_manual(name = "", values = fillcolours)
  
  SKR214_Satiety_sPITTest_splitSameDiff
  
  # Expt1. SKR214 - Satiety sPIT Test - sPIT Index  ----------------------------------------------  
  
  # Define rawdata filepath 
  data_path <- here("rawdata", "SKR214_sPIT_Test.csv")
  
  # Load and organise data 
  plot_data <- function(data_path) {
    
    ### 1. Prepare Data
    # Load Data
    full_data <- read_csv(data_path)
    
    summary <- full_data %>% 
      filter(
        `Trial Structure` == "PreCS" | `Trial Structure` == "CS",
        # `trial period split minutes [1,2]` == "PreCS2"|`trial period split minutes [1,2]` == "CS1",
        # `trial period split minutes [1,2]` == "PreCS2"|`trial period split minutes [1,2]` == "CS2",
        # `trial period split minutes [1,2]` == "PreCS1" | `trial period split minutes [1,2]` == "PreCS2"|`trial period split minutes [1,2]` == "CS1"|`trial period split minutes [1,2]` == "CS2",
        # `Trial Number` == 1|`Trial Number` == 2,
        Devaluation == "Satiety"
      ) %>% 
      group_by(Devaluation, Group, Session, Subject, `Trial Structure`, `Devalued Pavlovian US?`) %>% 
      summarise(`Devalued Lever` = mean(`Devalued Lever`)*60,
                `Non-Devalued Lever` = mean(`Non-Devalued Lever`)*60,
                MagEntries = mean(MagEntries)*60,
                # MagDuration = mean(MagDuration)*60 # MagDuration not recorded in these data
      ) %>% 
      ungroup()
    
    
    
    summary_LP <- summary %>% 
      select(-MagEntries, -Devaluation) %>% 
      pivot_longer( cols =  `Devalued Lever`:`Non-Devalued Lever`, names_to = "Devaluation", values_to = "lever Presses") 
    
    
    summary_LP_wide <- summary_LP %>% 
      pivot_wider(names_from = `Trial Structure`, values_from = `lever Presses`) 
    
    summary_LP_PreCSonly <- summary_LP_wide %>% 
      select(-CS) %>% 
      group_by(Group, Session, Subject, Devaluation) %>% 
      summarise(PreCS = mean(PreCS)) %>%
      ungroup()
    
    summary_LP_PreCSonly <- summary_LP_PreCSonly %>% 
      pivot_longer(cols = PreCS, names_to = "Trial Structure", values_to = "lever Presses") %>% 
      mutate("Devalued Pavlovian US?" = "PreCS")
    
    summary_LP_commonPreCS <- summary_LP %>% 
      filter(`Trial Structure` == "CS") %>% 
      full_join(summary_LP_PreCSonly)
    
    summary_LP_commonPreCS <- summary_LP_commonPreCS %>% 
      unite("same_diff",c("Devalued Pavlovian US?", "Devaluation"), sep = "_", remove = FALSE) %>% 
      mutate(same_diff = str_replace(string = same_diff, pattern = "Non-Devalued_Non-Devalued Lever",replacement = "Same"),
             same_diff = str_replace(string = same_diff, pattern = "Devalued_Non-Devalued Lever",replacement = "Different"),
             same_diff = str_replace(string = same_diff, pattern = "Non-Devalued_Devalued Lever",replacement = "Different"),
             same_diff = str_replace(string = same_diff, pattern = "Devalued_Devalued Lever",replacement = "Same"),
             same_diff = str_replace(string = same_diff, pattern = "General_Devalued Lever",replacement = "General"),
             same_diff = str_replace(string = same_diff, pattern = "General_Non-Devalued Lever",replacement = "General"),
             same_diff = str_replace(string = same_diff, pattern = "PreCS_Devalued Lever",replacement = "PreCS"),
             same_diff = str_replace(string = same_diff, pattern = "PreCS_Non-Devalued Lever",replacement = "PreCS"))
    
    
    # Average across repeated session
    summary_LP_commonPreCS <- summary_LP_commonPreCS %>% 
      group_by(Subject, Devaluation,  `Trial Structure`, same_diff, `Devalued Pavlovian US?`) %>% 
      summarise(`lever Presses` = mean(`lever Presses`)) %>% 
      ungroup()
    
    
    levelorder <- c("Non-Devalued", "Devalued")
    levelorderCS <- c("PreCS", "Non-Devalued", "Devalued", "General")
    leverlorderCS2 <- c("PreCS", "Same", "Different", "General")
    summary_LP_commonPreCS <- summary_LP_commonPreCS %>% 
      mutate(Devaluation = str_replace(string = Devaluation, pattern = "Non-Devalued Lever",replacement = "Non-Devalued"),
             Devaluation = str_replace(string = Devaluation, pattern = "Devalued Lever",replacement = "Devalued"),
             Devaluation = factor(Devaluation, levels = levelorder),
             `Devalued Pavlovian US?` = factor(`Devalued Pavlovian US?`, levels = levelorderCS),
             same_diff = factor(same_diff, levels = leverlorderCS2))
    
    
    summary_LP_sPITindex <- summary_LP_commonPreCS %>% 
      select(-`Devalued Pavlovian US?`) %>%
      filter(`Trial Structure` != "PreCS") %>% 
      pivot_wider(names_from = same_diff, values_from = `lever Presses`) %>% 
      mutate(same_vs_diff = Same - Different,
             same_bias = Same /(Same + Different)) %>% 
      pivot_wider(names_from = Devaluation, values_from = Different:same_bias) %>% 
      mutate(Same_NDev_vs_Dev = `Same_Non-Devalued` - Same_Devalued,
             Diff_NDev_vs_Dev = `Different_Non-Devalued` - Different_Devalued,
             General_NDev_vs_Dev = `General_Non-Devalued` - General_Devalued)
    
    
    devalindex <- read_csv(here("rawdata", "processed_data", "SKR214_SatietyDevalTest_DevalIndex.csv")) %>% 
      select(Subject, `Devalued Lever`, `Non-Devalued Lever`, Deval_index)
    
    
    summary_LP_sPITindex <- summary_LP_sPITindex %>% 
      full_join(devalindex)
    
    
    return(summary_LP_sPITindex)
    
  }
  
  
  
  # Save data for analysis
  summary <- plot_data(data_path)
  save_path <- here("rawdata", "processed_data", "SKR214_SatietysPITTest_sPITIndex.csv")
  write_csv(summary, save_path)
  
  # Plot Data
  # scalelims = c(-1,1)
  # scalelims_breaks = .2
  # 

  SKR214_Satiety_sPITTest_sPITIndex <- summary %>% ggplot(mapping = aes(x = Deval_index, y = `same_vs_diff_Non-Devalued`)) +
    # stat_summary(fun = "mean", geom = "bar", size = 1, position = position_dodge(width = .9)) +
    # stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.2, position = position_dodge(width = 0.9), colour = "black", linetype = 1, show.legend = FALSE) +
    geom_point() +
    theme_cowplot() +
    # coord_cartesian(ylim = scalelims) + 
    # scale_y_continuous(breaks = seq(scalelims[1],scalelims[2],by = scalelims_breaks), expand = c(0.0,0)) +
    # ggtitle("",subtitle = "") + 
    ylab("same_vs_diff_Devalued") + 
    xlab("Deval_index") +
    theme(axis.title.x=element_text(face = "bold")) 

  
  SKR214_Satiety_sPITTest_sPITIndex
  
  # Expt1. SKR214 - Satiety sPIT Test Magazine Entry ----------------------------------------------  
  
  # Define rawdata filepath 
  data_path <- here("rawdata", "SKR214_sPIT_Test.csv")
  
  # Load and organise data 
  plot_data <- function(data_path) {
    
    ### 1. Prepare Data
    # Load Data
    full_data <- read_csv(data_path)
    
    summary <- full_data %>% 
      filter(
        `Trial Structure` == "PreCS" | `Trial Structure` == "CS",
        # `trial period split minutes [1,2]` == "PreCS2"|`trial period split minutes [1,2]` == "CS1",
        # `trial period split minutes [1,2]` == "PreCS2"|`trial period split minutes [1,2]` == "CS2",
        # `trial period split minutes [1,2]` == "PreCS1" | `trial period split minutes [1,2]` == "PreCS2"|`trial period split minutes [1,2]` == "CS1"|`trial period split minutes [1,2]` == "CS2",
        # `Trial Number` == 1|`Trial Number` == 2,
        Devaluation == "Satiety"
      ) %>% 
      group_by(Devaluation, Group, Session, Subject, `Trial Structure`, `Devalued Pavlovian US?`) %>% 
      summarise(`Devalued Lever` = mean(`Devalued Lever`)*60,
                `Non-Devalued Lever` = mean(`Non-Devalued Lever`)*60,
                MagEntries = mean(MagEntries)*60,
                # MagDuration = mean(MagDuration)*60 # MagDuration not recorded in these data
      ) %>% 
      ungroup()
    
    
    
    summary_Mag <- summary %>% 
      select(-`Devalued Lever`, - `Non-Devalued Lever`, -Devaluation)
    
    
    summary_Mag_wide <- summary_Mag %>% 
      pivot_wider(names_from = `Trial Structure`, values_from = MagEntries) 
    
    summary_Mag_PreCSonly <- summary_Mag_wide %>% 
      select(-CS) %>% 
      group_by(Group, Session, Subject) %>% 
      summarise(PreCS = mean(PreCS)) %>%
      ungroup()
    
    summary_Mag_PreCSonly <- summary_Mag_PreCSonly %>% 
      pivot_longer(cols = PreCS, names_to = "Trial Structure", values_to = "MagEntries") %>% 
      mutate("Devalued Pavlovian US?" = "PreCS")
    
    summary_Mag_commonPreCS <- summary_Mag %>% 
      filter(`Trial Structure` == "CS") %>% 
      full_join(summary_Mag_PreCSonly)
    

    # Average across repeated session
    summary_Mag_commonPreCS <- summary_Mag_commonPreCS %>% 
      group_by(Subject,  `Trial Structure`, `Devalued Pavlovian US?`) %>% 
      summarise(MagEntries = mean(MagEntries)) %>% 
      ungroup()
    
    
    levelorder <- c("Non-Devalued", "Devalued")
    levelorderCS <- c("PreCS", "Non-Devalued", "Devalued", "General")
    leverlorderCS2 <- c("PreCS", "Same", "Different", "General")
    summary_Mag_commonPreCS <- summary_Mag_commonPreCS %>% 
      mutate(`Devalued Pavlovian US?` = factor(`Devalued Pavlovian US?`, levels = levelorderCS)) %>% 
      rename(Devaluation = `Devalued Pavlovian US?`)
    
    
    return(summary_Mag_commonPreCS)
    
  }
  
  
  
  # Save data for analysis
  summary <- plot_data(data_path)
  save_path <- here("rawdata", "processed_data", "SKR214_SatietysPITTest_MagEntries.csv")
  write_csv(summary, save_path)
  
  # Plot Data
  scalelims = c(0,8)
  scalelims_breaks = 2

  SKR214_Satiety_sPITTest_splitMagEntries <- summary %>% ggplot(mapping = aes(x = Devaluation, y = MagEntries, colour = Devaluation, group = Devaluation, fill = Devaluation)) +
    stat_summary(fun = "mean", geom = "bar", size = 1, position = position_dodge(width = .9)) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.2, position = position_dodge(width = 0.9), colour = "black", linetype = 1, show.legend = FALSE) +
    theme_cowplot() +
    coord_cartesian(ylim = scalelims) + 
    scale_y_continuous(breaks = seq(scalelims[1],scalelims[2],by = scalelims_breaks), expand = c(0.0,0)) +
    # ggtitle("",subtitle = "") + 
    ylab("MagEntries/min") + 
    xlab("CS") +
    theme(axis.title.x=element_text(face = "bold")) +
    scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    scale_fill_manual(name = "", values = fillcolours)
  
  SKR214_Satiety_sPITTest_splitMagEntries

#   # Expt1. SKR214 - LiCl Consumption  ----------------------------------------------  
#   
#   # Define rawdata filepath 
#   data_path <- here("rawdata", "SKR214_LiCl_TasteAversion.csv")
#   
#   # Load and organise data 
#   plot_data <- function(data_path) {
#     
#     ### 1. Prepare Data
#     # Load Data
#     full_data <- read_csv(data_path)
#     
#     levelorder <- c("Saline", "LiCl")
#     full_data <- full_data %>% 
#       mutate(Injection = factor(Injection, levels = levelorder))
#     
#     
#     # Check that there are equal numbers of entries per subject/day
#     full_data %>%
#       group_by(Rat, Pairing, Injection) %>%
#       summarise(n = n()) %>%
#       spread(Rat, n)%>%
#       kable()
#     
#     return(full_data)
# 
#   }
#   
#   # Save data for analysis
#   summary <- plot_data(data_path)
#   save_path <- here("rawdata", "processed_data", "SKR214_LiClConsumption.csv")
#   write_csv(summary, save_path)
#   
#   # Plot Data
#   scalelims = c(0,22)
#   scalelims_breaks = 2
# 
#   
#     SKR214_LiCl_Consumption <- summary %>% 
#     ggplot(mapping = aes(x = as.factor(Pairing), y = `Consumed (g)`, colour = Injection, group = Injection, fill = Injection, shape = Injection, linetype = Injection)) +
#     stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5, linetype = 1, show.legend = FALSE) +
#     stat_summary(fun = "mean", geom = "line", size = 0.5) +
#     stat_summary(fun = "mean", geom = "point", size = 3) +
#     theme_cowplot() +
#     coord_cartesian(ylim = scalelims) + 
#     scale_y_continuous(breaks = seq(scalelims[1],scalelims[2],by = scalelims_breaks), expand = c(0.0,0)) +
#     # ggtitle("",subtitle = "") + 
#     ylab("Consumed (g)") + 
#     xlab("Pairing") +
#     theme(axis.title.x=element_text(face = "bold")) +
#     scale_linetype_manual(name = "", values = linetypes)+
#     scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
#     scale_shape_manual(name = "", values = pointshapes) +
#     scale_fill_manual(name = "", values = fillcolours)
#   
#     SKR214_LiCl_Consumption
#   
# # Expt1. SKR214 - LiCl in Magazine  ----------------------------------------------  
#     
#     # Define rawdata filepath 
#     data_path <- here("rawdata", "SKR214_DevaluationinMag_LiCl.csv")
#     
#     # Load and organise data 
#     plot_data <- function(data_path) {
#       
#       ### 1. Prepare Data
#       # Load Data
#       full_data <- read_csv(data_path)
#       
#       summary <- full_data %>% 
#         mutate(Liquid = case_when(str_detect(MSN,"sucrose") ~ "Sucrose",
#                                   str_detect(MSN,"Maltodextrin") ~ "Maltodextrin")
#         ) %>% 
#         select(Injection, Liquid, Subject, Bin_4mins, Mag1_5sBins, Mag1_duration_5sbins) %>% 
#         group_by(Injection, Subject, Liquid, Bin_4mins) %>% 
#         summarise(Bin_4mins = median(Bin_4mins),
#                   MagEntry = sum(Mag1_5sBins)/4,
#                   MagDur = sum(Mag1_duration_5sbins)/100/4) %>% 
#         ungroup() %>% 
#         filter(Bin_4mins < 7)
#       
#       levelorder <- c("Saline", "LiCl")
#       summary <- summary %>% 
#         mutate(Injection = factor(Injection, levels = levelorder))
#       
#       return(summary)
#       
#     }   
#     
#     
#     # Save data for analysis
#     summary <- plot_data(data_path)
#     save_path <- here("rawdata", "processed_data", "SKR214_LiCl_DevaluationinMag.csv")
#     write_csv(summary, save_path)
#     
#     # Plot Data
#     scalelims = c(0,8)
#     scalelims_breaks = 2
#     
#     
#     SKR214_LiCl_DevalinMag <- summary %>% 
#       ggplot(mapping = aes(x = as.factor(Bin_4mins), y = MagEntry, colour = Injection, group = Injection, fill = Injection, shape = Injection, linetype = Injection)) +
#       stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5, linetype = 1, show.legend = FALSE) +
#       stat_summary(fun = "mean", geom = "line", size = 0.5) +
#       stat_summary(fun = "mean", geom = "point", size = 3) +
#       theme_cowplot() +
#       coord_cartesian(ylim = scalelims) + 
#       scale_y_continuous(breaks = seq(scalelims[1],scalelims[2],by = scalelims_breaks), expand = c(0.0,0)) +
#       # ggtitle("",subtitle = "") + 
#       ylab("Magazine Entries/Min") + 
#       xlab("4 minute bins") +
#       theme(axis.title.x=element_text(face = "bold")) +
#       scale_linetype_manual(name = "", values = linetypes)+
#       scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
#       scale_shape_manual(name = "", values = pointshapes) +
#       scale_fill_manual(name = "", values = fillcolours)
#     
#     SKR214_LiCl_DevalinMag
#     
#     # Expt1. SKR214 - LiCl Instrumental Devaluation  ----------------------------------------------  
#     
#     # Define rawdata filepath 
#     data_path <- here("rawdata", "SKR214_sPIT_Test.csv")
#     
#     # Load and organise data 
#     plot_data <- function(data_path) {
#       
#       ### 1. Prepare Data
#       # Load Data
#       full_data <- read_csv(data_path)
#       
#       summary <- full_data %>% 
#         filter(`Trial Structure` == "Baseline Extinction",
#                Devaluation == "LiCl") %>% 
#         group_by(Devaluation, Subject, `Minute Counter`) %>% 
#         summarise(`Devalued Lever` = mean(`Devalued Lever`)*60,
#                   `Non-Devalued Lever` = mean(`Non-Devalued Lever`)*60,
#                   MagEntries = mean(MagEntries)*60,
#                   # MagDuration = mean(MagDuration)*60 # MagDuration not recorded in these data
#         ) %>% 
#         ungroup()
#       
#       
#       summary_LP <- summary %>% 
#         select(Subject, `Minute Counter`, `Devalued Lever`, `Non-Devalued Lever`) %>% 
#         gather(key = "Devaluation", value = "lever Presses", `Devalued Lever`, `Non-Devalued Lever`)
#       
#       ### 2. Fix data Order for plotting
#       levelorder <- c("Non-Devalued", "Devalued")
#       summary_LP <- summary_LP %>% 
#         mutate(Devaluation = str_replace(string = Devaluation, pattern = "Non-Devalued Lever",replacement = "Non-Devalued"),
#                Devaluation = str_replace(string = Devaluation, pattern = "Devalued Lever",replacement = "Devalued"),
#                Devaluation = factor(Devaluation, levels = levelorder))
#       
#       
#       
#       # Check that there are equal numbers of entries per subject/day
#       summary_LP %>% 
#         group_by(Subject) %>% 
#         summarise(n = n()) %>% 
#         spread(Subject, n)%>%
#         kable()
#       
#       return(summary_LP)   
#       
#     }
#     
#     
#     # Save data for analysis
#     summary <- plot_data(data_path)
#     save_path <- here("rawdata", "processed_data", "SKR214_LiClDevalTest.csv")
#     write_csv(summary, save_path)
#     
#     # Plot Data
#     scalelims = c(0,30)
#     scalelims_breaks = 5
#     
#     SKR214_LiCl_DevalTest <- summary %>% ggplot(mapping = aes(x = as.factor(`Minute Counter`), y = `lever Presses`, colour = Devaluation, group = Devaluation, fill = Devaluation, shape = Devaluation, linetype = Devaluation)) +
#       stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5, linetype = 1, show.legend = FALSE) +
#       stat_summary(fun = "mean", geom = "line", size = 0.5) +
#       stat_summary(fun = "mean", geom = "point", size = 3) +
#       theme_cowplot() +
#       coord_cartesian(ylim = scalelims) + 
#       scale_y_continuous(breaks = seq(scalelims[1],scalelims[2],by = scalelims_breaks), expand = c(0.0,0)) +
#       # ggtitle("",subtitle = "") + 
#       ylab("Lever presses/min") + 
#       xlab("1 minute bins") +
#       theme(axis.title.x=element_text(face = "bold")) +
#       scale_linetype_manual(name = "", values = linetypes)+
#       scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
#       scale_shape_manual(name = "", values = pointshapes) +
#       scale_fill_manual(name = "", values = fillcolours)
#     
#     SKR214_LiCl_DevalTest
#     
#     
#     # Expt1. SKR214 - LiCl sPIT Test  ----------------------------------------------  
#     
#     # Define rawdata filepath 
#     data_path <- here("rawdata", "SKR214_sPIT_Test.csv")
#     
#     # Load and organise data 
#     plot_data <- function(data_path) {
#       
#       ### 1. Prepare Data
#       # Load Data
#       full_data <- read_csv(data_path)
#       
#       summary <- full_data %>% 
#         filter(
#           `Trial Structure` == "PreCS" | `Trial Structure` == "CS",
#           # `trial period split minutes [1,2]` == "PreCS2"|`trial period split minutes [1,2]` == "CS1",
#           # `trial period split minutes [1,2]` == "PreCS1" | `trial period split minutes [1,2]` == "PreCS2"|`trial period split minutes [1,2]` == "CS1"|`trial period split minutes [1,2]` == "CS2",
#           # `Trial Number` == 1|`Trial Number` == 2,
#           Devaluation == "LiCl"
#         ) %>% 
#         group_by(Devaluation, Group, Session, Subject, `Trial Structure`, `Devalued Pavlovian US?`) %>% 
#         summarise(`Devalued Lever` = mean(`Devalued Lever`)*60,
#                   `Non-Devalued Lever` = mean(`Non-Devalued Lever`)*60,
#                   MagEntries = mean(MagEntries)*60,
#                   # MagDuration = mean(MagDuration)*60 # MagDuration not recorded in these data
#         ) %>% 
#         ungroup()
#       
#       
#       
#       summary_LP <- summary %>% 
#         select(-MagEntries, -Devaluation) %>% 
#         pivot_longer( cols =  `Devalued Lever`:`Non-Devalued Lever`, names_to = "Devaluation", values_to = "lever Presses") 
#       
#       
#       summary_LP_wide <- summary_LP %>% 
#         pivot_wider(names_from = `Trial Structure`, values_from = `lever Presses`) 
#       
#       summary_LP_PreCSonly <- summary_LP_wide %>% 
#         select(-CS) %>% 
#         group_by(Group, Session, Subject, Devaluation) %>% 
#         summarise(PreCS = mean(PreCS)) %>%
#         ungroup()
#       
#       summary_LP_PreCSonly <- summary_LP_PreCSonly %>% 
#         pivot_longer(cols = PreCS, names_to = "Trial Structure", values_to = "lever Presses") %>% 
#         mutate("Devalued Pavlovian US?" = "PreCS")
#       
#       summary_LP_commonPreCS <- summary_LP %>% 
#         filter(`Trial Structure` == "CS") %>% 
#         full_join(summary_LP_PreCSonly)
#       
#       summary_LP_commonPreCS <- summary_LP_commonPreCS %>% 
#         unite("same_diff",c("Devalued Pavlovian US?", "Devaluation"), sep = "_", remove = FALSE) %>% 
#         mutate(same_diff = str_replace(string = same_diff, pattern = "Non-Devalued_Non-Devalued Lever",replacement = "Same"),
#                same_diff = str_replace(string = same_diff, pattern = "Devalued_Non-Devalued Lever",replacement = "Different"),
#                same_diff = str_replace(string = same_diff, pattern = "Non-Devalued_Devalued Lever",replacement = "Different"),
#                same_diff = str_replace(string = same_diff, pattern = "Devalued_Devalued Lever",replacement = "Same"),
#                same_diff = str_replace(string = same_diff, pattern = "General_Devalued Lever",replacement = "General"),
#                same_diff = str_replace(string = same_diff, pattern = "General_Non-Devalued Lever",replacement = "General"),
#                same_diff = str_replace(string = same_diff, pattern = "PreCS_Devalued Lever",replacement = "PreCS"),
#                same_diff = str_replace(string = same_diff, pattern = "PreCS_Non-Devalued Lever",replacement = "PreCS"))
#       
#       
#       # Average across repeated session
#       summary_LP_commonPreCS <- summary_LP_commonPreCS %>% 
#         group_by(Subject, Devaluation,  `Trial Structure`, same_diff, `Devalued Pavlovian US?`) %>% 
#         summarise(`lever Presses` = mean(`lever Presses`)) %>% 
#         ungroup()
#       
#       
#       levelorder <- c("Non-Devalued", "Devalued")
#       levelorderCS <- c("PreCS", "Non-Devalued", "Devalued", "General")
#       leverlorderCS2 <- c("PreCS", "Same", "Different", "General")
#       summary_LP_commonPreCS <- summary_LP_commonPreCS %>% 
#         mutate(Devaluation = str_replace(string = Devaluation, pattern = "Non-Devalued Lever",replacement = "Non-Devalued"),
#                Devaluation = str_replace(string = Devaluation, pattern = "Devalued Lever",replacement = "Devalued"),
#                Devaluation = factor(Devaluation, levels = levelorder),
#                `Devalued Pavlovian US?` = factor(`Devalued Pavlovian US?`, levels = levelorderCS),
#                same_diff = factor(same_diff, levels = leverlorderCS2))
#       
#       
#       return(summary_LP_commonPreCS)
#       
#     }
#     
#     
#     
#     # Save data for analysis
#     summary <- plot_data(data_path)
#     save_path <- here("rawdata", "processed_data", "SKR214_LiClsPITTest.csv")
#     write_csv(summary, save_path)
#     
#     # Plot Data
#     scalelims = c(0,8)
#     scalelims_breaks = 1
#     
# 
#     SKR214_LiCl_sPITTest_splitSameDiff <- summary %>% ggplot(mapping = aes(x = Devaluation, y = `lever Presses`, colour = same_diff, group = same_diff, fill = same_diff)) +
#       stat_summary(fun = "mean", geom = "bar", size = 1, position = position_dodge(width = .9)) +
#       stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.2, position = position_dodge(width = 0.9), colour = "black", linetype = 1, show.legend = FALSE) +
#       theme_cowplot() +
#       coord_cartesian(ylim = scalelims) + 
#       scale_y_continuous(breaks = seq(scalelims[1],scalelims[2],by = scalelims_breaks), expand = c(0.0,0)) +
#       # ggtitle("",subtitle = "") + 
#       ylab("Lever presses/min") + 
#       xlab("Lever") +
#       theme(axis.title.x=element_text(face = "bold")) +
#       scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
#       scale_fill_manual(name = "", values = fillcolours)
#     
#     SKR214_LiCl_sPITTest_splitSameDiff
    

# SKR216 SINGLE LEVER EXPERIMENTS 1. Satiety -----------------------------------------
    # Expt2. SKR216 Satiety - Instrumental Acquisition --------------------------------
    
    # Define rawdata filepath 
    data_path <- here("rawdata", "SKR216_Inst_Acquisition.csv")
    
    # Load and organise data 
    plot_data <- function(data_path) {
      
      ### 1. Prepare Data
      # Load Data
      full_data <- read_csv(data_path)
      
      # Summarise data into averages per session/day
      summary <- full_data %>% 
        group_by(Experiment,Experiment_Devaluation, Schedule, Day, Subject, RewardName, LeverName) %>% 
        summarise(LP = sum(`LP Bin`),
                  Rewards = sum(`Rewards Bin`),
                  SessionLength = median(`Session Length`[`Session Length` != 0]),
                  LP_rate_min = (LP/SessionLength) *60,
                  MagEntry = sum(`MagEntry Bin`),
                  MagEntry_rate_min = (MagEntry/SessionLength) *60,
        ) %>% 
        ungroup()

      # Check that there is only a single value per subject/reward/day
      summary %>% 
        group_by(Subject,RewardName, Day) %>% 
        summarise(n = n()) %>% 
        spread(Day, n)%>%
        kable()
      
      ### 2. Fix data Order for plotting
      levelorder <- c("Sucrose", "Maltodextrin")
      summary <- summary %>% 
        mutate(RewardName = str_replace(string = RewardName, pattern = "maltodextrin",replacement = "Maltodextrin"),
               RewardName = factor(RewardName, levels = levelorder)) %>% 
        arrange(RewardName,Day)
      
      ### Filter relevant Group
      summary <- summary %>% 
        filter(Experiment_Devaluation == "Satiety")
      
      # Return Summary Data
      return(summary)
    }
    
    # Save data for analysis
    summary <- plot_data(data_path)
    save_path <- here("rawdata", "processed_data", "SKR216_Satiety_InstrumentalAcquisition.csv")
    write_csv(summary, save_path)
    
    # Plot Data
    scalelims = c(0,35)
    scalelims_breaks = 5
    
    SKR216_Satiety_Instrumental_acquisition <- summary %>% 
      filter(Day <= 6) %>% 
      ggplot(mapping = aes(x = as.factor(Day), y = LP_rate_min, colour = RewardName, group = RewardName, fill = RewardName, shape = RewardName, linetype = RewardName)) +
      stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5, linetype = 1, show.legend = FALSE) +
      stat_summary(fun = "mean", geom = "line", size = 0.5) +
      stat_summary(fun = "mean", geom = "point", size = 3) +
      theme_cowplot() +
      coord_cartesian(ylim = scalelims) + 
      scale_y_continuous(breaks = seq(scalelims[1],scalelims[2],by = scalelims_breaks), expand = c(0.0,0)) +
      # ggtitle("",subtitle = "") + 
      ylab("Lever presses/min") + 
      xlab("Day") +
      theme(axis.title.x=element_text(face = "bold")) +
      scale_linetype_manual(name = "", values = linetypes)+
      scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
      scale_shape_manual(name = "", values = pointshapes) +
      scale_fill_manual(name = "", values = fillcolours)
    
    SKR216_Satiety_Instrumental_acquisition

    

# Expt2. SKR216 Satiety - Pavlovian Acquisition -----------------------------------------------------------------------

    # Define rawdata filepath 
    data_path <- here("rawdata", "SKR216_Pav_Freq_Acquisition.csv")
    
    # Load and organise data 
    plot_data <- function(data_path) {
      
      ### 1. Prepare Data
      # Load Data
      full_data <- read_csv(data_path)
      
      # Check that there are equal numbers of entries per subject/day
      full_data %>% 
        group_by(Subject, Day) %>% 
        summarise(n = n()) %>% 
        spread(Day, n)%>%
        kable()
      
      
      full_data <- full_data %>% 
        mutate(Experiment_Devaluation = case_when(Subject < 9 ~ "Taste Aversion",
                                                  Subject > 8 ~ "Satiety") )
      
      # Remove unnecessary calculation columns
      full_data <- full_data %>% 
        filter(Experiment_Devaluation == "Satiety") %>% 
        select(Experiment_Devaluation,
               `Day`,                                                    
               `MSN`,                                                    
               `StartDate`,                                              
               `StartTime`,                                              
               `Experiment`,                                             
               `Group`,                                                  
               `Box`,                                                    
               `Subject`,                                                
               `Total MagEntries`,                                       
               `Total MagDuration`,                                      
               `Total Rewards`,                                          
               `Total Session Time`,                                     
               `Trial Number`,                                           
               `ITI_Duration(minus PreCS)`,                              
               `CS_Identity [1 = Noise, 2 = Click, 3 = tone]`,           
               `US_Identity [1 = Sucrose, 2 = Maltodextrin, 3 = Pellet]`,
               `Number of USs`,                                          
               `MagEntry_ITI`,
               `Latency to first magazine Entry`,                        
               `First response was an Exit`,                             
               `MagEntry_PreCS (2mins)`,                                 
               `MagEntry_CS (2 mins)`,                                   
               `CS-PreCS`,                                               
               `Trial Number, 1 CS`,                                     
               `CS_Name`,                                                
               `US_Name`,                                                
               `MagEntriesCS excluding reward delivery 5s`,              
               `CS_Duration (s)_NoReward5s`,                             
               `Devalued Reward 1st test`,                               
               `Devalued?`)
      
      # Summarise data into averages per Outcome ID, rates per trial (1 mins)
      summary <- full_data %>% 
        group_by(Experiment, Day, Subject, CS_Name, US_Name) %>% 
        summarise(AvgRewards = mean(`Number of USs`),
                  CS_PreCS = mean(`CS-PreCS`)/2,
                  MagEntry_PreCS = mean(`MagEntry_PreCS (2mins)`)/2,
                  MagEntry_CS = mean(`MagEntry_CS (2 mins)`)/2
        ) %>% 
        ungroup()
      
      # Check that there are equal numbers of trial types/subject
      summary %>% 
        group_by(Subject, Day) %>% 
        summarise(n = n()) %>% 
        spread(Day, n)%>%
        kable()
      
      ### 2. Fix data Order for plotting
      levelorder <- c("Sucrose", "Maltodextrin", "Pellet")
      summary <- summary %>% 
        mutate(US_Name = factor(US_Name, levels = levelorder))
      
      
      # Return Summary Data
      return(summary)
    }
    
    # Save data for analysis
    summary <- plot_data(data_path)
    save_path <- here("rawdata", "processed_data", "SKR216_Satiety_PavlovianAcquisition.csv")
    write_csv(summary, save_path)
    
    # Plot Data
    scalelims = c(0,15)
    scalelims_breaks = 5
    
    SKR216_Satiety_Pavlovian_acquisition <- summary %>% 
      filter(Day <= 6) %>%
      # filter(Subject != 8) %>%
      filter(Subject != 29 & Subject != 31) %>% 
      ggplot(mapping = aes(x = as.factor(Day), y = CS_PreCS, colour = US_Name, group = US_Name, fill = US_Name, shape = US_Name, linetype = US_Name)) +
      stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5, linetype = 1, show.legend = FALSE) +
      stat_summary(fun = "mean", geom = "line", size = 0.5) +
      stat_summary(fun = "mean", geom = "point", size = 3) +
      theme_cowplot() +
      coord_cartesian(ylim = scalelims) + 
      scale_y_continuous(breaks = seq(scalelims[1],scalelims[2],by = scalelims_breaks), expand = c(0.0,0)) +
      # ggtitle("",subtitle = "") + 
      ylab("CS-PreCS Entries/min") + 
      xlab("Day") +
      theme(axis.title.x=element_text(face = "bold")) +
      scale_linetype_manual(name = "", values = linetypes)+
      scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
      scale_shape_manual(name = "", values = pointshapes) +
      scale_fill_manual(name = "", values = fillcolours)
    
    SKR216_Satiety_Pavlovian_acquisition


# Expt2. SKR216 Satiety - Consumption -----------------------------------------------------------------------   
    # Define rawdata filepath 
    data_path <- here("rawdata", "SKR216_Satiety_consumption.csv")
    
    # Load and organise data 
    plot_data <- function(data_path) {
      
      ### 1. Prepare Data
      # Load Data
      full_data <- read_csv(data_path)
      
      # Check that there are equal numbers of entries per subject/day
      full_data %>% 
        group_by(Rat, SatietyTestNum) %>% 
        summarise(n = n()) %>% 
        spread(SatietyTestNum, n)%>%
        kable()
      
      summary <- full_data
      
      ### 2. Fix data Order for plotting
      levelorder <- c("Sucrose", "Maltodextrin", "Pellet")
      summary <- summary %>% 
        rename(Liquid = Devaluation,
               `Consumed (g)` = `consumed(g)`) %>% 
        mutate(Liquid = factor(Liquid, levels = levelorder))
      
      
      # Return Summary Data
      return(summary)
    }
    
    # Save data for analysis
    summary <- plot_data(data_path)
    save_path <- here("rawdata", "processed_data", "SKR216_SatietyConsumption.csv")
    write_csv(summary, save_path)
    
    # Plot Data
    scalelims = c(0,26)
    scalelims_breaks = 2
    
    
    SKR216_Satiety_Consumption <- summary %>%
      # filter(Rat != 8) %>%
      filter(Rat != 29 & Rat != 31) %>%
      ggplot(mapping = aes(x = Liquid, y = `Consumed (g)`, colour = Liquid, group = Liquid, fill = Liquid)) +
      stat_summary(fun = "mean", geom = "bar", size = 1, position = position_dodge(width = .9)) +
      stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.2, position = position_dodge(width = 0.9), colour = "black", linetype = 1, show.legend = FALSE) +
      theme_cowplot() +
      coord_cartesian(ylim = scalelims) + 
      scale_y_continuous(breaks = seq(scalelims[1],scalelims[2],by = scalelims_breaks), expand = c(0.0,0)) +
      # ggtitle("",subtitle = "") + 
      ylab("Consumed (g)") + 
      xlab("") +
      # theme(axis.title.x=element_blank())+
      theme(axis.text.x = element_blank()) +
      scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
      scale_fill_manual(name = "", values = fillcolours)
    
    SKR216_Satiety_Consumption

# break -------------------------------------------------------------------
    # Expt2. SKR216 - Satiety Instrumental Devaluation  ----------------------------------------------  
    
    # Define rawdata filepath 
    data_path <- here("rawdata", "SKR216_sPIT_Test.csv")
    
    # Load and organise data 
    plot_data <- function(data_path) {
      
      ### 1. Prepare Data
      # Load Data
      full_data <- read_csv(data_path)
      
      summary <- full_data %>% 
        filter(`Trial Structure` == "Baseline Extinction",
               Devaluation == "Satiety") %>% 
        group_by(Subject,`1Lever_DevaluedLever?`, `Minute Counter`) %>% 
        summarise(LP = mean(`1 Lever Test_Lever Pressing`)*60,
                  MagEntries = mean(MagEntries)*60,
                  MagDuration = mean(MagDuration)*60
        ) %>% 
        ungroup() %>% 
        mutate(Devaluation = `1Lever_DevaluedLever?`) %>% 
        rename(`lever Presses` = LP)

      
      ### 2. Fix data Order for plotting
      levelorder <- c("Non-Devalued", "Devalued")
      summary_LP <- summary%>% 
        mutate(Devaluation = factor(Devaluation, levels = levelorder))
      
      
      
      # Check that there are equal numbers of entries per subject/day
      summary_LP %>% 
        group_by(Subject) %>% 
        summarise(n = n()) %>% 
        spread(Subject, n)%>%
        kable()
      
      return(summary_LP)   
      
    }
    
    
    # Save data for analysis
    summary <- plot_data(data_path)
    save_path <- here("rawdata", "processed_data", "SKR216_SatietyDevalTest.csv")
    write_csv(summary, save_path)
    
    # Plot Data
    scalelims = c(0,20)
    scalelims_breaks = 2
    
    SKR216_Satiety_DevalTest <- summary %>% 
    # filter(Subject != 8) %>%
    filter(Subject  != 29 & Subject  != 31) %>% ggplot(mapping = aes(x = as.factor(`Minute Counter`), y = `lever Presses`, colour = Devaluation, group = Devaluation, fill = Devaluation, shape = Devaluation, linetype = Devaluation)) +
      stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5, linetype = 1, show.legend = FALSE) +
      stat_summary(fun = "mean", geom = "line", size = 0.5) +
      stat_summary(fun = "mean", geom = "point", size = 3) +
      theme_cowplot() +
      coord_cartesian(ylim = scalelims) + 
      scale_y_continuous(breaks = seq(scalelims[1],scalelims[2],by = scalelims_breaks), expand = c(0.0,0)) +
      # ggtitle("",subtitle = "") + 
      ylab("Lever presses/min") + 
      xlab("1 minute bins") +
      theme(axis.title.x=element_text(face = "bold")) +
      scale_linetype_manual(name = "", values = linetypes)+
      scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
      scale_shape_manual(name = "", values = pointshapes) +
      scale_fill_manual(name = "", values = fillcolours)
    
    SKR216_Satiety_DevalTest
    
    # Expt2. SKR216 - Satiety Instrumental Devaluation Devalindex  ----------------------------------------------  
    
    # Define rawdata filepath 
    data_path <- here("rawdata", "SKR216_sPIT_Test.csv")
    
    # Load and organise data 
    plot_data <- function(data_path) {
      
      ### 1. Prepare Data
      # Load Data
      full_data <- read_csv(data_path)
      
      summary <- full_data %>% 
        filter(`Trial Structure` == "Baseline Extinction",
               Devaluation == "Satiety",
               `Minute Counter` <= 4) %>% 
        group_by( Subject,`1Lever_DevaluedLever?`) %>% 
        summarise(LP = mean(`1 Lever Test_Lever Pressing`)*60,
                  MagEntries = mean(MagEntries)*60,
                  MagDuration = mean(MagDuration)*60
        ) %>% 
        ungroup() %>% 
        mutate(Devaluation = `1Lever_DevaluedLever?`) %>% 
        rename(`lever Presses` = LP)
      
      # COnvert to wide format for difference score
      summary_wide <- summary %>%
        select(-Devaluation, -MagEntries, -MagDuration) %>% 
        pivot_wider(names_from = "1Lever_DevaluedLever?", values_from = "lever Presses") %>% 
        mutate(Deval_index = `Non-Devalued` - Devalued,
               Devaluation = "Satiety")
        
      # Check that there are equal numbers of entries per subject/day
      summary_wide %>% 
        group_by(Subject) %>% 
        summarise(n = n()) %>% 
        spread(Subject, n)%>%
        kable()
      
      return(summary_wide)   
      
    }
    
    
    # Save data for analysis
    summary <- plot_data(data_path)
    save_path <- here("rawdata", "processed_data", "SKR216_SatietyDevalTest_DevalIndex.csv")
    write_csv(summary, save_path)
    
    # Plot Data
    scalelims = c(-10,30)
    scalelims_breaks = 5
    
    SKR216_Satiety_DevalTest_Index <- summary %>% 
      filter(Subject  != 29 & Subject  != 31) %>% 
      ggplot(mapping = aes(x = Devaluation, y = Deval_index, colour = Devaluation, group = Devaluation, fill = Devaluation)) +
      # stat_summary(fun = "mean", geom = "bar", size = 1, position = position_dodge(width = .9)) +
      # stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.2, position = position_dodge(width = 0.9), colour = "black", linetype = 1, show.legend = FALSE) +
      geom_violin(show.legend = FALSE)+
      geom_point(show.legend = FALSE)+
      theme_cowplot() +
      coord_cartesian(ylim = scalelims) + 
      scale_y_continuous(breaks = seq(scalelims[1],scalelims[2],by = scalelims_breaks), expand = c(0.0,0)) +
      # ggtitle("",subtitle = "") + 
      ylab("Non-Devalued - Devalued \n Lever presses/min") + 
      xlab("Devaluation") +
      theme(axis.title.x=element_text(face = "bold")) +
      scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
      scale_fill_manual(name = "", values = fillcolours)
    
    SKR216_Satiety_DevalTest_Index
    
    
    
    
    
    
    # Expt2. SKR216 - Satiety sPIT Test  ----------------------------------------------  

    # Define rawdata filepath 
    data_path <- here("rawdata", "SKR216_sPIT_Test.csv")
    
    # Load and organise data 
    plot_data <- function(data_path) {
      
      ### 1. Prepare Data
      # Load Data
      full_data <- read_csv(data_path)
      
      summary <- full_data %>% 
        filter(
          `Trial Structure` == "PreCS" | `Trial Structure` == "CS",
          # `trial period split minutes [1,2]` == "PreCS2"|`trial period split minutes [1,2]` == "CS1",
          # `trial period split minutes [1,2]` == "PreCS2"|`trial period split minutes [1,2]` == "CS2",
          # `trial period split minutes [1,2]` == "PreCS1" | `trial period split minutes [1,2]` == "PreCS2"|`trial period split minutes [1,2]` == "CS1"|`trial period split minutes [1,2]` == "CS2",
          # `Trial Number` == 1|`Trial Number` == 2,
          # Session == 1 | Session == 2,
          Devaluation == "Satiety"
        ) %>% 
        group_by(Devaluation, Group, Session, Subject, `Trial Structure`, `Devalued Pavlovian US?`, `1Lever_LeverOutcomeIdentity`,`1Lever_DevaluedLever?`, `1Lever_PavlovianUS_Same/Different`) %>% 
        summarise(LP = mean(`1 Lever Test_Lever Pressing`)*60,
                  MagEntries = mean(MagEntries)*60,
                  MagDuration = mean(MagDuration)*60
        ) %>% 
        ungroup()
      
      
      
      summary_LP_wide <- summary %>% 
        select(-`Devalued Pavlovian US?`, -MagEntries, -MagDuration) %>% 
        pivot_wider(names_from = `Trial Structure`, values_from = LP) 
      
      summary_LP_PreCSonly <- summary_LP_wide %>% 
        select(-CS) %>% 
        group_by(Group, Session, Subject, Devaluation, `1Lever_DevaluedLever?`) %>% 
        summarise(PreCS = mean(PreCS)) %>%
        ungroup()
      
      summary_LP_PreCSonly <- summary_LP_PreCSonly %>% 
        pivot_longer(cols = PreCS, names_to = "Trial Structure", values_to = "LP") %>% 
        mutate("1Lever_PavlovianUS_Same/Different" = "PreCS",
               `Devalued Pavlovian US?` = "PreCS")
      
      summary_LP_commonPreCS <- summary %>% 
        filter(`Trial Structure` == "CS") %>% 
        full_join(summary_LP_PreCSonly)
      
      
      summary_LP_commonPreCS <- summary_LP_commonPreCS %>% 
        select(-Devaluation) %>% 
        rename(same_diff = `1Lever_PavlovianUS_Same/Different`,
               Devaluation = `1Lever_DevaluedLever?`,
               `lever Presses` = LP)
      
      # Average across repeated session
      summary_LP_commonPreCS <- summary_LP_commonPreCS %>% 
        group_by(Subject, Devaluation,  `Trial Structure`, same_diff, `Devalued Pavlovian US?`) %>% 
        summarise(`lever Presses` = mean(`lever Presses`)) %>% 
        ungroup()
      
      
      levelorder <- c("Non-Devalued", "Devalued")
      levelorderCS <- c("PreCS", "Non-Devalued", "Devalued", "General")
      leverlorderCS2 <- c("PreCS", "Same", "Different", "General")
      summary_LP_commonPreCS <- summary_LP_commonPreCS %>% 
        mutate(Devaluation = factor(Devaluation, levels = levelorder),
               `Devalued Pavlovian US?` = factor(`Devalued Pavlovian US?`, levels = levelorderCS),
               same_diff = factor(same_diff, levels = leverlorderCS2))
      
      
      return(summary_LP_commonPreCS)
      
    }
    
    
    
    # Save data for analysis
    summary <- plot_data(data_path)
    save_path <- here("rawdata", "processed_data", "SKR216_SatietysPITTest.csv")
    write_csv(summary, save_path)
    
    # Plot Data
    scalelims = c(0,8)
    scalelims_breaks = 1
    
    
    SKR216_satiety_sPITTest_splitSameDiff <- summary %>%
      filter(Subject != 8) %>%
      filter(Subject  != 29 & Subject  != 31) %>% 
      ggplot(mapping = aes(x = Devaluation, y = `lever Presses`, colour = same_diff, group = same_diff, fill = same_diff)) +
      stat_summary(fun = "mean", geom = "bar", size = 1, position = position_dodge(width = .9)) +
      stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.2, position = position_dodge(width = 0.9), colour = "black", linetype = 1, show.legend = FALSE) +
      theme_cowplot() +
      coord_cartesian(ylim = scalelims) + 
      scale_y_continuous(breaks = seq(scalelims[1],scalelims[2],by = scalelims_breaks), expand = c(0.0,0)) +
      # ggtitle("",subtitle = "") + 
      ylab("Lever presses/min") + 
      xlab("Lever") +
      theme(axis.title.x=element_text(face = "bold")) +
      scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
      scale_fill_manual(name = "", values = fillcolours)
    
    SKR216_satiety_sPITTest_splitSameDiff   
    
    # Expt2. SKR216 - Satiety sPIT Test  - sPIT Index ----------------------------------------------  
    
    # Define rawdata filepath 
    data_path <- here("rawdata", "SKR216_sPIT_Test.csv")
    
    # Load and organise data 
    plot_data <- function(data_path) {
      
      ### 1. Prepare Data
      # Load Data
      full_data <- read_csv(data_path)
      
      summary <- full_data %>% 
        filter(
          `Trial Structure` == "PreCS" | `Trial Structure` == "CS",
          # `trial period split minutes [1,2]` == "PreCS2"|`trial period split minutes [1,2]` == "CS1",
          # `trial period split minutes [1,2]` == "PreCS2"|`trial period split minutes [1,2]` == "CS2",
          # `trial period split minutes [1,2]` == "PreCS1" | `trial period split minutes [1,2]` == "PreCS2"|`trial period split minutes [1,2]` == "CS1"|`trial period split minutes [1,2]` == "CS2",
          # `Trial Number` == 1|`Trial Number` == 2,
          # Session == 1 | Session == 2,
          Devaluation == "Satiety"
        ) %>% 
        group_by(Devaluation, Group, Session, Subject, `Trial Structure`, `Devalued Pavlovian US?`, `1Lever_LeverOutcomeIdentity`,`1Lever_DevaluedLever?`, `1Lever_PavlovianUS_Same/Different`) %>% 
        summarise(LP = mean(`1 Lever Test_Lever Pressing`)*60,
                  MagEntries = mean(MagEntries)*60,
                  MagDuration = mean(MagDuration)*60
        ) %>% 
        ungroup()
      
      
      
      summary_LP_wide <- summary %>% 
        select(-`Devalued Pavlovian US?`, -MagEntries, -MagDuration) %>% 
        pivot_wider(names_from = `Trial Structure`, values_from = LP) 
      
      summary_LP_PreCSonly <- summary_LP_wide %>% 
        select(-CS) %>% 
        group_by(Group, Session, Subject, Devaluation, `1Lever_DevaluedLever?`) %>% 
        summarise(PreCS = mean(PreCS)) %>%
        ungroup()
      
      summary_LP_PreCSonly <- summary_LP_PreCSonly %>% 
        pivot_longer(cols = PreCS, names_to = "Trial Structure", values_to = "LP") %>% 
        mutate("1Lever_PavlovianUS_Same/Different" = "PreCS",
               `Devalued Pavlovian US?` = "PreCS")
      
      summary_LP_commonPreCS <- summary %>% 
        filter(`Trial Structure` == "CS") %>% 
        full_join(summary_LP_PreCSonly)
      
      
      summary_LP_commonPreCS <- summary_LP_commonPreCS %>% 
        select(-Devaluation) %>% 
        rename(same_diff = `1Lever_PavlovianUS_Same/Different`,
               Devaluation = `1Lever_DevaluedLever?`,
               `lever Presses` = LP)
      
      # Average across repeated session
      summary_LP_commonPreCS <- summary_LP_commonPreCS %>% 
        group_by(Subject, Devaluation,  `Trial Structure`, same_diff, `Devalued Pavlovian US?`) %>% 
        summarise(`lever Presses` = mean(`lever Presses`)) %>% 
        ungroup()
      
      
      levelorder <- c("Non-Devalued", "Devalued")
      levelorderCS <- c("PreCS", "Non-Devalued", "Devalued", "General")
      leverlorderCS2 <- c("PreCS", "Same", "Different", "General")
      summary_LP_commonPreCS <- summary_LP_commonPreCS %>% 
        mutate(Devaluation = factor(Devaluation, levels = levelorder),
               `Devalued Pavlovian US?` = factor(`Devalued Pavlovian US?`, levels = levelorderCS),
               same_diff = factor(same_diff, levels = leverlorderCS2))
      
      
      
      summary_LP_sPITindex <- summary_LP_commonPreCS %>% 
        select(-`Devalued Pavlovian US?`) %>%
        filter(`Trial Structure` != "PreCS") %>% 
        pivot_wider(names_from = same_diff, values_from = `lever Presses`) %>% 
        mutate(same_vs_diff = Same - Different,
               same_bias = Same /(Same + Different)) %>% 
        pivot_wider(names_from = Devaluation, values_from = Different:same_bias) %>% 
        mutate(Same_NDev_vs_Dev = `Same_Non-Devalued` - Same_Devalued,
               Diff_NDev_vs_Dev = `Different_Non-Devalued` - Different_Devalued,
               General_NDev_vs_Dev = `General_Non-Devalued` - General_Devalued)
      
      
      devalindex <- read_csv(here("rawdata", "processed_data", "SKR216_SatietyDevalTest_DevalIndex.csv")) %>% 
        select(Subject, Devalued, `Non-Devalued`, Deval_index)
      
      
      summary_LP_sPITindex <- summary_LP_sPITindex %>% 
        full_join(devalindex)
      
      
      
      return(summary_LP_sPITindex)
      
    }
    
    
    # Save data for analysis
    summary <- plot_data(data_path)
    save_path <- here("rawdata", "processed_data", "SKR216_SatietysPITTest_sPITIndex.csv")
    write_csv(summary, save_path)
    
    # Plot Data
    # scalelims = c(-1,1)
    # scalelims_breaks = .2
    # 
    
    SKR216_Satiety_sPITTest_sPITIndex <- summary %>%
      filter(Subject  != 29 & Subject  != 31) %>% 
      ggplot(mapping = aes(x = Deval_index, y = `same_vs_diff_Devalued`)) +
      # stat_summary(fun = "mean", geom = "bar", size = 1, position = position_dodge(width = .9)) +
      # stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.2, position = position_dodge(width = 0.9), colour = "black", linetype = 1, show.legend = FALSE) +
      geom_point() +
      theme_cowplot() +
      # coord_cartesian(ylim = scalelims) + 
      # scale_y_continuous(breaks = seq(scalelims[1],scalelims[2],by = scalelims_breaks), expand = c(0.0,0)) +
      # ggtitle("",subtitle = "") + 
      ylab("same_vs_diff_Devalued") + 
      xlab("Deval_index") +
      theme(axis.title.x=element_text(face = "bold")) 
    
    
    SKR216_Satiety_sPITTest_sPITIndex
    
    # Expt2. SKR216 - Satiety sPIT Test Magazine Entry ----------------------------------------------  
    
    # Define rawdata filepath 
    data_path <- here("rawdata", "SKR216_sPIT_Test.csv")
    
    # Load and organise data 
    plot_data <- function(data_path) {
      
      ### 1. Prepare Data
      # Load Data
      full_data <- read_csv(data_path)
      
      summary <- full_data %>% 
        filter(
          `Trial Structure` == "PreCS" | `Trial Structure` == "CS",
          # `trial period split minutes [1,2]` == "PreCS2"|`trial period split minutes [1,2]` == "CS1",
          # `trial period split minutes [1,2]` == "PreCS2"|`trial period split minutes [1,2]` == "CS2",
          # `trial period split minutes [1,2]` == "PreCS1" | `trial period split minutes [1,2]` == "PreCS2"|`trial period split minutes [1,2]` == "CS1"|`trial period split minutes [1,2]` == "CS2",
          # `Trial Number` == 1|`Trial Number` == 2,
          # Session == 1 | Session == 2,
          Devaluation == "Satiety"
        ) %>% 
        group_by(Devaluation, Group, Session, Subject, `Trial Structure`, `Devalued Pavlovian US?`, `1Lever_LeverOutcomeIdentity`,`1Lever_DevaluedLever?`, `1Lever_PavlovianUS_Same/Different`) %>% 
        summarise(LP = mean(`1 Lever Test_Lever Pressing`)*60,
                  MagEntries = mean(MagEntries)*60,
                  MagDuration = mean(MagDuration)*60
        ) %>% 
        ungroup()
      
      
      
      summary_Mag_wide <- summary %>% 
        select(-`Devalued Pavlovian US?`, -LP, -MagDuration) %>% 
        pivot_wider(names_from = `Trial Structure`, values_from = MagEntries) 
      
      summary_Mag_PreCSonly <- summary_Mag_wide %>% 
        select(-CS) %>% 
        group_by(Group, Session, Subject, Devaluation, `1Lever_DevaluedLever?`) %>% 
        summarise(PreCS = mean(PreCS)) %>%
        ungroup()
      
      summary_Mag_PreCSonly <- summary_Mag_PreCSonly %>% 
        pivot_longer(cols = PreCS, names_to = "Trial Structure", values_to = "MagEntries") %>% 
        mutate("1Lever_PavlovianUS_Same/Different" = "PreCS",
               `Devalued Pavlovian US?` = "PreCS")
      
      summary_Mag_commonPreCS <- summary %>% 
        filter(`Trial Structure` == "CS") %>% 
        full_join(summary_Mag_PreCSonly)
      
      
      summary_Mag_commonPreCS <- summary_Mag_commonPreCS %>% 
        select(-Devaluation) %>% 
        rename(same_diff = `1Lever_PavlovianUS_Same/Different`,
               Devaluation = `1Lever_DevaluedLever?`
               )
      
      # Average across repeated session
      summary_Mag_commonPreCS <- summary_Mag_commonPreCS %>% 
        group_by(Subject, Devaluation,  `Trial Structure`, same_diff, `Devalued Pavlovian US?`) %>% 
        summarise(MagEntries = mean(MagEntries)) %>% 
        ungroup()
      
      
      levelorder <- c("Non-Devalued", "Devalued")
      levelorderCS <- c("PreCS", "Non-Devalued", "Devalued", "General")
      leverlorderCS2 <- c("PreCS", "Same", "Different", "General")
      summary_Mag_commonPreCS <- summary_Mag_commonPreCS %>% 
        mutate(Devaluation = factor(Devaluation, levels = levelorder),
               `Devalued Pavlovian US?` = factor(`Devalued Pavlovian US?`, levels = levelorderCS),
               same_diff = factor(same_diff, levels = leverlorderCS2))
      
      
      return(summary_Mag_commonPreCS)
      
    }
    
    
    
    # Save data for analysis
    summary <- plot_data(data_path)
    save_path <- here("rawdata", "processed_data", "SKR216_SatietysPITTest_MagEntries.csv")
    write_csv(summary, save_path)
    
    # Plot Data
    scalelims = c(0,8)
    scalelims_breaks = 1
    
    
    SKR216_satiety_sPITTest_splitMagEntries<- summary %>%
      filter(Subject != 8) %>%
      filter(Subject  != 29 & Subject  != 31) %>% 
      ggplot(mapping = aes(x = Devaluation, y = MagEntries, colour = same_diff, group = same_diff, fill = same_diff)) +
      stat_summary(fun = "mean", geom = "bar", size = 1, position = position_dodge(width = .9)) +
      stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.2, position = position_dodge(width = 0.9), colour = "black", linetype = 1, show.legend = FALSE) +
      theme_cowplot() +
      coord_cartesian(ylim = scalelims) + 
      scale_y_continuous(breaks = seq(scalelims[1],scalelims[2],by = scalelims_breaks), expand = c(0.0,0)) +
      # ggtitle("",subtitle = "") + 
      ylab("Lever presses/min") + 
      xlab("Lever") +
      theme(axis.title.x=element_text(face = "bold")) +
      scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
      scale_fill_manual(name = "", values = fillcolours)
    
    SKR216_satiety_sPITTest_splitMagEntries   
    
    SKR216_satiety_sPITTest_splitMagEntries<- summary %>%
      filter(Subject != 8) %>%
      filter(Subject  != 29 & Subject  != 31) %>% 
      ggplot(mapping = aes(x = Devaluation, y = MagEntries, colour = `Devalued Pavlovian US?`, group = `Devalued Pavlovian US?`, fill = `Devalued Pavlovian US?`)) +
      stat_summary(fun = "mean", geom = "bar", size = 1, position = position_dodge(width = .9)) +
      stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.2, position = position_dodge(width = 0.9), colour = "black", linetype = 1, show.legend = FALSE) +
      theme_cowplot() +
      coord_cartesian(ylim = scalelims) + 
      scale_y_continuous(breaks = seq(scalelims[1],scalelims[2],by = scalelims_breaks), expand = c(0.0,0)) +
      # ggtitle("",subtitle = "") + 
      ylab("Lever presses/min") + 
      xlab("Lever") +
      theme(axis.title.x=element_text(face = "bold")) +
      scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
      scale_fill_manual(name = "", values = fillcolours)
    
    SKR216_satiety_sPITTest_splitMagEntries   
    
    
    
    
    
    # # SKR216 SINGLE LEVER EXPERIMENTS 3. LiCL -----------------------------------------
    # # Expt3. SKR216 LiCL - Instrumental Acquisition --------------------------------
    # 
    # # Define rawdata filepath
    # data_path <- here("rawdata", "SKR216_Inst_Acquisition.csv")
    # 
    # # Load and organise data
    # plot_data <- function(data_path) {
    # 
    #   ### 1. Prepare Data
    #   # Load Data
    #   full_data <- read_csv(data_path)
    # 
    #   # Summarise data into averages per session/day
    #   summary <- full_data %>%
    #     group_by(Experiment,Experiment_Devaluation, Schedule, Day, Subject, RewardName, LeverName) %>%
    #     summarise(LP = sum(`LP Bin`),
    #               Rewards = sum(`Rewards Bin`),
    #               SessionLength = median(`Session Length`[`Session Length` != 0]),
    #               LP_rate_min = (LP/SessionLength) *60,
    #               MagEntry = sum(`MagEntry Bin`),
    #               MagEntry_rate_min = (MagEntry/SessionLength) *60,
    #     ) %>%
    #     ungroup()
    # 
    #   # Check that there is only a single value per subject/reward/day
    #   summary %>%
    #     group_by(Subject,RewardName, Day) %>%
    #     summarise(n = n()) %>%
    #     spread(Day, n)%>%
    #     kable()
    # 
    #   ### 2. Fix data Order for plotting
    #   levelorder <- c("Sucrose", "Maltodextrin")
    #   summary <- summary %>%
    #     mutate(RewardName = str_replace(string = RewardName, pattern = "maltodextrin",replacement = "Maltodextrin"),
    #            RewardName = factor(RewardName, levels = levelorder)) %>%
    #     arrange(RewardName,Day)
    # 
    #   ### Filter relevant Group
    #   summary <- summary %>%
    #     filter(Experiment_Devaluation == "Taste_Aversion")
    # 
    #   # Return Summary Data
    #   return(summary)
    # }
    # 
    # # Save data for analysis
    # summary <- plot_data(data_path)
    # save_path <- here("rawdata", "processed_data", "SKR216_LiCL_InstrumentalAcquisition.csv")
    # write_csv(summary, save_path)
    # 
    # # Plot Data
    # scalelims = c(0,35)
    # scalelims_breaks = 5
    # 
    # SKR216_LiCL_Instrumental_acquisition <- summary %>%
    #   filter(Day <= 6) %>%
    #   filter(Subject != 29 & Subject != 31) %>%
    #   ggplot(mapping = aes(x = as.factor(Day), y = LP_rate_min, colour = RewardName, group = RewardName, fill = RewardName, shape = RewardName, linetype = RewardName)) +
    #   stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5, linetype = 1, show.legend = FALSE) +
    #   stat_summary(fun = "mean", geom = "line", size = 0.5) +
    #   stat_summary(fun = "mean", geom = "point", size = 3) +
    #   theme_cowplot() +
    #   coord_cartesian(ylim = scalelims) +
    #   scale_y_continuous(breaks = seq(scalelims[1],scalelims[2],by = scalelims_breaks), expand = c(0.0,0)) +
    #   # ggtitle("",subtitle = "") +
    #   ylab("Lever presses/min") +
    #   xlab("Day") +
    #   theme(axis.title.x=element_text(face = "bold")) +
    #   scale_linetype_manual(name = "", values = linetypes)+
    #   scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    #   scale_shape_manual(name = "", values = pointshapes) +
    #   scale_fill_manual(name = "", values = fillcolours)
    # 
    # SKR216_LiCL_Instrumental_acquisition
    # 
    # # Expt3. SKR216 LiCl - Pavlovian Acquisition -----------------------------------------------------------------------
    # 
    # # Define rawdata filepath 
    # data_path <- here("rawdata", "SKR216_Pav_Freq_Acquisition.csv")
    # 
    # # Load and organise data 
    # plot_data <- function(data_path) {
    #   
    #   ### 1. Prepare Data
    #   # Load Data
    #   full_data <- read_csv(data_path)
    #   
    #   # Check that there are equal numbers of entries per subject/day
    #   full_data %>% 
    #     group_by(Subject, Day) %>% 
    #     summarise(n = n()) %>% 
    #     spread(Day, n)%>%
    #     kable()
    #   
    #   
    #   full_data <- full_data %>% 
    #     mutate(Experiment_Devaluation = case_when(Subject < 9 ~ "Taste Aversion",
    #                                               Subject > 8 ~ "Satiety") )
    #   
    #   # Remove unnecessary calculation columns
    #   full_data <- full_data %>% 
    #     filter(Experiment_Devaluation == "Taste Aversion") %>% 
    #     select(Experiment_Devaluation,
    #            `Day`,                                                    
    #            `MSN`,                                                    
    #            `StartDate`,                                              
    #            `StartTime`,                                              
    #            `Experiment`,                                             
    #            `Group`,                                                  
    #            `Box`,                                                    
    #            `Subject`,                                                
    #            `Total MagEntries`,                                       
    #            `Total MagDuration`,                                      
    #            `Total Rewards`,                                          
    #            `Total Session Time`,                                     
    #            `Trial Number`,                                           
    #            `ITI_Duration(minus PreCS)`,                              
    #            `CS_Identity [1 = Noise, 2 = Click, 3 = tone]`,           
    #            `US_Identity [1 = Sucrose, 2 = Maltodextrin, 3 = Pellet]`,
    #            `Number of USs`,                                          
    #            `MagEntry_ITI`,
    #            `Latency to first magazine Entry`,                        
    #            `First response was an Exit`,                             
    #            `MagEntry_PreCS (2mins)`,                                 
    #            `MagEntry_CS (2 mins)`,                                   
    #            `CS-PreCS`,                                               
    #            `Trial Number, 1 CS`,                                     
    #            `CS_Name`,                                                
    #            `US_Name`,                                                
    #            `MagEntriesCS excluding reward delivery 5s`,              
    #            `CS_Duration (s)_NoReward5s`,                             
    #            `Devalued Reward 1st test`,                               
    #            `Devalued?`)
    #   
    #   # Summarise data into averages per Outcome ID, rates per trial (1 mins)
    #   summary <- full_data %>% 
    #     group_by(Experiment, Day, Subject, CS_Name, US_Name) %>% 
    #     summarise(AvgRewards = mean(`Number of USs`),
    #               CS_PreCS = mean(`CS-PreCS`)/2,
    #               MagEntry_PreCS = mean(`MagEntry_PreCS (2mins)`)/2,
    #               MagEntry_CS = mean(`MagEntry_CS (2 mins)`)/2
    #     ) %>% 
    #     ungroup()
    #   
    #   # Check that there are equal numbers of trial types/subject
    #   summary %>% 
    #     group_by(Subject, Day) %>% 
    #     summarise(n = n()) %>% 
    #     spread(Day, n)%>%
    #     kable()
    #   
    #   ### 2. Fix data Order for plotting
    #   levelorder <- c("Sucrose", "Maltodextrin", "Pellet")
    #   summary <- summary %>% 
    #     mutate(US_Name = factor(US_Name, levels = levelorder))
    #   
    #   
    #   # Return Summary Data
    #   return(summary)
    # }
    # 
    # # Save data for analysis
    # summary <- plot_data(data_path)
    # save_path <- here("rawdata", "processed_data", "SKR216_LiCl_PavlovianAcquisition.csv")
    # write_csv(summary, save_path)
    # 
    # # Plot Data
    # scalelims = c(0,15)
    # scalelims_breaks = 5
    # 
    # SKR216_LiCl_Pavlovian_acquisition <- summary %>% 
    #   filter(Day <= 6) %>%
    #   # filter(Subject != 8) %>%
    #   filter(Subject != 29 & Subject != 31) %>% 
    #   ggplot(mapping = aes(x = as.factor(Day), y = CS_PreCS, colour = US_Name, group = US_Name, fill = US_Name, shape = US_Name, linetype = US_Name)) +
    #   stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5, linetype = 1, show.legend = FALSE) +
    #   stat_summary(fun = "mean", geom = "line", size = 0.5) +
    #   stat_summary(fun = "mean", geom = "point", size = 3) +
    #   theme_cowplot() +
    #   coord_cartesian(ylim = scalelims) + 
    #   scale_y_continuous(breaks = seq(scalelims[1],scalelims[2],by = scalelims_breaks), expand = c(0.0,0)) +
    #   # ggtitle("",subtitle = "") + 
    #   ylab("CS-PreCS Entries/min") + 
    #   xlab("Day") +
    #   theme(axis.title.x=element_text(face = "bold")) +
    #   scale_linetype_manual(name = "", values = linetypes)+
    #   scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    #   scale_shape_manual(name = "", values = pointshapes) +
    #   scale_fill_manual(name = "", values = fillcolours)
    # 
    # SKR216_LiCl_Pavlovian_acquisition
    #
    # 
    # # Expt3. SKR216 LiCl - LiCl Consumption  ----------------------------------------------  
    # 
    # # Define rawdata filepath 
    # data_path <- here("rawdata", "SKR216_LiCl_TasteAversion.csv")
    # 
    # # Load and organise data 
    # plot_data <- function(data_path) {
    #   
    #   ### 1. Prepare Data
    #   # Load Data
    #   full_data <- read_csv(data_path)
    #   
    # 
    #   ### 2. Fix data Order for plotting
    #   levelorder <- c("Saline", "LiCl")
    #   full_data <- full_data %>% 
    #     rename(`Consumed (g)` = `consumed(g)`) %>% 
    #     mutate(Injection = factor(Injection, levels = levelorder))
    # 
    #   # Check that there are equal numbers of entries per subject/day
    #   full_data %>%
    #     group_by(Rat, Pairing, Injection) %>%
    #     summarise(n = n()) %>%
    #     spread(Rat, n)%>%
    #     kable()
    #   
    #   return(full_data)
    #   
    # }
    # 
    # # Save data for analysis
    # summary <- plot_data(data_path)
    # save_path <- here("rawdata", "processed_data", "SKR216_LiClConsumption.csv")
    # write_csv(summary, save_path)
    # 
    # # Plot Data
    # scalelims = c(0,22)
    # scalelims_breaks = 2
    # 
    # 
    # SKR216_LiCl_Consumption <- summary %>% 
    #   ggplot(mapping = aes(x = as.factor(Pairing), y = `Consumed (g)`, colour = Injection, group = Injection, fill = Injection, shape = Injection, linetype = Injection)) +
    #   stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5, linetype = 1, show.legend = FALSE) +
    #   stat_summary(fun = "mean", geom = "line", size = 0.5) +
    #   stat_summary(fun = "mean", geom = "point", size = 3) +
    #   theme_cowplot() +
    #   coord_cartesian(ylim = scalelims) + 
    #   scale_y_continuous(breaks = seq(scalelims[1],scalelims[2],by = scalelims_breaks), expand = c(0.0,0)) +
    #   # ggtitle("",subtitle = "") + 
    #   ylab("Consumed (g)") + 
    #   xlab("Pairing") +
    #   theme(axis.title.x=element_text(face = "bold")) +
    #   scale_linetype_manual(name = "", values = linetypes)+
    #   scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    #   scale_shape_manual(name = "", values = pointshapes) +
    #   scale_fill_manual(name = "", values = fillcolours)
    # 
    # SKR216_LiCl_Consumption
    # 
    # # Expt3. SKR216 - LiCl in Magazine  ----------------------------------------------  
    # 
    # # Define rawdata filepath 
    # data_path <- here("rawdata", "SKR216_LiCl_TasteAversion_inMagazine.csv")
    # 
    # # Load and organise data 
    # plot_data <- function(data_path) {
    #   
    #   ### 1. Prepare Data
    #   # Load Data
    #   full_data <- read_csv(data_path)
    #   
    #   summary <- full_data %>% 
    #     mutate(Liquid = case_when(str_detect(MSN,"sucrose") ~ "Sucrose",
    #                               str_detect(MSN,"Maltodextrin") ~ "Maltodextrin")
    #     ) %>% 
    #     select(Injection, Liquid, Subject, Bin_4mins, Mag1_5sBins, Mag1_duration_5sbins) %>% 
    #     group_by(Injection, Subject, Liquid, Bin_4mins) %>% 
    #     summarise(Bin_4mins = median(Bin_4mins),
    #               MagEntry = sum(Mag1_5sBins)/4,
    #               MagDur = sum(Mag1_duration_5sbins)/100/4) %>% 
    #     ungroup() %>% 
    #     filter(Bin_4mins < 7)
    #   
    #   levelorder <- c("Saline", "LiCl")
    #   summary <- summary %>% 
    #     mutate(Injection = factor(Injection, levels = levelorder))
    #   
    #   return(summary)
    #   
    # }   
    # 
    # 
    # # Save data for analysis
    # summary <- plot_data(data_path)
    # save_path <- here("rawdata", "processed_data", "SKR216_LiCl_DevaluationinMag.csv")
    # write_csv(summary, save_path)
    # 
    # # Plot Data
    # scalelims = c(0,10)
    # scalelims_breaks = 2
    # 
    # 
    # SKR216_LiCl_DevalinMag <- summary %>% 
    #   ggplot(mapping = aes(x = as.factor(Bin_4mins), y = MagEntry, colour = Injection, group = Injection, fill = Injection, shape = Injection, linetype = Injection)) +
    #   stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5, linetype = 1, show.legend = FALSE) +
    #   stat_summary(fun = "mean", geom = "line", size = 0.5) +
    #   stat_summary(fun = "mean", geom = "point", size = 3) +
    #   theme_cowplot() +
    #   coord_cartesian(ylim = scalelims) + 
    #   scale_y_continuous(breaks = seq(scalelims[1],scalelims[2],by = scalelims_breaks), expand = c(0.0,0)) +
    #   # ggtitle("",subtitle = "") + 
    #   ylab("Magazine Entries/Min") + 
    #   xlab("4 minute bins") +
    #   theme(axis.title.x=element_text(face = "bold")) +
    #   scale_linetype_manual(name = "", values = linetypes)+
    #   scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    #   scale_shape_manual(name = "", values = pointshapes) +
    #   scale_fill_manual(name = "", values = fillcolours)
    # 
    # SKR216_LiCl_DevalinMag   
    # 
    # # Expt3. SKR216 - LiCl Instrumental Devaluation  ----------------------------------------------  
    # 
    # # Define rawdata filepath 
    # data_path <- here("rawdata", "SKR216_sPIT_Test.csv")
    # 
    # # Load and organise data 
    # plot_data <- function(data_path) {
    #   
    #   ### 1. Prepare Data
    #   # Load Data
    #   full_data <- read_csv(data_path)
    #   
    #   summary <- full_data %>% 
    #     filter(`Trial Structure` == "Baseline Extinction",
    #            Devaluation == "LiCl") %>% 
    #     group_by(Group, Subject,`1Lever_DevaluedLever?`, `Minute Counter`) %>% 
    #     summarise(LP = mean(`1 Lever Test_Lever Pressing`)*60,
    #               MagEntries = mean(MagEntries)*60,
    #               MagDuration = mean(MagDuration)*60
    #     ) %>% 
    #     ungroup() %>% 
    #     mutate(Devaluation = `1Lever_DevaluedLever?`) %>% 
    #     rename(`lever Presses` = LP)
    #   
    #   
    #   ### 2. Fix data Order for plotting
    #   levelorder <- c("Non-Devalued", "Devalued")
    #   summary_LP <- summary%>% 
    #     mutate(Devaluation = factor(Devaluation, levels = levelorder))
    #   
    #   
    #   
    #   # Check that there are equal numbers of entries per subject/day
    #   summary_LP %>% 
    #     group_by(Subject) %>% 
    #     summarise(n = n()) %>% 
    #     spread(Subject, n)%>%
    #     kable()
    #   
    #   return(summary_LP)   
    #   
    # }
    # 
    # 
    # # Save data for analysis
    # summary <- plot_data(data_path)
    # save_path <- here("rawdata", "processed_data", "SKR216_LiClDevalTest.csv")
    # write_csv(summary, save_path)
    # 
    # # Plot Data
    # scalelims = c(0,30)
    # scalelims_breaks = 5
    # 
    # SKR216_LiCl_DevalTest <- summary %>% 
    #   # filter(Subject != 8) %>%
    #   filter(Subject  != 29 & Subject  != 31) %>% ggplot(mapping = aes(x = as.factor(`Minute Counter`), y = `lever Presses`, colour = Devaluation, group = Devaluation, fill = Devaluation, shape = Devaluation, linetype = Devaluation)) +
    #   stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.5, linetype = 1, show.legend = FALSE) +
    #   stat_summary(fun = "mean", geom = "line", size = 0.5) +
    #   stat_summary(fun = "mean", geom = "point", size = 3) +
    #   theme_cowplot() +
    #   coord_cartesian(ylim = scalelims) + 
    #   scale_y_continuous(breaks = seq(scalelims[1],scalelims[2],by = scalelims_breaks), expand = c(0.0,0)) +
    #   # ggtitle("",subtitle = "") + 
    #   ylab("Lever presses/min") + 
    #   xlab("1 minute bins") +
    #   theme(axis.title.x=element_text(face = "bold")) +
    #   scale_linetype_manual(name = "", values = linetypes)+
    #   scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    #   scale_shape_manual(name = "", values = pointshapes) +
    #   scale_fill_manual(name = "", values = fillcolours)
    # 
    # SKR216_LiCl_DevalTest
    # 
    # 
    # # Expt3. SKR216 - LiCl sPIT Test  ----------------------------------------------  
    # 
    # # Define rawdata filepath 
    # data_path <- here("rawdata", "SKR216_sPIT_Test.csv")
    # 
    # # Load and organise data 
    # plot_data <- function(data_path) {
    #   
    #   ### 1. Prepare Data
    #   # Load Data
    #   full_data <- read_csv(data_path)
    #   
    #   summary <- full_data %>% 
    #     filter(
    #       `Trial Structure` == "PreCS" | `Trial Structure` == "CS",
    #       # `trial period split minutes [1,2]` == "PreCS2"|`trial period split minutes [1,2]` == "CS1",
    #       # `trial period split minutes [1,2]` == "PreCS1" | `trial period split minutes [1,2]` == "PreCS2"|`trial period split minutes [1,2]` == "CS1"|`trial period split minutes [1,2]` == "CS2",
    #       `Trial Number` == 1|`Trial Number` == 2,
    #       Session == 1 | Session == 2,
    #       Devaluation == "LiCl"
    #     ) %>% 
    #     group_by(Devaluation, Group, Session, Subject, `Trial Structure`, `Devalued Pavlovian US?`, `1Lever_LeverOutcomeIdentity`,`1Lever_DevaluedLever?`, `1Lever_PavlovianUS_Same/Different`) %>% 
    #     summarise(LP = mean(`1 Lever Test_Lever Pressing`)*60,
    #               MagEntries = mean(MagEntries)*60,
    #               MagDuration = mean(MagDuration)*60
    #     ) %>% 
    #     ungroup()
    # 
    #   
    #   
    #   summary_LP_wide <- summary %>% 
    #     select(-`Devalued Pavlovian US?`, -MagEntries, -MagDuration) %>% 
    #     pivot_wider(names_from = `Trial Structure`, values_from = LP) 
    #   
    #   summary_LP_PreCSonly <- summary_LP_wide %>% 
    #     select(-CS) %>% 
    #     group_by(Group, Session, Subject, Devaluation, `1Lever_DevaluedLever?`) %>% 
    #     summarise(PreCS = mean(PreCS)) %>%
    #     ungroup()
    #   
    #   summary_LP_PreCSonly <- summary_LP_PreCSonly %>% 
    #     pivot_longer(cols = PreCS, names_to = "Trial Structure", values_to = "LP") %>% 
    #     mutate("1Lever_PavlovianUS_Same/Different" = "PreCS",
    #            `Devalued Pavlovian US?` = "PreCS")
    #   
    #   summary_LP_commonPreCS <- summary %>% 
    #     filter(`Trial Structure` == "CS") %>% 
    #     full_join(summary_LP_PreCSonly)
    # 
    #   
    #   summary_LP_commonPreCS <- summary_LP_commonPreCS %>% 
    #     select(-Devaluation) %>% 
    #     rename(same_diff = `1Lever_PavlovianUS_Same/Different`,
    #            Devaluation = `1Lever_DevaluedLever?`,
    #            `lever Presses` = LP)
    #   
    #   # Average across repeated session
    #   summary_LP_commonPreCS <- summary_LP_commonPreCS %>% 
    #     group_by(Subject, Devaluation,  `Trial Structure`, same_diff, `Devalued Pavlovian US?`) %>% 
    #     summarise(`lever Presses` = mean(`lever Presses`)) %>% 
    #     ungroup()
    #   
    #   
    #   levelorder <- c("Non-Devalued", "Devalued")
    #   levelorderCS <- c("PreCS", "Non-Devalued", "Devalued", "General")
    #   leverlorderCS2 <- c("PreCS", "Same", "Different", "General")
    #   summary_LP_commonPreCS <- summary_LP_commonPreCS %>% 
    #     mutate(Devaluation = factor(Devaluation, levels = levelorder),
    #            `Devalued Pavlovian US?` = factor(`Devalued Pavlovian US?`, levels = levelorderCS),
    #            same_diff = factor(same_diff, levels = leverlorderCS2))
    #   
    #   
    #   return(summary_LP_commonPreCS)
    #   
    # }
    # 
    # 
    # 
    # # Save data for analysis
    # summary <- plot_data(data_path)
    # save_path <- here("rawdata", "processed_data", "SKR216_LiClsPITTest.csv")
    # write_csv(summary, save_path)
    # 
    # # Plot Data
    # scalelims = c(0,8)
    # scalelims_breaks = 1
    # 
    # 
    # SKR216_LiCl_sPITTest_splitSameDiff <- summary %>%
    #   filter(Subject != 8) %>%
    #   filter(Subject  != 29 & Subject  != 31) %>% 
    #   ggplot(mapping = aes(x = Devaluation, y = `lever Presses`, colour = same_diff, group = same_diff, fill = same_diff)) +
    #   stat_summary(fun = "mean", geom = "bar", size = 1, position = position_dodge(width = .9)) +
    #   stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0, size = 0.2, position = position_dodge(width = 0.9), colour = "black", linetype = 1, show.legend = FALSE) +
    #   theme_cowplot() +
    #   coord_cartesian(ylim = scalelims) + 
    #   scale_y_continuous(breaks = seq(scalelims[1],scalelims[2],by = scalelims_breaks), expand = c(0.0,0)) +
    #   # ggtitle("",subtitle = "") + 
    #   ylab("Lever presses/min") + 
    #   xlab("Lever") +
    #   theme(axis.title.x=element_text(face = "bold")) +
    #   scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
    #   scale_fill_manual(name = "", values = fillcolours)
    # 
    # SKR216_LiCl_sPITTest_splitSameDiff    
    # 
    # 
    # 
        

    
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
library(grid)
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


##### SKR132 - Sequential procedure ----

#### Load data ----


full_data <- read_csv(here('data','SKR132_ConditionedInhibitionSequential_MagFreq.csv'))

# Filter out exclusions due to histology/behaviouralcriterion
# None
# Re-label columns for easier variable names
full_data <- full_data %>% 
  rename(InfusionGroup = `Infusion Group`,
         US = `US identity`,
         CS_id = `CS identity`,
         CS_name = `CS Name`,
         trialblock6 = `2 CS Block 6`,
         trialblock3 = `2 CS Block 3`) %>% 
  mutate(reward = US, 
         reward = recode(reward, "3" = "+", "0" = "-")) %>% 
  unite("GroupCueID", InfusionGroup, Cue, reward, sep = "", remove = FALSE)

# unique(full_data$`Infusion Group`)
# "Saline"   "Muscimol"

# unique(full_data$Infusion)
# "Pre-Surgery"  "Post-Surgery"

# unique(full_data$Stage)
# "Stage 1"                                                
# "Stage 1 - Dry Run"                                      
# "Stage 2 - Infusion"                                     
# "Stage 3 - PROTECTION FROM EXTINCTION TEST [NO INFUSION]"
# "Stage 4 - SUMMATION TEST [NO INFUSION]"                 
# "Stage 5 - RETARDATION TEST [NO INFUSION]"     

# unique(full_data$Cue)
# "C"  "A"  "B"  "Z"  "AX" "BX" "Y"  "X" 


# Re order and rename levels for plotting

levels <- c("Saline: A+" = "SalineA+",
            "Saline: A-" = "SalineA-",
            "Saline: B+" = "SalineB+",
            "Saline: C+" = "SalineC+",
            "Saline: C-" = "SalineC-",
            "Saline: AX-" = "SalineAX-",
            "Saline: B-" = "SalineB-",
            "Saline: BX-" = "SalineBX-",
            "Saline: X+" = "SalineX+",
            "Saline: Y+" = "SalineY+",
            "Saline: Z+" = "SalineZ+",
            "Saline: Y-" = "SalineY-",
            "Saline: Z-" = "SalineZ-",
            "Muscimol: A+" = "MuscimolA+",
            "Muscimol: A-" = "MuscimolA-",
            "Muscimol: B+" = "MuscimolB+",
            "Muscimol: C+" = "MuscimolC+",
            "Muscimol: C-" = "MuscimolC-",
            "Muscimol: AX-" = "MuscimolAX-",
            "Muscimol: B-" = "MuscimolB-",
            "Muscimol: BX-" = "MuscimolBX-",
            "Muscimol: X+" = "MuscimolX+",
            "Muscimol: Y+" = "MuscimolY+",
            "Muscimol: Z+" = "MuscimolZ+",
            "Muscimol: Y-" = "MuscimolY-",
            "Muscimol: Z-" = "MuscimolZ-")
full_data$GroupCueID <- fct_recode(full_data$GroupCueID, !!!levels)

levelorder <- c("Saline: A+" ,
                "Saline: A-" ,
                "Saline: B+" ,
                "Saline: C+" ,
                "Saline: C-" ,
                "Saline: AX-",
                "Saline: B-" ,
                "Saline: BX-",
                "Saline: X+" ,
                "Saline: Y+" ,
                "Saline: Z+" ,
                "Saline: Y-" ,
                "Saline: Z-" ,
                "Muscimol: A+" ,
                "Muscimol: A-" ,
                "Muscimol: B+" ,
                "Muscimol: C+" ,
                "Muscimol: C-" ,
                "Muscimol: AX-",
                "Muscimol: B-" ,
                "Muscimol: BX-",
                "Muscimol: X+" ,
                "Muscimol: Y+" ,
                "Muscimol: Z+" ,
                "Muscimol: Y-" ,
                "Muscimol: Z-" )
full_data$GroupCueID <- fct_relevel(as.factor(full_data$GroupCueID), levelorder)

levelorder <- c("Saline", "Muscimol")

full_data$InfusionGroup <- fct_relevel(as.factor(full_data$InfusionGroup), levelorder)


##### Summarise Data ----
#N.B. Summarising data - Averaging over 2 sessions per day

# unique(full_data$Stage)
# "Stage 1"                                                
# "Stage 1 - Dry Run"                                      
# "Stage 2 - Infusion"                                     
# "Stage 3 - PROTECTION FROM EXTINCTION TEST [NO INFUSION]"
# "Stage 4 - SUMMATION TEST [NO INFUSION]"                 
# "Stage 5 - RETARDATION TEST [NO INFUSION]"     

data_stage1 <- full_data %>% 
  filter(Stage == "Stage 1" | Stage == "Stage 1 - Dry Run" )

data_stage1_summary <- data_stage1 %>% 
  group_by(Day, Subject, Cue, US, CS_id, CS_name, Infusion, InfusionGroup, GroupCueID) %>% 
  summarise(CS = mean(`CS`), na.rm=TRUE,
            PreCS = mean(`PreCS`), na.rm=TRUE,
            CS_Pre = mean(`CS-PreCS`), na.rm=TRUE,
            Latency_mean = mean(`MagEntry Latency to CS`)/100, na.rm=TRUE,
            Latency_median = median(`MagEntry Latency to CS`)/100, na.rm=TRUE,)


#N.B. Summarising data 

data_stage2 <- full_data %>% 
  filter(Stage == "Stage 2 - Infusion")

data_stage2_summary <- data_stage2 %>% 
  group_by(Day, Subject, Cue, US, CS_id, CS_name, Infusion, InfusionGroup, GroupCueID, trialblock6) %>% 
  summarise(CS = mean(`CS`), na.rm=TRUE,
            PreCS = mean(`PreCS`), na.rm=TRUE,
            CS_Pre = mean(`CS-PreCS`), na.rm=TRUE,
            Latency_mean = mean(`MagEntry Latency to CS`)/100, na.rm=TRUE,
            Latency_median = median(`MagEntry Latency to CS`)/100, na.rm=TRUE,)

data_stage2_pertrial_summary <- data_stage2 %>% 
  group_by(Day, Subject, Cue, US, CS_id, CS_name, Infusion, InfusionGroup, GroupCueID, trialblock6, `2 CS Trial Number`) %>% 
  summarise(CS = mean(`CS`), na.rm=TRUE,
            PreCS = mean(`PreCS`), na.rm=TRUE,
            CS_Pre = mean(`CS-PreCS`), na.rm=TRUE,
            Latency_mean = mean(`MagEntry Latency to CS`)/100, na.rm=TRUE,
            Latency_median = median(`MagEntry Latency to CS`)/100, na.rm=TRUE,)


#N.B. Summarising data 

data_stage3 <- full_data %>% 
  filter(Stage == "Stage 3 - PROTECTION FROM EXTINCTION TEST [NO INFUSION]")

data_stage3_summary <- data_stage3 %>% 
  group_by(Day, Subject, Cue, US, CS_id, CS_name, Infusion, InfusionGroup, GroupCueID, trialblock3) %>% 
  summarise(CS = mean(`CS`), na.rm=TRUE,
            PreCS = mean(`PreCS`), na.rm=TRUE,
            CS_Pre = mean(`CS-PreCS`), na.rm=TRUE,
            Latency_mean = mean(`MagEntry Latency to CS`)/100, na.rm=TRUE,
            Latency_median = median(`MagEntry Latency to CS`)/100, na.rm=TRUE,)

#N.B. Summarising data 

data_stage4 <- full_data %>% 
  filter(Stage == "Stage 4 - SUMMATION TEST [NO INFUSION]", 
         `Trial Number Summation Test` < 6)

data_stage4_summary <- data_stage4 %>% 
  group_by(Day, Subject, Cue, US, CS_id, CS_name, Infusion, InfusionGroup, GroupCueID) %>% 
  summarise(CS = mean(`CS`), na.rm=TRUE,
            PreCS = mean(`PreCS`), na.rm=TRUE,
            CS_Pre = mean(`CS-PreCS`), na.rm=TRUE,
            Latency_mean = mean(`MagEntry Latency to CS`)/100, na.rm=TRUE,
            Latency_median = median(`MagEntry Latency to CS`)/100, na.rm=TRUE,)

data_stage4_summary_Block2Trials <- data_stage4 %>% 
  group_by(Day, Subject, Cue, US, CS_id, CS_name, InfusionGroup, GroupCueID, `Trial Number Summation Test Blocked`) %>% 
  summarise(CS = mean(`CS`), na.rm=TRUE,
            PreCS = mean(`PreCS`), na.rm=TRUE,
            CS_Pre = mean(`CS-PreCS`), na.rm=TRUE,
            Latency_mean = mean(`MagEntry Latency to CS`)/100, na.rm=TRUE,
            Latency_median = median(`MagEntry Latency to CS`)/100, na.rm=TRUE,)


#N.B. Summarising data 

data_stage5 <- full_data %>% 
  filter(Stage == "Stage 5 - RETARDATION TEST [NO INFUSION]" )

data_stage5_summary <- data_stage5 %>% 
  group_by(Day, Subject, Cue, US, CS_id, CS_name, Infusion, InfusionGroup, GroupCueID) %>% 
  summarise(CS = mean(`CS`), na.rm=TRUE,
            PreCS = mean(`PreCS`), na.rm=TRUE,
            CS_Pre = mean(`CS-PreCS`), na.rm=TRUE,
            Latency_mean = mean(`MagEntry Latency to CS`)/100, na.rm=TRUE,
            Latency_median = median(`MagEntry Latency to CS`)/100, na.rm=TRUE)



##### locomotor data =====

locomotor_data <- read_csv(here('data','SKR132_LocomotorTest.csv'))

locomotor_data <- locomotor_data %>% 
  rename(InfusionGroup = `Group`)
         
levelorder <- c("Saline", "Muscimol")

locomotor_data$InfusionGroup <- fct_relevel(as.factor(locomotor_data$InfusionGroup), levelorder)

# Summarise data into 5 min bins
data_locomotor_summary <- locomotor_data %>% 
  group_by(Subject, InfusionGroup, Block5mins) %>% 
  summarise(Distance = mean(`Ambulatory Distance`), na.rm=TRUE,
            `Vertical Counts` = mean(`Vertical Counts`), na.rm=TRUE)

#### Figure plotting Parameter ----
# Dark Red "#67001F"  medium Red "#B2182B" Light Red "#D6604D"
# Dark Blue "#053061" meidum Blue "#2166AC" Light Blue "#4393C3"  


#### Figure plotting Parameter ----

DarkRed = "#67001F"
MediumRed = "#B2182B"
LightRed = "#D6604D"
DarkBlue = "#053061"
MediumBlue = "#2166AC"
LightBlue = "#4393C3"
Black       = "#000000"
White       = "#FFFFFF"
LightGrey   = "#F0F0F0"
MediumGrey  = "#BDBDBD"
DarkGrey    = "#252525"

## Shapes for Geom_point
circle            = 21
square            = 22
diamond           = 23
triangleUp        = 24
triangleDown      = 25
cross       = 4

# fillcolours <- c("Saline: A+" = MediumBlue,
#                  "Saline: A-" = MediumBlue,
#                  "Saline: B+" = MediumBlue,
#                  "Saline: C+" = MediumBlue,
#                  "Saline: C-" = MediumBlue,
#                  "Saline: AX-"= LightBlue,
#                  "Saline: B-" = MediumBlue,
#                  "Saline: BX-"= LightBlue,
#                  "Saline: X+" = LightBlue,
#                  "Saline: Y+" = MediumBlue,
#                  "Saline: Z+" = MediumBlue,
#                  "Saline: Y-" = White,
#                  "Saline: Z-" = White,
#                  "Muscimol: A+" = MediumRed,
#                  "Muscimol: A-" = MediumRed,
#                  "Muscimol: B+" = MediumRed,
#                  "Muscimol: C+" = MediumRed,
#                  "Muscimol: C-" = MediumRed,
#                  "Muscimol: AX-"= LightRed,
#                  "Muscimol: B-" = MediumRed,
#                  "Muscimol: BX-"= LightRed,
#                  "Muscimol: X+" = LightRed,
#                  "Muscimol: Y+" = MediumRed,
#                  "Muscimol: Z+" = MediumRed,
#                  "Muscimol: Y-" = White,
#                  "Muscimol: Z-" = White)
# 
# linecolours <- c("Saline: A+" = MediumBlue,
#                  "Saline: A-" = MediumBlue,
#                  "Saline: B+" = MediumBlue,
#                  "Saline: C+" = MediumBlue,
#                  "Saline: C-" = MediumBlue,
#                  "Saline: AX-"= DarkBlue,
#                  "Saline: B-" = DarkBlue,
#                  "Saline: BX-"= DarkBlue,
#                  "Saline: X+" = MediumBlue,
#                  "Saline: Y+" = MediumBlue,
#                  "Saline: Z+" = MediumBlue,
#                  "Saline: Y-" = MediumBlue,
#                  "Saline: Z-" = MediumBlue,
#                  "Muscimol: A+" = MediumRed,
#                  "Muscimol: A-" = MediumRed,
#                  "Muscimol: B+" = MediumRed,
#                  "Muscimol: C+" = MediumRed,
#                  "Muscimol: C-" = MediumRed,
#                  "Muscimol: AX-"= DarkRed,
#                  "Muscimol: B-" = MediumRed,
#                  "Muscimol: BX-"= MediumRed,
#                  "Muscimol: X+" = MediumRed,
#                  "Muscimol: Y+" = MediumRed,
#                  "Muscimol: Z+" = MediumRed,
#                  "Muscimol: Y-" = MediumRed,
#                  "Muscimol: Z-" = MediumRed)
# 
# linetypes <- c("Saline: A+" = "dotted",
#                "Saline: A-" = "dotted",
#                "Saline: B+" = "dotted",
#                "Saline: C+" = "dotted",
#                "Saline: C-" = "dotted",
#                "Saline: AX-"= "dotted",
#                "Saline: B-" = "dotted",
#                "Saline: BX-"= "dotted",
#                "Saline: X+" = "dotted",
#                "Saline: Y+" = "dotted",
#                "Saline: Z+" = "dotted",
#                "Saline: Y-" = "dotted",
#                "Saline: Z-" = "dotted",
#                "Muscimol: A+" = "solid",
#                "Muscimol: A-" = "solid",
#                "Muscimol: B+" = "solid",
#                "Muscimol: C+" = "solid",
#                "Muscimol: C-" = "solid",
#                "Muscimol: AX-"= "solid",
#                "Muscimol: B-" = "solid",
#                "Muscimol: BX-"= "solid",
#                "Muscimol: X+" = "solid",
#                "Muscimol: Y+" = "solid",
#                "Muscimol: Z+" = "solid",
#                "Muscimol: Y-" = "solid",
#                "Muscimol: Z-" = "solid")
# 
# pointshapes <- c("Saline: A+" = circle,
#                  "Saline: A-" = circle,
#                  "Saline: B+" = diamond,
#                  "Saline: C+" = cross,
#                  "Saline: C-" = cross,
#                  "Saline: AX-"= square,
#                  "Saline: B-" = diamond,
#                  "Saline: BX-"= square,
#                  "Saline: X+" = triangleDown,
#                  "Saline: Y+" = triangleUp,
#                  "Saline: Z+" = circle,
#                  "Saline: Y-" = triangleDown,
#                  "Saline: Z-" = circle,
#                  "Muscimol: A+" = circle,
#                  "Muscimol: A-" = circle,
#                  "Muscimol: B+" = diamond,
#                  "Muscimol: C+" = cross,
#                  "Muscimol: C-" = cross,
#                  "Muscimol: AX-"= square,
#                  "Muscimol: B-" = diamond,
#                  "Muscimol: BX-"= square,
#                  "Muscimol: X+" = triangleDown,
#                  "Muscimol: Y+" = triangleUp,
#                  "Muscimol: Z+" = circle,
#                  "Muscimol: Y-" = triangleDown,
#                  "Muscimol: Z-" = circle)
# 
# # Plotting parameters when only using Saline/Muscimol distinction
# fillcolours_infusion <- c("Saline" = MediumBlue,
#                           "Muscimol" = MediumRed)
# 
# linecolours_infusion <- c("Saline" = MediumBlue,
#                           "Muscimol" = MediumRed)
# 
# linetypes_infusion <- c("Saline" = "dotted",
#                         "Muscimol" = "solid")
# 
# pointshapes_infusion <- c("Saline" = square,
#                           "Muscimol" = square)
# 
fillcolours <- c("Saline: A+" = White,
                 "Saline: A-" = White,
                 "Saline: B+" = White,
                 "Saline: C+" = LightGrey,
                 "Saline: C-" = LightGrey,
                 "Saline: AX-"= White,
                 "Saline: B-" = White,
                 "Saline: BX-"= LightGrey,
                 "Saline: X+" = LightGrey,
                 "Saline: Y+" = White,
                 "Saline: Z+" = White,
                 "Saline: Y-" = White,
                 "Saline: Z-" = White,
                 "Muscimol: A+" = DarkGrey,
                 "Muscimol: A-" = DarkGrey,
                 "Muscimol: B+" = DarkGrey,
                 "Muscimol: C+" = MediumGrey,
                 "Muscimol: C-" = MediumGrey,
                 "Muscimol: AX-"= DarkGrey,
                 "Muscimol: B-" = DarkGrey,
                 "Muscimol: BX-"= MediumGrey,
                 "Muscimol: X+" = MediumGrey,
                 "Muscimol: Y+" = DarkGrey,
                 "Muscimol: Z+" = DarkGrey,
                 "Muscimol: Y-" = DarkGrey,
                 "Muscimol: Z-" = DarkGrey)

linecolours <- c("Saline: A+" = Black,
                 "Saline: A-" = Black,
                 "Saline: B+" = Black,
                 "Saline: C+" = Black,
                 "Saline: C-" = Black,
                 "Saline: AX-"= Black,
                 "Saline: B-" = Black,
                 "Saline: BX-"= Black,
                 "Saline: X+" = Black,
                 "Saline: Y+" = Black,
                 "Saline: Z+" = Black,
                 "Saline: Y-" = Black,
                 "Saline: Z-" = Black,
                 "Muscimol: A+" = Black,
                 "Muscimol: A-" = Black,
                 "Muscimol: B+" = Black,
                 "Muscimol: C+" = Black,
                 "Muscimol: C-" = Black,
                 "Muscimol: AX-"= Black,
                 "Muscimol: B-" = Black,
                 "Muscimol: BX-"= Black,
                 "Muscimol: X+" = Black,
                 "Muscimol: Y+" = Black,
                 "Muscimol: Z+" = Black,
                 "Muscimol: Y-" = Black,
                 "Muscimol: Z-" = Black)

linetypes <- c("Saline: A+" = "dotted",
               "Saline: A-" = "dotted",
               "Saline: B+" = "dotted",
               "Saline: C+" = "dotted",
               "Saline: C-" = "dotted",
               "Saline: AX-"= "dotted",
               "Saline: B-" = "dotted",
               "Saline: BX-"= "dotted",
               "Saline: X+" = "dotted",
               "Saline: Y+" = "dotted",
               "Saline: Z+" = "dotted",
               "Saline: Y-" = "dotted",
               "Saline: Z-" = "dotted",
               "Muscimol: A+" = "solid",
               "Muscimol: A-" = "solid",
               "Muscimol: B+" = "solid",
               "Muscimol: C+" = "solid",
               "Muscimol: C-" = "solid",
               "Muscimol: AX-"= "solid",
               "Muscimol: B-" = "solid",
               "Muscimol: BX-"= "solid",
               "Muscimol: X+" = "solid",
               "Muscimol: Y+" = "solid",
               "Muscimol: Z+" = "solid",
               "Muscimol: Y-" = "solid",
               "Muscimol: Z-" = "solid")

pointshapes <- c("Saline: A+" = circle,
                 "Saline: A-" = circle,
                 "Saline: B+" = diamond,
                 "Saline: C+" = circle,
                 "Saline: C-" = circle,
                 "Saline: AX-"= square,
                 "Saline: B-" = diamond,
                 "Saline: BX-"= square,
                 "Saline: X+" = triangleDown,
                 "Saline: Y+" = triangleUp,
                 "Saline: Z+" = circle,
                 "Saline: Y-" = triangleDown,
                 "Saline: Z-" = circle,
                 "Muscimol: A+" = circle,
                 "Muscimol: A-" = circle,
                 "Muscimol: B+" = diamond,
                 "Muscimol: C+" = circle,
                 "Muscimol: C-" = circle,
                 "Muscimol: AX-"= square,
                 "Muscimol: B-" = diamond,
                 "Muscimol: BX-"= square,
                 "Muscimol: X+" = triangleDown,
                 "Muscimol: Y+" = triangleUp,
                 "Muscimol: Z+" = circle,
                 "Muscimol: Y-" = triangleDown,
                 "Muscimol: Z-" = circle)

# Plotting parameters when only using Saline/Muscimol distinction

fillcolours_infusion <- c("Saline" = White,
                          "Muscimol" = DarkGrey)
linecolours_infusion <- c("Saline" = Black,
                          "Muscimol" = DarkGrey)
linetypes_infusion <- c("Saline" = "dotted",
                        "Muscimol" = "solid")
pointshapes_infusion <- c("Saline" = square,
                          "Muscimol" = square)

##### Plot Figures ----

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

##### Main figures ====
stage1_freq <- data_stage1_summary %>% filter(Cue != "Z") %>% 
  ggplot(mapping = aes(x = as.factor(Day), y = CS_Pre, group = GroupCueID, colour = GroupCueID, fill = GroupCueID, shape = GroupCueID,linetype = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-10,20,1)) +
  ggtitle("Stage 1 - Acquisition") + xlab("Day") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(9) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,5.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))  




stage2_freq <- data_stage2_summary %>% filter(Cue != "Z") %>% 
  ggplot(mapping = aes(x = as.factor(trialblock6), y = CS_Pre, group = GroupCueID, colour = GroupCueID, fill = GroupCueID, shape = GroupCueID,linetype = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  facet_wrap(~ as.factor(Day), nrow = 1, strip.position = "top")+
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-10,20,.25)) +
  ggtitle("Stage 2 - Feature Negative") + xlab("Block 6 Trials") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(9) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(-.25,3.2501)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(axis.line.x = element_blank()) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))  +
  theme(strip.background = element_blank(), strip.placement = "inside") 




stage3_freq <- data_stage3_summary %>% filter(Cue != "Z") %>% 
  ggplot(mapping = aes(x = as.factor(trialblock3), y = CS_Pre, group = GroupCueID, colour = GroupCueID, fill = GroupCueID, shape = GroupCueID,linetype = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-10,20,.25)) +
  ggtitle("Reacquisition") + xlab("Block 3 Trials") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(9) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(-.25,3.2501)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))  





summation_freq <- data_stage4_summary %>% filter(!Cue %in%  c("Z", "Y"), 
                               US == "0") %>% 
  ggplot(mapping = aes(x = as.factor(InfusionGroup), y = CS_Pre, group = GroupCueID, colour = GroupCueID ,fill = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-10,20,.25)) +
  ggtitle("Summation") + xlab("Group") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(9) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(-.25,3.2501)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(.5,"line"))  


summation_freq_2trials <- data_stage4_summary_Block2Trials %>% filter(!Cue %in%  c("Z", "Y"), 
                               US == "0",
                               `Trial Number Summation Test Blocked` == 1) %>% 
  ggplot(mapping = aes(x = as.factor(InfusionGroup), y = CS_Pre, group = GroupCueID, colour = GroupCueID ,fill = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "bar", position = "dodge",  size = .3) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.9),  width = 0,  size = .3, colour = "black", linetype = "solid", show.legend = FALSE) + 
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-10,20,1)) +
  ggtitle("Summation") + xlab("Group") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(9) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,5.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(.5,"line"))  


summation_freq_alltrials <- data_stage4_summary_Block2Trials %>% filter(!Cue %in%  c("Z", "Y"), 
                                            US == "0") %>% 
  ggplot(mapping = aes(x = as.factor(`Trial Number Summation Test Blocked`), y = CS_Pre, group = GroupCueID, colour = GroupCueID, fill = GroupCueID, shape = GroupCueID,linetype = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-10,20,1)) +
  ggtitle("Summation") + xlab("Block 2 Trials") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(9) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,5.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))  


retardation_freq <- data_stage5_summary %>% filter(Cue != "Z") %>% 
  ggplot(mapping = aes(x = as.factor(Day), y = CS_Pre, group = GroupCueID, colour = GroupCueID, fill = GroupCueID, shape = GroupCueID,linetype = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-10,20,.25)) +
  ggtitle("Retardation") + xlab("Day") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(9) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(-.25,3.2501)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))  


##### Cue Z figures ====
stage1_freq_z <- data_stage1_summary %>% filter(Cue == "Z") %>% 
  ggplot(mapping = aes(x = as.factor(Day), y = CS_Pre, group = GroupCueID, colour = GroupCueID, fill = GroupCueID, shape = GroupCueID,linetype = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-10,20,.25)) +
  ggtitle("Stage 1 - Acquisition") + xlab("Day") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(9) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(-.25,3.2501)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))  



retardation_freq_z <- data_stage5_summary %>% filter(Cue == "Z") %>% 
  ggplot(mapping = aes(x = as.factor(Day), y = CS_Pre, group = GroupCueID, colour = GroupCueID, fill = GroupCueID, shape = GroupCueID,linetype = GroupCueID)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(-10,20,.25)) +
  ggtitle("Retardation") + xlab("Day") + ylab("Magazine Entry (CS - PreCS)") +
  theme_cowplot(9) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(-.25,3.2501)) +
  theme(axis.title.x=element_text(face = "bold")) +
  scale_linetype_manual(name = "", values = linetypes)  +
  scale_colour_manual(name = "", values = linecolours, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes) +
  scale_fill_manual(name = "", values = fillcolours) +
  theme(legend.key.width=unit(1,"line"))  

#### Locomotor Test Figures ====

locomotor <- data_locomotor_summary %>%
  ggplot(mapping = aes(x = as.factor(Block5mins), y = Distance, group = InfusionGroup, colour = InfusionGroup, fill = InfusionGroup, shape = InfusionGroup,linetype = InfusionGroup)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(0,5000,200)) +
  ggtitle("Distance") + xlab("Block 5 mins") + ylab("Beam breaks") +
  theme_cowplot(9) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,1400.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  scale_linetype_manual(name = "", values = linetypes_infusion)  +
  scale_colour_manual(name = "", values = linecolours_infusion, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes_infusion) +
  scale_fill_manual(name = "", values = fillcolours_infusion) +
  theme(legend.key.width=unit(1,"line"))


locomotor_vertical <- data_locomotor_summary %>%
  ggplot(mapping = aes(x = as.factor(Block5mins), y = `Vertical Counts`, group = InfusionGroup, colour = InfusionGroup, fill = InfusionGroup, shape = InfusionGroup,linetype = InfusionGroup)) +
  stat_summary_bin(fun.data = "mean_se", geom = "line", size = .5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.0, size = .3, linetype = 1, show.legend = FALSE) +
  stat_summary_bin(fun.data = "mean_se", geom = "point", size = 2) +
  # Make Pretty
  scale_y_continuous( expand = expansion(mult = c(0, 0)), breaks=seq(0,5000,5)) +
  ggtitle("Rearing") + xlab("Block 5 mins") + ylab("Beam breaks") +
  theme_cowplot(9) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=8)) +
  coord_cartesian(ylim = c(0,35.0001)) +
  theme(axis.title.x=element_text(face = "bold")) +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  scale_linetype_manual(name = "", values = linetypes_infusion)  +
  scale_colour_manual(name = "", values = linecolours_infusion, aesthetics = c("colour")) +
  scale_shape_manual(name = "", values = pointshapes_infusion) +
  scale_fill_manual(name = "", values = fillcolours_infusion) +
  theme(legend.key.width=unit(1,"line"))



##### Figure Layout and Save ----

A = stage1_freq + theme(legend.position= c(0.05,.9), 
                        legend.justification='left',
                        legend.direction='vertical') 

B = stage2_freq 

B = shift_xaxis_facet(B, y =0)


C = stage3_freq + theme(legend.position= c(0.05,.95), 
                        legend.justification='left',
                        legend.direction='vertical', 
                        axis.title.y = element_blank(),
                        axis.text.y = element_blank())

C = shift_xaxis(C, y =0)

D = summation_freq + theme(legend.position= c(0.35,0.95), 
        legend.justification='left',
        legend.direction='vertical', 
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

D = shift_xaxis(D, y =0)

E = retardation_freq + theme(legend.position= c(0.05,.95), 
                             legend.justification='left',
                             legend.direction='vertical', 
                             axis.title.y = element_blank(),
                             axis.text.y = element_blank())

E = shift_xaxis(E, y =0)

# summation_freq_2trials
# summation_freq_alltrials

Fig4 <- B + C + D + E + plot_annotation(tag_levels = 'A') + plot_layout(nrow = 1, widths = c(2,.75,.9,.75))


filename = here("figures", "Fig4.png")
ggsave(filename, Fig4, width =  220, height = 100, units = "mm", dpi = 1200)

filename = here("figures", "Fig4.pdf")
ggsave(filename, Fig4, width =  220, height = 100, units = "mm")



# Acquisition saved as supplementary figure
Fig_S4 <- A


filename = here("figures", "Fig_S4.png")
ggsave(filename, Fig_S4, width =  80, height = 80, units = "mm", dpi = 1200)

filename = here("figures", "Fig_S4.pdf")
ggsave(filename, Fig_S4, width =  80, height = 80, units = "mm")




#Figure 2 Cue Z
A2<-stage1_freq_z + theme(legend.position= c(0.05,.95), 
                        legend.justification='left',
                        legend.direction='vertical') 

A2 = shift_xaxis(A2, y =0)

D2<-retardation_freq_z + theme(legend.position= c(0.05,.95), 
                             legend.justification='left',
                             legend.direction='vertical', 
                             axis.title.y = element_blank(),
                             axis.text.y = element_blank())
D2 = shift_xaxis(D2, y =0)


Fig_S6_Expt2CueZ <- (A2 + D2) + plot_annotation(tag_levels = 'A')+ plot_layout(nrow = 1, widths = c(2,1))

filename = here("figures", "Fig_S6_Expt2CueZ.png")
ggsave(filename, Fig_S6_Expt2CueZ, width =  86, height = 80, units = "mm", dpi = 1200) 

filename = here("figures", "Fig_S6_Expt2CueZ.pdf")
ggsave(filename, Fig_S6_Expt2CueZ, width =  86, height = 80, units = "mm") 

# Locomotor activity
S3A <-locomotor + theme(legend.position= c(0.05,.95), 
                             legend.justification='left',
                             legend.direction='vertical')
S3B <- locomotor_vertical + theme(legend.position= c(0.05,.95), 
                         legend.justification='left',
                         legend.direction='vertical')

Fig_S5 <- S3A + S3B + plot_annotation(tag_levels = 'A') + plot_layout(nrow = 1, widths = c(1,1))

filename = here("figures", "Fig_S5.png")
ggsave(filename, Fig_S5,  width =  120, height = 80, units = "mm", dpi = 1200) 

filename = here("figures", "Fig_S5.pdf")
ggsave(filename, Fig_S5,  width =  120, height = 80, units = "mm") 



##### Save Plot Data as CSV for publication ----
data_Fig_S4 <- data_stage1_summary %>% filter(Cue != "Z")
data_Fig4A <- data_stage2_summary %>% filter(Cue != "Z")
data_Fig4B <- data_stage3_summary %>% filter(Cue != "Z") 
data_Fig4C <- data_stage4_summary %>% filter(!Cue %in%  c("Z", "Y"), US == "0")
data_Fig4D <- data_stage5_summary %>% filter(Cue != "Z")

data_Fig_S6A_Expt2CueZ <- data_stage1_summary %>% filter(Cue == "Z")
data_Fig_S6B_Expt2CueZ <- data_stage5_summary %>% filter(Cue == "Z")

data_Fig_S5AB <- data_locomotor_summary



dataset_name <-  c("data_Fig_S4",
                   "data_Fig4A",
                   "data_Fig4B",
                   "data_Fig4C",
                   "data_Fig4D",
                   "data_Fig_S6A_Expt2CueZ",
                   "data_Fig_S6B_Expt2CueZ",
                   "data_Fig_S5AB")


for (i in dataset_name) { 
  filename = here("figures", "figure_data", str_c(i,".csv"))
  temp_data = as.name(i)
  write_csv(eval(temp_data), filename)
}

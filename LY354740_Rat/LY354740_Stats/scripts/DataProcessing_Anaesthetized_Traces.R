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



# Load average traces from Awake FCV Data set and convert from wide format to long format
full_data_wide <- read_csv("rawdata/WideFormatData/LY_Anaesthetized_AvgTraces.csv")

full_data_long <- full_data_wide %>% 
  pivot_longer(3:14, names_to = "UniqueID", values_to = "DA") %>% 
  mutate(splitme = UniqueID,
         Time = Time - 5) %>% 
  separate(splitme, c("drug", "Subj")) 
  
# Save as long format
write_csv(full_data_long, "rawdata/Anaesthetized_Traces.csv")


#####

# Load wide format representative CV
full_data_wide <- read_csv(here("rawdata", "WideFormatData", "/LY354740_Anaesthetized_representativeCVs.csv"))
full_data_long <- full_data_wide %>% 
  pivot_longer(2:3, names_to = "drug", values_to = "nA")

# Svae as long format
write_csv(full_data_long, "rawdata/LY354740_Anaesthetized_representativeCVs.csv")


# Load wide formay representative trace
full_data_wide <-read_csv(here("rawdata", "WideFormatData", "/LY354740_Anaesthetized_representativeTraces.csv"))
full_data_long <- full_data_wide %>% 
  pivot_longer(2:3, names_to = "drug", values_to = "nA")

# Svae as long format
write_csv(full_data_long, "rawdata/LY354740_Anaesthetized_representativeTraces.csv")


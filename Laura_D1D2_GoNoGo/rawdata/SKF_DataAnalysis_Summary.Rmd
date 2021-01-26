---
title: "Laura_SKF_GoNoGo_ErrorTrials"
author: "Marios Panayi"
date: "1/26/2021"
output: word_document
---

```{r setup, include=FALSE, message = FALSE, warning=FALSE}
knitr::opts_chunk$set(echo = FALSE)
## Packages for data organisation and plotting
library(tidyverse)
# Package for relative file paths
library(here)
# library(ggpubr)
library(cowplot)
library(ggsignif)
library(patchwork)
################################################################################
## Packages for Data analysis
library(afex)
afex_options(emmeans_model = "multivariate")# use multivariate model for all follow-up tests.
library(emmeans)
# install.packages("devtools")
# devtools::install_github("crsh/papaja")
library(papaja)
library(knitr)
# remotes::install_github("noamross/redoc")
# library(redoc)
```





```{r Load data for Stats , include=FALSE, message = FALSE, warning=FALSE}
datafolder <- "rawdata"
filename <- "SKF_errordata.csv"
rawdata <- fread(here(datafolder,filename))

```
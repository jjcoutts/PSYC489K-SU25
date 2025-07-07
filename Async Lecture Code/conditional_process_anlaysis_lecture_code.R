# R script created by Jacob J. Coutts (Copyright 2025)
# Conditional Process Analysis Lecture (PSYC489)

# load required packages 
library(ggplot2)
library(jtools)
source("~/PSYC489K-SU25/process_v5.R")
# Make sure the source command above routes to where PROCESS is located in YOUR computer/environment

# read in data and summarize - be sure to change file path to appropriate location on your computer
selfc <- read.csv(file="~/PSYC489K-SU25/Datasets/self_compassion.csv")
summary(selfc)

# fit the model manually
m_mod <- lm(scc_TOT ~ self_TOT*cesd_TOT, data =selfc); summary(m_mod)
y_mod <- lm(swl_TOT ~ self_TOT*cesd_TOT + scc_TOT*cesd_TOT, data = selfc); summary(y_mod)
# both interaction effects sig, significant by way of joint test

# fit the model in PROCESS
process(data = selfc, y = "swl_TOT", x = "self_TOT", m = "scc_TOT", w = "cesd_TOT", model = 59, boot = 10000, seed = 489, progress = 0)
# model 59 is a dual-stage conditional IE with the direct effect moderated

### end of script 
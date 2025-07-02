# R script created by Jacob J. Coutts (Copyright 2025)
# Moderation Lecture II (PSYC489)

# load required packages 
library(ggplot2)
library(jtools)

# Make sure the source command above routes to where PROCESS is located in YOUR computer
source("~/PSYC489K-SU25/process_v5.R")

# read in data and summarize - be sure to change file path to appropriate location on your computer
vgames <- read.csv(file="~/PSYC489K-SU25/Datasets/videogames.csv")
# summarize the model
summary(vgames)

# we need to mean center the perceived performance of the other play so it has a meaningful interpretation
vgames$perform_o_cen <- vgames$perform_o - mean(vgames$perform_o)
mean(vgames$perform_o_cen)
# the correlation between perform_o and perform_o_cen with other variables is different--it seems like centering reduces colinearity but as we'll see below it has no effect on the interaciton because the variance inflation factor (VIF) also changes. 
cor(vgames)


############ Simple moderation with perceived performance (continuous moderator centered vs. not) - manual ############ 
### modeling
process(data = vgames, x = "comp_cond",w = "perform_o", y = "like", model = 1, plot =1, center=1, jn =1,moments =1) 
process(data = vgames, x = "comp_cond",w = "perform_o", y = "like", model = 1, plot =1, jn =1,moments =1) 
# the interaction effect does not change and is still significant


############ Multiple moderation with perceived performance and losing disposition - Additive (two two-way interactions) ############ 
additive_mod <- lm(like ~ comp_cond*perform_o_cen + comp_cond*hate_lose, data = vgames)
summary(additive_mod)
# now fit it in PROCESS
process(data = vgames, x = "comp_cond",w = "perform_o_cen", z= "hate_lose", y = "like", model = 2, plot =1,moments =1) 


############ Multiple moderation with perceived performance and losing disposition - Multiplicative (three-way interaction) ############ 
mutiply_mod <- lm(data = vgames, like~comp_cond*hate_lose*perform_o_cen) # fit the model 
summary(mutiply_mod) # check the results
# now fit it in PROCESS
process(data = vgames, x = "comp_cond",w = "perform_o_cen", z= "hate_lose", y = "like", model = 3, plot =1,moments =1) 


### end of script 
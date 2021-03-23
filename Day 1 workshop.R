###
# Readme: This first of the two workshops should provide you with an basic understanding of some of basics of R and 
# you can load data and set up data in a way that is appropriate for your analysis. 
###


# Working with directories in R
getwd()
list.files()

# Distinction between relative vs absolute path
setwd('../../Bruce Peng/Google Drive/Phd (1)/Stats/Multilevel modeling/Multilevel modeling workshop for SUNY/')
setwd ('C:/Users/Bruce Peng/Google Drive/Phd (1)/Stats/Multilevel modeling/Multilevel modeling workshop for SUNY/')

# Reading data file 
IPE.df <- read.csv('IPE&2_fullData_N=40.csv', header=T)

# Get information from your datafile
summary(IPE.df)
dim(IPE.df)

# Ensure there are no missing cases from our data file and there are expected number of participants and stimuli
IPE.df<- IPE.df[complete.cases(IPE.df$Condition),]
dim(IPE.df)
summary(IPE.df)
unique(IPE.df$Participant)
unique(IPE.df$Story_ID)
sort(unique(IPE.df$Story_ID))

# Make sure all variables are in the correct format
str(IPE.df)
IPE.df$Story_ID<- as.factor(IPE.df$Story_ID)
IPE.df$Gender<- as.factor (IPE.df$Gender)
IPE.df$Version <- as.factor(IPE.df$Version)
str(IPE.df)

levels(IPE.df$Condition)
IPE.df$Condition<- relevel(IPE.df$Condition, ref= 'Identify')
levels(IPE.df$Condition)

# Loading packages
install.packages(c('lme4', 'lmerTest','emmeans','ggeffects','sjstats', 'sjPlot'))
install.packages('lme4') 
if (!require("lme4")) install.packages("lme4")
library(lme4)
library(lmerTest)
library(emmeans)
library(lattice)
library (ggeffects)
library(sjstats)
library(sjPlot)

#sourcing outside codes
source("basenull_logtest.R")
source("group_center.R")
source("means.R")

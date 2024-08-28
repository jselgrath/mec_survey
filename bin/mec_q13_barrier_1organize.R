# Equity in Ocean Access (MPAs Equity and Climate (mec))
# Jennifer Selgrath - base code from Tim Frawley
# California Marine Sanctuary Foundation/ CINMS

# goal: subset cleaned data for graphing and making long version - 

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)
library(likert) 

# --------------------------------------------------------------------------
# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/mec_surveys")

###Load in Data file
d1<-read_csv("./results/data_wide.csv")%>%
  select(Q1:Format,ResponseId)%>%  # remove metadata from surveys
  glimpse()

names(d1)

##transpose Data so that each column is a survey response and each row is a prompt
d0<-as.data.frame(t(d1))%>%
  glimpse()
d0[1:10,]


# Question list ##### -------------------------------------------
# Q1: time desired
# Q2: time spent
# Q3: location
# Q4: activities
# Q5: one activity
# Q6: transportation
# Q7: travel time
# Q8: mentorship
# Q9: companionship
# 5a: fishing method
# 5b: species: fishing use
# 5b.2 species: cultural 
# 5b.3: species: observing photo. education
# 5b.4: species: snorkling/scuba
# Q10: digital
# Q11: wellbeing
# Q12: ecosystem services
# Q13: barriers
# Q14: microaggressions
# Q15: observed changes
# Q16: anticipated changes
# Q17: state MPAs
# Q18: sanctuaries
# Q19: ranking management
# Q20: climate causes
# Q20a: climate perceptions
# Q21: confidence
# Q22: zip code
# Q23: years in CA
# Q24: race
# 24a: Asian
# Q25: gender
# Q26: year born
# Q27: income
# Q28: education
# Q29: household size
# Q30: household age
# Q31: swimming
# Q32: thoughts
# Q33: management


# -------------------------------------------------------
##Retain Columns w/ Data you Want to Work With
# barriers -----------------------------------

#select barriers question --------------------------
d13<-d1%>%
  select(starts_with("Q13"))%>%
  glimpse()
d13


##Transform Data so that each column is a survey response and each row is a prompt
d13a<-as.data.frame(t(d13))%>%
  mutate(barrier=V1)%>%
  select(-V1)%>%
  pivot_longer(cols = !c(barrier))%>%
  select(-name)%>% # keep to link to demographics
  # filter(!is.na(value))%>%
  glimpse()

d13a$value[d13a$value=="Neither agree nor disagree"]<-"Neutral"



# repeat for LOW income ------------------
unique(d1$Q27)

d13_low<-d1%>%
  filter(Q27=="Less than $59,999" | is.na(Q27))%>%
  select(starts_with("Q13"))%>%
  glimpse()

#Transform Data so that each column is a survey response and each row is a prompt
d13a_low<-as.data.frame(t(d13_low))%>%
  mutate(barrier=V1)%>%
  select(-V1)%>%
  pivot_longer(cols = !c(barrier))%>%
  select(-name)%>% # keep to link to demographics
  # filter(!is.na(value))%>%
  glimpse()
d13a_low

d13a_low$value[d13a_low$value=="Neither agree nor disagree"]<-"Neutral"


# save -----------------------------
write_csv(d13a,"./results/q13_barrier_long.csv")
write_csv(d13a_low,"./results/q13_barrier_long_low.csv")

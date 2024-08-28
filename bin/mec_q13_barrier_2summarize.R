# Equity in Ocean Access (MPAs Equity and Climate (mec))
# Jennifer Selgrath - base code from Tim Frawley
# California Marine Sanctuary Foundation/ CINMS

# goal: summarizing q12 es data

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)
library(likert) 
library(colorspace)

# ---------------------------------------------------------
# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/mec_surveys")

###Load in Data file -------------------
d1<-read_csv("./results/q13_barrier_long.csv")%>%
  glimpse()

# summarize -------------------

# n respondents - includes NA --------------
d1c<-d1%>%
  group_by(barrier)%>%
  summarize(
    n=n())%>%
  glimpse() #586


# responses - includes NA --------------
d1d<-d1%>%
  group_by(barrier,value)%>%
  summarize(
    n_val=n())%>%
  mutate(pct=round(n_val/d1c$n[1],3))%>%
  glimpse()

# n respondents - no NA --------------
d1e<-d1%>%
  filter(!is.na(value))%>%
  group_by(barrier)%>%
  summarize(
    n_tot=n())%>%
  glimpse() 

# responses - no NA --------------
d1f<-d1%>%
  filter(!is.na(value))%>%
  group_by(barrier,value)%>%
  summarize(
    n_val=n())%>%
  full_join(d1e)%>%
  mutate(pct=round(n_val/n_tot,3))%>%
  glimpse()


# -----------------------------------------------
# prep for graphing ------------------------------
source("./bin/deets.R")


# # order factors ---------------------------
d1f$value <- factor(d1f$value, levels = c("Strongly agree", "Agree", "Neutral", "Disagree", "Strongly disagree"))

d1f
# view(d1f)

#make Disagree and Strongly disagree percentages negative show up of the left side of the y-axis
d1g<-d1f%>%
  group_by(barrier)%>%
  reframe(pct2=c(pct[value=='Strongly agree'],
                   pct[value=='Agree'],
                   pct[value=='Neutral']/2,
                   -pct[value=='Neutral']/2,
                   -pct[value=='Disagree'],
                   -pct[value=='Strongly disagree']),
          value = c('Strongly agree','Agree', 'Neutral', 'Neutral', 'Disagree','Strongly disagree'),
          overall = sum(pct2)) %>%
  mutate(barrier = reorder(barrier, overall))%>%
  glimpse()

d1g$value<-ordered(d1g$value, levels = c("Strongly agree", "Agree", "Strongly disagree", "Disagree","Neutral"))


##Order Prompts so the highest level of "Strongly agree" is at the top
Factor_Order<-d1g[which(d1g$value=="Strongly agree"),]
Factor_Order<-Factor_Order[order(Factor_Order$pct2),]
Order<-Factor_Order$barrier
d1g$barrier <- ordered(d1g$barrier, levels=Order)

# graph -------------------------------------
source("./bin/deets.R")

ggplot(d1g, aes(y=barrier, x=pct2, fill=value)) + 
  geom_col(orientation = 'y', width = 0.6) +
  geom_bar(stat="identity") +
scale_fill_manual(
    # values = c( "#fe6100","#ffb000", "grey50", "#648fff", "#785ef0"),
    values = c( "#002F70", "#879FDB", "grey50", "#DA8A8B", "#5F1415"),
    breaks=c('Strongly agree', 'Agree', 'Neutral', 'Disagree', "Strongly disagree"),
    name="")+
  geom_vline(xintercept = 0) +
  xlab("% of Respondents") + 
  ylab("")+
  xlim(c(-1,1))+
  theme_bw() + 
  ggtitle("Use and/or experience within ocean and \ncoastal areas in California:")+
  deets9

ggsave("./doc/q13_barrier.png", width=12, height=4.5, units="in")
# ggsave("./doc/q13_barrier_low.png", width=12, height=4.5, units="in")



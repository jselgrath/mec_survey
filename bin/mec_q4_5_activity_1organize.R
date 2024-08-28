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

#select fishing activities and related questions --------------------------

# fishing is an activity
d2<-d1%>%
  select(starts_with("Q4") | starts_with("Q5")| starts_with("Q8") | starts_with("Q9")| starts_with("Q24")| starts_with("Q25")| starts_with("Q26")| starts_with("Q27")| starts_with("Q28")|"ResponseId")%>%
  filter(grepl("Fishing or collecting food",Q4))%>% # select only activities where fishing or collecting food is important
  select(Q4:Q5c,Q8:ResponseId)%>% # remove other types of important species (non-fishery)
  arrange(Q5b.1._4)%>% # arrange by first caught species
  glimpse()
d2

# only ones with species
d2a<-d2%>%
  filter(!is.na(Q5b.1._4)) 

# most important activity
d2c<-d2%>%
  filter(Q5=="Fishing or collecting food") 

length(unique(d2$ResponseId)) # check this is unique




# SPECIES ##### --------------------------------
# notes on species from Kevin -----------------------------
# brass are croaker 
# whitefish == a fish
# people catch Striped bass in aquaducts and surf line, river mouths. anadramous. get a lot at caistic lake 
# Rock cod = rockfish - rockcod are 500 ft or deeper. before depth limits shallow rock fish and deep rockfish
# Red snapper = Vermillion rockfish. catch in vanderberg in 30 ft


# clean species -------------------

unique (d2$Q5b.1._4)

d3<-d2%>%
  mutate(Q5b.1._4=if_else(Q5b.1._4=="Dungeness","Dungeness crab",Q5b.1._4),
         Q5b.1._4=if_else(Q5b.1._4=="Leopard Shark","Leopard shark",Q5b.1._4),
         Q5b.1._4=if_else(Q5b.1._4=="Mackarel","Mackerel",Q5b.1._4),
         Q5b.1._4=if_else(Q5b.1._4=="Makarel","Mackerel",Q5b.1._4),
         Q5b.1._4=if_else(Q5b.1._4=="mackerel","Mackerel",Q5b.1._4),
         Q5b.1._4=if_else(Q5b.1._4=="Surf Perch","Surf perch",Q5b.1._4),
         Q5b.1._4=if_else(Q5b.1._4=="Thresher","Shark - thresher",Q5b.1._4),
         Q5b.1._4=if_else(Q5b.1._4=="Thresher Shark","Shark - thresher",Q5b.1._4),
         Q5b.1._4=if_else(Q5b.1._4=="Spanish Markel","Spanish mackerel",Q5b.1._4),
         Q5b.1._4=if_else(Q5b.1._4=="Stripper bass","Striped bass",Q5b.1._4),
         Q5b.1._4=if_else(Q5b.1._4=="Stripper","Striped bass",Q5b.1._4),
         Q5b.1._4=if_else(Q5b.1._4=="Striper","Striped bass",Q5b.1._4),
         Q5b.1._4=if_else(Q5b.1._4=="Rock Fish","Rockfish",Q5b.1._4),         
         Q5b.1._4=if_else(Q5b.1._4=="Rock fish","Rockfish",Q5b.1._4),
         Q5b.1._4=if_else(Q5b.1._4=="Muscles","Mussels",Q5b.1._4),
         Q5b.1._4=if_else(Q5b.1._4=="海貝","Shells",Q5b.1._4), #sea shells
         Q5b.1._4=if_else(Q5b.1._4=="Conchitas","Shells",Q5b.1._4),
         Q5b.1._4=if_else(Q5b.1._4=="salmon","Salmon",Q5b.1._4),
         Q5b.1._4=if_else(Q5b.1._4=="Share","Shark",Q5b.1._4),
         Q5b.1._4=if_else(Q5b.1._4=="unagi","Sea urchin",Q5b.1._4),
         Q5b.1._4=if_else(Q5b.1._4=="Bass","Seabass",Q5b.1._4),  
         Q5b.1._4=if_else(Q5b.1._4=="Cod","Rockfish",Q5b.1._4),    # used interchangably
         Q5b.1._4=if_else(Q5b.1._4=="Rock cod","Rockfish",Q5b.1._4), 
         Q5b.1._4=if_else(Q5b.1._4=="Macro","Mackerel",Q5b.1._4), 
         Q5b.1._4=if_else(Q5b.1._4=="Sardine","Sardines",Q5b.1._4), 
         Q5b.1._4=if_else(Q5b.1._4=="Cangrejo","Crab",Q5b.1._4)      
         )%>%
  glimpse()

glimpse(filter(d3,Q5b.1._4=="Share")) # unclear what this is
unique (d3$Q5b.1._4)

# clean second  column of species -----------------------
d3<-d3%>%
  arrange(Q5b.1._5) # make list alphabetical

unique(d3$Q5b.1._5)

d4<-d3%>%
  
  mutate(Q5b.1._5=if_else(Q5b.1._5=="Cabazone","Cabezon",Q5b.1._5),
         Q5b.1._5=if_else(Q5b.1._5=="Ling cod","Lingcod",Q5b.1._5),
         Q5b.1._5=if_else(Q5b.1._5=="Ling Cod","Lingcod",Q5b.1._5),
         Q5b.1._5=if_else(Q5b.1._5=="Mackrel","Mackerel",Q5b.1._5),
         Q5b.1._5=if_else(Q5b.1._5=="Lasgosta","Lobster",Q5b.1._5),  # Langosta = lobster
         Q5b.1._5=if_else(Q5b.1._5=="Roca","Rockfish",Q5b.1._5), # seems most likely
         Q5b.1._5=if_else(Q5b.1._5=="Surf Perch","Surf perch",Q5b.1._5),
         Q5b.1._5=if_else(Q5b.1._5=="Thresher","Shark - thresher",Q5b.1._5),
         Q5b.1._5=if_else(Q5b.1._5=="Tiburon","Shark",Q5b.1._5),
         Q5b.1._5=if_else(Q5b.1._5=="Spanish Sardines","Spanish sardines",Q5b.1._5),
         Q5b.1._5=if_else(Q5b.1._5=="海參","Sea cucumber",Q5b.1._5),
         Q5b.1._5=if_else(Q5b.1._5=="Stripers","Striped bass",Q5b.1._5),
         Q5b.1._5=if_else(Q5b.1._5=="Striper","Striped bass",Q5b.1._5),
         Q5b.1._5=if_else(Q5b.1._5=="Striped Bass","Striped bass",Q5b.1._5),         
         Q5b.1._5=if_else(Q5b.1._5=="Rock fish","Rockfish",Q5b.1._5),
         Q5b.1._5=if_else(Q5b.1._5=="crab","Crab",Q5b.1._5),
         Q5b.1._5=if_else(Q5b.1._5=="fish","Fish",Q5b.1._5),
         Q5b.1._5=if_else(Q5b.1._5=="Mussel","Mussels",Q5b.1._5),
         Q5b.1._5=if_else(Q5b.1._5=="spanish sardines","Spanish sardines",Q5b.1._5),
         Q5b.1._5=if_else(Q5b.1._5=="Cod","Rockfish",Q5b.1._5), 
         
  )%>%
  arrange(Q5b.1._5) %>%
  glimpse()

unique(d4$Q5b.1._5)

# outstanding: "Striped bass and perch", "Merou", "Spanish sardines"
# is rock cod rockfish?

# clean third column of species -----------------------
d4<-d4%>%
  arrange(Q5b.1._6) # make list alphabetical

unique(d4$Q5b.1._6)

d5<-d4%>%
    mutate(Q5b.1._6=if_else(Q5b.1._6=="Dungeness crabs","Dungeness crab",Q5b.1._6),
         # Q5b.1._6=if_else(Q5b.1._6=="Jalabe","XX",Q5b.1._6),
         Q5b.1._6=if_else(Q5b.1._6=="Monkey face Prickleback Eels","Monkey face prickleback eels",Q5b.1._6),
         Q5b.1._6=if_else(Q5b.1._6=="Pescado","Fish",Q5b.1._6),
         Q5b.1._6=if_else(Q5b.1._6=="Pulpo","Octopus",Q5b.1._6),  # Langosta = lobster
         Q5b.1._6=if_else(Q5b.1._6=="Rock fish","Rockfish",Q5b.1._6), # seems most likely
         Q5b.1._6=if_else(Q5b.1._6=="Sardine","Sardines",Q5b.1._6),
         Q5b.1._6=if_else(Q5b.1._6=="Thresher","Shark - thresher",Q5b.1._6),
         Q5b.1._6=if_else(Q5b.1._6=="Sharks","Shark",Q5b.1._6),
         Q5b.1._6=if_else(Q5b.1._6=="Uni","Sea urchin",Q5b.1._6),
         Q5b.1._6=if_else(Q5b.1._6=="Spanish Macarons","Spanish mackerel",Q5b.1._6),
         Q5b.1._6=if_else(Q5b.1._6=="Stripper","Striped bass",Q5b.1._6),
         Q5b.1._6=if_else(Q5b.1._6=="Striper","Striped bass",Q5b.1._6),
         Q5b.1._6=if_else(Q5b.1._6=="mussel","Mussels",Q5b.1._6),         
         # Q5b.1._6=if_else(Q5b.1._6=="pers ya","XX",Q5b.1._6),
         Q5b.1._6=if_else(Q5b.1._6=="Rock crabs","Rock crab",Q5b.1._6),
         Q5b.1._6=if_else(Q5b.1._6=="rock crabs","Rock crab",Q5b.1._6),
         Q5b.1._6=if_else(Q5b.1._6=="seaweed","Seaweed",Q5b.1._6),
         Q5b.1._6=if_else(Q5b.1._6=="mussel","Musssels",Q5b.1._6),
         Q5b.1._6=if_else(Q5b.1._6=="螃蟹","Crab",Q5b.1._6),
         Q5b.1._6=if_else(Q5b.1._6=="Star fish","Seastar",Q5b.1._6),   
         Q5b.1._6=if_else(Q5b.1._6=="Stripe bass","Striped bass",Q5b.1._6),
         Q5b.1._6=if_else(Q5b.1._6=="Red snapper","Rockfish - Vermillion",Q5b.1._6),
         Q5b.1._6=if_else(Q5b.1._6=="Stripe bass","Striped bass",Q5b.1._6),
         Q5b.1._6=if_else(Q5b.1._6=="Cod","Rockfish",Q5b.1._6),     
         
  )%>%
  glimpse()

unique(d5$Q5b.1._6)
# outstanding questions: Jalabe, Carp, Catfish, Crappie

# select ID and species, make long format and clean --------------------

d6<-d5%>%
  select(ResponseId,Q5b.1._4,Q5b.1._5,Q5b.1._6)%>%
  mutate(Q5b.1._new=NA)%>%
  mutate(Q5b.1._new=if_else(Q5b.1._5=="Striped bass and perch","Perch",Q5b.1._new),
         Q5b.1._5=if_else(Q5b.1._5=="Striped bass and perch","Striped bass",Q5b.1._5))%>%
  mutate(Q5b.1._new=if_else(Q5b.1._4=="Carpe, tilapia","Tilapia",Q5b.1._new),
         Q5b.1._4=if_else(Q5b.1._4=="Carpe, tilapia","Carp",Q5b.1._4))%>%
  mutate(Q5b.1._new=if_else(Q5b.1._5=="Spanish sardines","Sardines",Q5b.1._new),
         Q5b.1._5=if_else(Q5b.1._5=="Spanish sardines","Spanish mackerel",Q5b.1._5))%>%
  glimpse()
d4$Q5b.1._5

unique(d6$Q5b.1._new)



##Pivot long
d7<-d6%>%
  pivot_longer(col=!ResponseId,names_to="question",values_to="species")%>%
  filter(!is.na(species))%>%
  mutate(species=if_else(species=="Bass","Sea bass",species),
         species=if_else(species=="Cod","Rockfish",species),
         species=if_else(species=="Rock cod","Rockfish",species))%>%
  arrange(species)%>%
  glimpse()

unique(d7$species)

# remove non-fished species
d8_non<-d7%>%
  filter(species=="Birds"| species == "Dolphins"| species == "Turtle" | species == "Mammals")

# freshwater
d8_fresh<-d7%>%
  filter(species=="Carp"| species == "Crappie" | species == "Catfish"| species == "Trout"| species == "Tilapia")

# unclear species
d8_unclear<-d7%>%
  filter(species=="pers ya"| species == "Merou" | species == "Jalabe"| species == "Trout" ) # Jalabe - Halibut??

# clean species
d8<-d7%>%
  filter(species!="Birds"& species != "Dolphin"& species != "Dolphins"& species != "Turtle" &species != "Mammals"& species!="Carp"& species != "Crappie" & species != "Catfish"& species != "Trout"& species!="pers ya"& species != "Merou" & species != "Jalabe"& species != "Trout"&species != "Tilapia" ) %>%
  mutate(species=if_else(species=="Sea bass","Seabass", species))%>%
  mutate(species_type="Fish")%>%
  mutate(species_type=if_else(species=="Clam"|species=="Crab"|species=="Dungeness crab"|species=="Lobster"|species=="Mussels"|species=="Octopus"|species=="Red crab"|species=="Rock crab"|species=="Sea cucumber"|species=="Sea urchin"|species=="Seastar"|species=="Shells","Invertebrate",species_type))%>%
  mutate(species_type=if_else(species=="Seaweed","Algae",species_type) )%>% 
  arrange(species)%>%
  glimpse()

unique(d8$species)
unique(d8$species_type)


# GENERAL INFO #####---------------------------
d9<-d5%>%
  select(Q4,Q5,Q5a,Q5c:ResponseId)%>% #non-species info
  glimpse()


# save
write_csv(d8,"./results/species_caught.csv")
write_csv(d9,"./results/fishing_culture.csv") # includes people where fishing was NOT the most important activity

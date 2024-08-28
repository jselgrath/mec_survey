# Equity in Ocean Access (MPAs Equity and Climate Change (mec))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: driver file for code to analyze surveys about ocean access done in the summer and fall of 2024 

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
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/mec_survey")

# import joined data from qualitrics, remove low quality data, and organize headers
source("./bin/mec_clean_data.R")
# input:        ./data/Combined_Data_8.27.24.csv
# output:       ./results/data_wide.csv

# Ecosystem services ------------------------------
# organize ecosystem services data 
source("./bin/mec_q12_es_1organize.R")
# input:      ./results/data_wide.csv
# output:     ./results/q12_es_long.csv
#             ./results/q12_es_long_low.csv

# summarize and graph ecosystem services data & separate code for low income only
source("./bin/mec_q12_es_1summarize.R")
# input:       ./results/q12_es_long.csv
# output:  

# Barriers ------------------------------
# organize barrier data
source("./bin/mec_q13_barrier_1organize.R")
# input:      ./results/data_wide.csv
# output:     ./results/q12_barrier_long.csv
#             ./results/q12_barrier_long_low.csv

# summarize and graph barrier data & separate code for low income only
source("./bin/mec_q13_barrier_2summarize.R")
# input:       ./results/q12_barrier_long.csv
# output:      ./doc/q13_barrier_low.png
#              ./doc/q13_barrier.png 


# Activities ------------------------------
# select activities where fishing is important, clean species names
source("./bin/mec_q4_5_activity_1organize.R")
# input:    ./results/data_wide.csv    
# output:   ./results/species_caught.csv
#           ./results/fishing_culture.csv

# input:        
# output:  

# input:        
# output:  

# input:        
# output:  
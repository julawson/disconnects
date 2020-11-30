###Loading Packages###
library(here)
library(tidyverse)
library(reshape2)

#Importing Overall RFMO Data
iattc_all <- read.csv(here("02_processed-data","iattc_all.csv")) 
iotc_all <- read.csv(here("02_processed-data","iotc_all.csv")) 
wcpfc_all <- read.csv(here("02_processed-data","wcpfc_all.csv")) 

#Importing Overall CITES/CMS Data

###Leaving for later.

#Importing Species-Specific RFMO Data
iattc_prop <- read.csv(here("02_processed-data","iattc_prop_long.csv")) 
iotc_prop <- read.csv(here("02_processed-data","iotc_prop_long.csv")) 
wcpfc_prop <- read.csv(here("02_processed-data","wcpfc_prop_long.csv")) 

#Importing Species-Specific CMS/CITES Data
cons_state <- read.csv(here("02_processed-data","cons_state.csv")) 

#Combining for Carcharhinus falciformis
cfalci <- cons_state %>% 
  filter()
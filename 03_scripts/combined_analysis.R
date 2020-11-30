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
iattc_prop <- read.csv(here("02_processed-data","iattc_prop_long.csv")) %>% 
  rename("Species" = "Species.New") %>% 
  rename("CountryName" = "Country") %>% 
  rename("iattcValue" = "value") %>% 
  rename("iattcYear" = "Year") %>% 
  select(CountryName, Species, iattcValue)
iotc_prop <- read.csv(here("02_processed-data","iotc_prop_long.csv")) %>% 
  rename("Species" = "Species.New") %>% 
  rename("CountryName" = "Country") %>% 
  rename("iotcValue" = "value") %>% 
  rename("iotcYear" = "Year") %>% 
  select(CountryName, Species, iotcValue)
wcpfc_prop <- read.csv(here("02_processed-data","wcpfc_prop_long.csv")) %>% 
  rename("Species" = "Species.New") %>% 
  rename("CountryName" = "Country") %>% 
  rename("wcpfcValue" = "value") %>% 
  rename("wcpfcYear" = "Year") %>% 
  select(CountryName, Species, wcpfcValue)

#Importing Species-Specific CMS/CITES Data
cons_state <- read.csv(here("02_processed-data","cons_state.csv")) %>% 
  select(CountryName, Species, cmsValue, citesValue)

#Combining for Carcharhinus falciformis
species <- cons_state %>% 
  full_join(iattc_prop, by=c("CountryName", "Species")) %>% 
  full_join(iotc_prop, by=c("CountryName", "Species")) %>%
  full_join(wcpfc_prop, by=c("CountryName", "Species")) %>%
  mutate(CountryName = str_replace(CountryName,"USA", "United States")) %>% 
  mutate(CountryName = str_replace(CountryName,"EU", "European Union"))

cfalci <- species %>% 
  filter(Species=="Carcharhinus falciformis")
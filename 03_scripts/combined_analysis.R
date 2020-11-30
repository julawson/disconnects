###Loading Packages###
library(here)
library(tidyverse)
library(reshape2)

###Species-specific policy analyses###

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

#Importing Species-Specific CMS/CITES Data, creating one Conservation Value
cons_state <- read.csv(here("02_processed-data","cons_state.csv")) %>% 
  select(CountryName, Species, cmsValue, citesValue) %>%
  mutate(consValue = case_when(
    cmsValue == 1 & is.na(citesValue) ~ 1,
    cmsValue == 0 & is.na(citesValue) ~ 0,
    is.na(cmsValue) & citesValue == 1 ~ 1,
    is.na(cmsValue) & citesValue == 0 ~ 0,
    cmsValue == 1 & citesValue == 1 ~ 1,
    cmsValue == 0 & citesValue == 1 ~ 0.5)) %>% 
  select(CountryName, Species, consValue) 

#Combining all tRFMOs
trfmo_combo <- iattc_prop %>% 
  full_join(iotc_prop, by=c("CountryName", "Species")) %>%
  full_join(wcpfc_prop, by=c("CountryName", "Species")) %>%
  mutate(CountryName = str_replace(CountryName,"USA", "United States")) %>% 
  mutate(CountryName = str_replace(CountryName,"EU", "European Union")) %>%
  mutate(CountryName = str_replace(CountryName,"Australia ", "Australia")) %>% 
  mutate(trfmoValue= case_when(
    iattcValue == 1 & is.na(iotcValue) & is.na(wcpfcValue) ~ 1,
    iattcValue == 0 & is.na(iotcValue) & is.na(wcpfcValue) ~ 0,
    is.na(iattcValue) & iotcValue == 1 & is.na(wcpfcValue) ~ 1,
    is.na(iattcValue) & iotcValue == 0 & is.na(wcpfcValue) ~ 0,
    is.na(iattcValue) & is.na(iotcValue) & wcpfcValue == 1 ~ 1,
    is.na(iattcValue) & is.na(iotcValue) & wcpfcValue == 0 ~ 0,
    iattcValue == 0 & iotcValue ==0 & is.na(wcpfcValue) ~ 0,
    is.na(iattcValue) & iotcValue == 1 & wcpfcValue == 1 ~ 1,
    is.na(iattcValue) & iotcValue == 0 & wcpfcValue == 0 ~ 0)) %>% 
  select(CountryName, Species, trfmoValue) %>% 
  distinct() %>% 
  group_by(CountryName, Species) %>% 
  summarize(trfmoValue = mean(trfmoValue))

#filtering by species group for tRFMO
cfalci <- cons_state %>% 
  full_join(trfmo_combo, by=c("CountryName","Species")) %>% 
  filter(Species == "Carcharhinus falciformis") %>%
  mutate(overallValue= case_when(
    consValue == 1 & is.na(trfmoValue) ~ 1,
    consValue == 0.5 & is.na(trfmoValue) ~ 0.5,
    consValue == 0 & is.na(trfmoValue) ~ 0,
    is.na(consValue) & trfmoValue == 1 ~ 1,
    is.na(consValue) & trfmoValue == 0.5 ~ 0.5,
    is.na(consValue) & trfmoValue == 0 ~ 0,
    consValue == 1 & trfmoValue == 1 ~ 1,
    consValue == 0 & trfmoValue == 0 ~ 0)) %>% 
  group_by(CountryName) %>%
  summarize(overallValue = mean(overallValue)) %>% 
  distinct(CountryName)


rtypus <- trfmo_combo %>% 
  filter(Species == "Rhincodon typus") %>% 
  mutate(overallValue= case_when(
    iattcValue == 1 & is.na(iotcValue) & is.na(wcpfcValue) ~ 1,
    iattcValue == 0 & is.na(iotcValue) & is.na(wcpfcValue) ~ 0,
    is.na(iattcValue) & iotcValue == 1 & is.na(wcpfcValue) ~ 1,
    is.na(iattcValue) & iotcValue == 0 & is.na(wcpfcValue) ~ 0,
    is.na(iattcValue) & is.na(iotcValue) & wcpfcValue == 1 ~ 1,
    is.na(iattcValue) & is.na(iotcValue) & wcpfcValue == 0 ~ 0,
    iattcValue == 0 & iotcValue ==0 & is.na(wcpfcValue) ~ 0,
    is.na(iattcValue) & iotcValue == 1 & wcpfcValue == 1 ~ 1,
    is.na(iattcValue) & iotcValue == 0 & wcpfcValue == 0 ~ 0)) %>% 
  select(CountryName, Species, overallValue) %>% 
  distinct() %>% 
  group_by(CountryName, Species) %>% 
  summarize(trfmoValue = mean(overallValue))



#Importing Overall RFMO Data
iattc_all <- read.csv(here("02_processed-data","iattc_all.csv")) 
iotc_all <- read.csv(here("02_processed-data","iotc_all.csv")) 
wcpfc_all <- read.csv(here("02_processed-data","wcpfc_all.csv")) 

#Importing Overall CITES/CMS Data

###Leaving for later.
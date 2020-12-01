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

#Overall score for conservation and tRFMO mismatch
cons_trfmo <- cons_state %>% 
  left_join(trfmo_combo, by=c("CountryName","Species")) %>%
  mutate(overallValue= case_when(
    consValue == 1 & is.na(trfmoValue) ~ 1,
    consValue == 0.5 & is.na(trfmoValue) ~ 0.5,
    consValue == 0 & is.na(trfmoValue) ~ 0,
    is.na(consValue) & trfmoValue == 1 ~ 1,
    is.na(consValue) & trfmoValue == 0.5 ~ 0.5,
    is.na(consValue) & trfmoValue == 0 ~ 0,
    consValue == 1 & trfmoValue == 1 ~ 1,
    consValue == 0 & trfmoValue == 0 ~ 0,
    consValue == 1 & trfmoValue == 0 ~ 0.5,
    consValue == 0 & trfmoValue == 1 ~ 0.5))

#Analysis by species, all countries

#Thresher Sharks, Alopias spp.
alopias <- cons_trfmo %>% 
  filter(Species == "Alopias spp.")
#just countries in cons and trfmo
alopias_select <- alopias %>% 
  drop_na()

#Whale Shark, Rhincodon typus
rtypus <- cons_trfmo %>% 
  filter(Species == "Rhincodon typus")
#just countries in cons and trfmo
rtypus_select <- rtypus %>% 
  drop_na()

#Silky Shark, Carcharhinus falciformis
cfalci <- cons_trfmo %>% 
  filter(Species == "Carcharhinus falciformis")
#just countries in cons and trfmo
cfalci_select <- cfalci %>% 
  drop_na()

#Oceanic Whitetip, Carcharhinus longimanus
clongi <- cons_trfmo %>% 
  filter(Species == "Carcharhinus longimanus")
#just countries in cons and trfmo
clongi_select <- clongi %>% 
  drop_na()

#Hammerhead Sharks, Sphyrna zygaena or Sphyrna spp.
sphyrna <- cons_trfmo %>% 
  filter(Species == "Sphyrna spp." | Species == "Sphyrna zygaena") 
sphyrna_genus <- sphyrna %>% 
  distinct() %>% 
  group_by(CountryName) %>% 
  summarize(overallValue = mean(overallValue))
#just countries in cons and trfmo
sphyrna_select <- sphyrna %>% 
  drop_na()

#Mobulids and mantas, Manta birostris, Manta alfredi, Manta spp., Mobula spp.
mobulidae <- cons_trfmo %>% 
  filter(Species == "Manta birostris" | Species == "Manta alfredi" | Species == "Manta spp." | Species == "Mobula spp.") 
mobulidae_genus <- mobulidae %>% 
  distinct() %>% 
  group_by(CountryName) %>% 
  summarize(overallValue = mean(overallValue))
#just countries in cons and trfmo
mobulidae_select <- mobulidae %>% 
  mutate(Species = str_replace(Species,"Manta birostris", "Mobula spp.")) %>%
  mutate(Species = str_replace(Species,"Manta spp.", "Mobula spp.")) %>%
  mutate(Species = str_replace(Species,"Manta alfredi", "Mobula spp.")) %>%
  drop_na()

#Creating a table of consistent/inconsistent actors
all_species <- bind_rows(alopias_select, rtypus_select, cfalci_select, clongi_select, sphyrna_select, mobulidae_select)

###Exporting Data###
#Overall Analysis, Species level#
write.csv(x = all_species,
          file = here("04_results", "trfmo_cons_all_species.csv"),
          row.names = F)
write.csv(x = cons_state,
          file = here("02_processed-data", "cons_state_numeric.csv"),
          row.names = F)


###Overall###

#Importing Overall RFMO Data
iattc_all <- read.csv(here("02_processed-data","iattc_all.csv")) 
iotc_all <- read.csv(here("02_processed-data","iotc_all.csv")) 
wcpfc_all <- read.csv(here("02_processed-data","wcpfc_all.csv")) 

#Importing Overall CITES/CMS Data

###Leaving for later.
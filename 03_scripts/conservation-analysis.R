###Loading Packages###
library(here)
library(tidyverse)
library(reshape2)

cms_prop <- read.csv(here("02_processed-data","cms_prop_combo.csv")) %>% 
  rename(cmsValue = value)
cms_state <- read.csv(here("02_processed-data","cms_state_combo.csv")) %>% 
  rename(cmsValue = value)

cites_prop <- read.csv(here("02_processed-data","cites_prop_combo.csv")) %>% 
  rename(citesValue = value) %>% 
  rename(Year = Listing.Year)
cites_state <- read.csv(here("02_processed-data","cites_state_combo.csv")) %>% 
  rename(citesValue = value) %>% 
  rename(Year = Listing.Year)

###Joining CMS and CITES Prop Datasets###

#These are all countries that proposed all species
all_prop <- full_join(cms_prop,cites_prop, by=c("Species","CountryName")) %>% 
  select(Species,Year.x, Year.y, CountryName, cmsValue, citesValue)

#These are countries that proposed the same species in both CMS and CITES
same_prop <- all_prop %>% 
  drop_na()

#These are the countries that made statements on all species
all_state <- full_join(cms_state,cites_state, by=c("Species","CountryName")) %>% 
  select(Species,Year.x, Year.y, CountryName, cmsValue, citesValue) 

#These are countries that proposed the same species in both CMS and CITES
same_state <- all_state %>% 
  drop_na()

###Creating 'Conservation' Dataset of both Conventions###
cons_prop <- all_prop %>% 
  mutate(ConsValue = 1) %>% 
  rename(CMSyear = Year.x) %>% 
  rename(CITESyear = Year.y) %>% 
  select(Species, CMSyear, CITESyear, CountryName, ConsValue)

cons_state <- all_state %>% 
  mutate(
    combined = case_when(cmsValue == 0 & citesValue == 0 ~ "oppose",
                         cmsValue == 1 & citesValue == 1 ~ "support",
                         cmsValue == 0 & is.na(citesValue) ~ "oppose",
                         cmsValue == 1 & is.na(citesValue) ~ "support",
                         is.na(cmsValue) & citesValue == 0 ~ "oppose",
                         is.na(cmsValue) & citesValue == 1 ~ "support",
                         cmsValue == 1 & citesValue == 0 ~ "mixed",
                         cmsValue == 0 & citesValue == 1 ~ "mixed"))  %>% 
  
  rename(CMSyear = Year.x) %>% 
  rename(CITESyear = Year.y)

#Identify which countries were good actors, mixed actors, or bad actors across both CMS and CITES agreements.
cons_oppose <- cons_state %>% 
  filter(combined == "oppose" | combined == "mixed")
unique(cons_oppose$CountryName)


###Exporting Tidy Data###

#Overall Data, tRFMO level#
write.csv(x = cons_state,
          file = here("02_processed-data", "cons_state.csv"),
          row.names = F)
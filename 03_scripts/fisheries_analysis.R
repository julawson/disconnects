###Loading Packages###
library(here)
library(tidyverse)
library(reshape2)

#Importing Species Data
iattc_prop <- read.csv(here("02_processed-data","iattc_prop_long.csv")) 
iotc_prop <- read.csv(here("02_processed-data","iotc_prop_long.csv")) 
wcpfc_prop <- read.csv(here("02_processed-data","wcpfc_prop_long.csv")) 

#Importing All tRFMO Data
trfmo_all <- read.csv(here("02_processed-data","trfmo_all_long.csv"))

###IATTC Analysis###
#Creating new Datasets with Opposed and Support Countries
iattc_opposed <- trfmo_all %>% 
  filter(binaryValue == 0 & RFMOnew == "IATTC")
iattc_support <- trfmo_all %>% 
  filter(binaryValue == 1 & RFMOnew == "IATTC")
#Identifying general support, opposition, and mixef for IATTC countries
iattc_all <- full_join(iattc_opposed,iattc_support, by=c("Country","RFMOnew")) %>% 
  mutate(
    combined = case_when(binaryValue.x == 0 & is.na(binaryValue.y) ~ "oppose",
                         binaryValue.x == 0 & binaryValue.y == 1 ~ "mixed",
                         is.na(binaryValue.x) & binaryValue.y == 1 ~ "support")) %>% 
  select(Country, RFMOnew, combined)

###IOTC Analysis###
#Creating new Datasets with Opposed and Support Countries
iotc_opposed <- trfmo_all %>% 
  filter(binaryValue == 0 & RFMOnew == "IOTC")
iotc_support <- trfmo_all %>% 
  filter(binaryValue == 1 & RFMOnew == "IOTC")
#Identifying general support, opposition, and mixef for IATTC countries
iotc_all <- full_join(iotc_opposed,iotc_support, by=c("Country","RFMOnew")) %>% 
  mutate(
    combined = case_when(binaryValue.x == 0 & is.na(binaryValue.y) ~ "oppose",
                         binaryValue.x == 0 & binaryValue.y == 1 ~ "mixed",
                         is.na(binaryValue.x) & binaryValue.y == 1 ~ "support")) %>% 
  select(Country, RFMOnew, combined)

###WCPFC Analysis###
#Creating new Datasets with Opposed and Support Countries
wcpfc_opposed <- trfmo_all %>% 
  filter(binaryValue == 0 & RFMOnew == "WCPFC")
wcpfc_support <- trfmo_all %>% 
  filter(binaryValue == 1 & RFMOnew == "WCPFC")
#Identifying general support, opposition, and mixef for IATTC countries
wcpfc_all <- full_join(wcpfc_opposed,wcpfc_support, by=c("Country","RFMOnew")) %>% 
  mutate(
    combined = case_when(binaryValue.x == 0 & is.na(binaryValue.y) ~ "oppose",
                         binaryValue.x == 0 & binaryValue.y == 1 ~ "mixed",
                         is.na(binaryValue.x) & binaryValue.y == 1 ~ "support")) %>% 
  select(Country, RFMOnew, combined)


###Exporting Tidy Data###

#Overall Data, tRFMO level#
write.csv(x = iattc_all,
          file = here("02_processed-data", "iattc_all.csv"),
          row.names = F)
write.csv(x = iotc_all,
          file = here("02_processed-data", "iotc_all.csv"),
          row.names = F)
write.csv(x = wcpfc_all,
          file = here("02_processed-data", "wcpfc_all.csv"),
          row.names = F)

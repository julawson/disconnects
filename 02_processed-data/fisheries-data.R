###Loading Packages###
library(here)
library(tidyverse)
library(reshape2)

###IATTC Proponants###

#Loading IATTC CSV Files
iattc_prop <- read.csv(here("01_raw-data", "IATTC_Proponents.csv"),
                     stringsAsFactors = F) %>% 
  rename("Manta birostris" = "Manta.birostris") %>%
  rename("Alopias spp." = "Alopas.spp.") %>% 
  rename("Carcharhinus falciformis 2012" = "Carcharhinus.falciformis_2012") %>% 
  rename("Carcharhinus falciformis 2014" = "Carcharhinus.falciformis_2014") %>%
  rename("Carcharhinus falciformis 2015" = "Carcharhinus.falciformis_2015") %>%
  rename("Carcharhinus falciformis 2019" = "Carcharhinus.falciformis_2019") %>%  
  rename("Manta alfredi" = "Manta.alfredi") %>%
  rename("Mobula spp. 2015" = "Mobula.spp_2015") %>% 
  rename("Sphyrna spp." = "Sphyrna.spp.") %>%  
  rename("Prionace glauca" = "Prionace.glauca") %>% 
  rename("Rhincodon typus 2017" = "Rhincodon.typus.2017") %>% 
  rename("Sphyrna zygaena 2011" = "Sphyrna.zygaena_2011") %>% 
  rename("Sphyrna zygaena 2012" = "Sphyrna.zygaena_2012") %>% 
  rename("Sphyrna zygaena 2015" = "Sphyrna.zygaena_2015") %>% 
  rename("Carcharhinus longimanus 2011" = "Carcharhinus.longimanus_2011")

iattc_prop_long <- melt(iattc_prop, id = c("Country", "membership_status"), variable.name = "Species") %>% 
  drop_na() %>% 
  separate(Species, c("Genus", "Species","Year")) %>% 
  unite(Species.New, Genus, Species, sep = " ") %>%
  mutate(Species.New = str_replace(Species.New,"spp", "spp."))

#Loading WCPFC CSV Files
wcpfc_prop <- read.csv(here("01_raw-data", "WCPFC_Proponents.csv"),
                       stringsAsFactors = F) %>% 
  rename("Manta birostris" = "Manta.birostris") %>%
  rename("Alopias spp." = "Alopas.spp.") %>% 
  rename("Carcharhinus falciformis 2014" = "Carcharhinus.falciformis_2014") %>%
  rename("Manta alfredi" = "Manta.alfredi") %>%
  rename("Mobula spp. 2017" = "Mobula.spp_2017") %>% 
  rename("Mobula spp. 2020" = "Mobula.spp_2020") %>%
  rename("Sphyrna spp." = "Sphyrna.spp.") %>%
  rename("Prionace glauca" = "Prionace.glauca") %>% 
  rename("Rhincodon typus 2010" = "Rhincodon.typus_2010") %>% 
  rename("Rhincodon typus 2012" = "Rhincodon.typus_2012") %>% 
  rename("Rhincodon typus 2013" = "Rhincodon.typus_2013") %>% 
  rename("Sphyrna zygaena" = "Sphyrna.zygaena") %>% 
  rename("Carcharhinus longimanus 2012" = "Carcharhinus.longimanus_2012")

wcpfc_prop_long <- melt(wcpfc_prop, id = c("Country", "membership_status"), variable.name = "Species") %>% 
  drop_na() %>% 
  separate(Species, c("Genus", "Species","Year")) %>% 
  unite(Species.New, Genus, Species, sep = " ") %>%
  mutate(Species.New = str_replace(Species.New,"spp", "spp.")) %>% 
  filter(Country != "FFA")

#Loading IOTC CSV Files
iotc_prop <- read.csv(here("01_raw-data", "IOTC_proponents.csv"),
                       stringsAsFactors = F) %>%
  rename("Manta birostris" = "Manta.birostris") %>%
  rename("Alopias spp." = "Alopas.spp.") %>% 
  rename("Alopias spp. 2010" = "Alopas.spp_2010") %>% 
  rename("Carcharhinus falciformis 2014" = "Carcharhinus.falciformis_2014") %>%
  rename("Carcharhinus falciformis 2013" = "Carcharhinus.falciformis_2013") %>%
  rename("Manta alfredi" = "Manta.alfredi") %>%
  rename("Mobula spp. 2017" = "Mobula.spp_2017") %>%
  rename("Sphyrna spp. 2014" = "Sphyrna.spp_2014") %>%
  rename("Sphyrna spp. 2013" = "Sphyrna.spp_2013") %>%
  rename("Sphyrna spp. 2012" = "Sphyrna.spp_2012") %>%
  rename("Sphyrna spp. 2011" = "Sphyrna.spp_2011") %>%
  rename("Prionace glauca 2018" = "Prionace.glauca_2018") %>% 
  rename("Rhincodon typus 2012" = "Rhincodon.typus_2012") %>%
  rename("Rhincodon typus 2013" = "Rhincodon.typus_2013") %>% 
  rename("Carcharhinus longimanus 2017" = "Carcharhinus.longimanus_2017") %>% 
  rename("Carcharhinus longimanus 2013" = "Carcharhinus.longimanus_2013") %>% 
  rename("Carcharhinus longimanus 2011" = "Carcharhinus.longimanus_2011")

iotc_prop_long <- melt(iotc_prop, id = c("Country", "Status"), variable.name = "Species") %>% 
  drop_na() %>% 
  separate(Species, c("Genus", "Species","Year")) %>% 
  unite(Species.New, Genus, Species, sep = " ") %>%
  mutate(Species.New = str_replace(Species.New,"spp", "spp.")) 
  

###tRFMO Voting All Measures###
trfmo_all <- read.csv(here("01_raw-data", "tRFMO_voting_all.csv"),
                      stringsAsFactors = F)

trfmo_all_long <- melt(trfmo_all, id = c("Country"), variable.name = "RFMOposition") %>% 
  drop_na() %>% 
  mutate(
    binaryValue = case_when(RFMOposition == "iattc_proposed" ~ 1,
                         RFMOposition == "iattc_opposed" ~ 0,
                         RFMOposition == "wcpfc_proposed" ~ 1,
                         RFMOposition == "wcpfc_supported" ~ 1,
                         RFMOposition == "wcpfc_opposed" ~ 0,
                         RFMOposition == "iotc_proposed" ~ 1,
                         RFMOposition == "iotc_supported" ~ 1,
                         RFMOposition == "iotc_opposed" ~ 0)) %>%
  mutate(
    RFMOnew = case_when(RFMOposition == "iattc_proposed" ~ "IATTC",
                     RFMOposition == "iattc_opposed" ~ "IATTC",
                     RFMOposition == "wcpfc_proposed" ~ "WCPFC",
                     RFMOposition == "wcpfc_supported" ~ "WCPFC",
                     RFMOposition == "wcpfc_opposed" ~ "WCPFC",
                     RFMOposition == "iotc_proposed" ~ "IOTC",
                     RFMOposition == "iotc_supported" ~ "IOTC",
                     RFMOposition == "iotc_opposed" ~ "IOTC")) %>% 
  select(Country, binaryValue, RFMOnew)

###Exporting Tidy Data###

#Species Data#
write.csv(x = iattc_prop_long,
          file = here("02_processed-data", "iattc_prop_long.csv"),
          row.names = F)
write.csv(x = iotc_prop_long,
          file = here("02_processed-data", "iotc_prop_long.csv"),
          row.names = F)
write.csv(x = wcpfc_prop_long,
          file = here("02_processed-data", "wcpfc_prop_long.csv"),
          row.names = F)

#Overall Data#
write.csv(x = trfmo_all_long,
          file = here("02_processed-data", "trfmo_all_long.csv"),
          row.names = F)
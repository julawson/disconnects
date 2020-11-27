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
  case_when()

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
  unite(Species.New, Genus, Species, sep = " ")
<<<<<<< HEAD
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
=======

library(dplyr)
library(tidyverse)
library(reshape2)
library(gridExtra)

#create plot of country participantion for each tRFMO 
#note that this is high level for all shark-related policy (not single species)


all_trfmo_votes<- read.csv("tRFMO_voting_all.csv", sep = ",", header = TRUE)

#pivot database from wide to long
i<-all_trfmo_votes %>% 
  pivot_longer(!Country, names_to = "position", values_to = "votes")


#add column for commission
i<-i %>%
  mutate(tRFMO= case_when(
    startsWith(position, "iattc") ~ "IATTC" ,
    startsWith(position, "wcpfc") ~ "WCPFC" ,
    startsWith(position, "iotc") ~ "IOTC" ,
    startsWith(position, "iccat") ~ "ICCAT" 
     ))

#add column for position
i<-i %>%
  mutate(stance= case_when(
    endsWith(position, "supported") ~ "Support" ,
    endsWith(position, "proposed") ~ "Support" ,
    endsWith(position, "opposed") ~ "Oppose" 
  ))

#remove NAs
i<-na.omit(i)

#plot in facet 
p1<-ggplot(i, aes( x=reorder(Country, -votes, FUN = sum), y = votes, fill=stance  ))+
  geom_bar( stat= "identity", position= "stack")+
  labs(title="Country participation in fisheries policy",
     y= "Number of votes", x= "Country"  )+
  guides(fill=guide_legend(title=NULL))+
  facet_wrap(~reorder( tRFMO, -votes, FUN= sum), scales= "free_y",  drop=TRUE)+
  coord_flip()+
  theme_bw()

p1 + facet_wrap(vars(tRFMO ), nrow=1, scales= "free",drop=TRUE)



>>>>>>> f00aba1df6cdd7efffb55ebbbabfc9d9ddd9772e


library(dplyr)
library(tidyverse)
library(reshape2)
library(gridExtra)
library(viridis)
library(ggpubr)
library(here)
library(scales)

setwd("~/Desktop/Fisheries_Disconnects_Paper/disconnects/01_raw-data")

#global shark landings reported to FAO 
fao_catch<-read.csv("Global_shark_FAO_catch.csv", sep= ",", header= TRUE)
#remove early years
fao_catch<-fao_catch[,-3:-93 ]

#remove x's 
catch<-  fao_catch[, grepl( "^X",names(fao_catch))]
#merge datasets
catch<-cbind(fao_catch[,1:2], catch)

#fix colnames
names<-c("Country" , "species", 1995:2018)
colnames(catch)<-names
catch<-catch[!grepl("Total", catch$Country),]
catch<-catch[!grepl("FAO", catch$Country),]

#combine spp groups to match analyses

catch$species[catch$species == "Hammerhead sharks, etc. nei"] <- "Sphyrna spp."
catch$species[catch$species == "Silky shark"] <- "Carcharhinus falciformis"
catch$species[catch$species == "Oceanic whitetip shark"] <- "Carcharhinus longimanus"
catch$species[catch$species == "Mako sharks"] <- "Isurus spp."
catch$species[catch$species == "Mantas, devil rays nei"] <- "Mobula spp./Manta spp."
catch$species[catch$species == "Mobula nei"] <- "Mobula spp./Manta spp."
catch$species[catch$species == "Giant manta"] <- "Mobula spp./Manta spp."
catch$species[catch$species == "Spinetail mobula"] <- "Mobula spp./Manta spp."
catch$species[catch$species == "Thresher sharks nei"] <- "Alopias spp."
catch$species[catch$species == "Blue shark"] <- "Prionace glauca"

#now we have formatted dataset. 
#need to pivot to long, group by species and coutnry

#summarise by total catch of all species, 1995-2018

#next go back to data with species and time
catch_totals<- catch %>% pivot_longer(
  cols = 3:26,
  names_to = "Year" ,values_to = "catch") 


totals<-catch_totals %>%
  group_by(species, Year) %>%
  summarise(total=sum(catch))
#total catch by spp over time
ggplot(totals, aes(x=Year, y=total, group=as.character(species)))+
  geom_line(aes(color=species))+
  geom_point(aes(color=species))+
  scale_y_continuous(labels = scales::comma)+
  #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
              #  labels = trans_format("log10", math_format(10^.x))) +
  theme_bw()+
  labs(y= "Total Reported Catch (1995 - 2018)")

#area plot
ggplot(totals, aes(x=Year, y=total, group=as.character(species)))+
  geom_area(aes(color=species, fill=species))+
  scale_y_continuous(labels = scales::comma)+
  #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #  labels = trans_format("log10", math_format(10^.x))) +
  theme_bw()+
  labs(y= "Total Reported Catch (1995 - 2018)")



#isolate highest-capture countries
top_countries <- catch_totals  %>% 
  group_by(Country) %>% 
  summarise(total=sum(catch))

top_countries<-top_countries %>% 
  # desc orders from largest to smallest
  arrange(desc(total)) 

#remove other category
top_countries<-top_countries[!grepl("Other", top_countries$Country),]
top_countries<-top_countries[1:10,]

#simple country plot (no fill) - top 10 countries
ggplot(top_countries, aes(x=reorder(Country, total), y=total ))+
  geom_bar(stat="identity", position="stack")+
  theme_bw()+
  coord_flip()+
  labs(y= "Total Reported Catch (1995 - 2018)", x = "Country" )





library(dplyr)
library(tidyverse)
library(reshape2)
library(gridExtra)
library(viridis)
library(ggpubr)
library(here)

#create plot of country participantion for each tRFMO 
#note that this is high level for all shark-related policy (not single species)


all_trfmo_votes<- read.csv(here("01_raw-data", "tRFMO_voting_all.csv"))

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

#opposers

opposers<-subset(i, stance == "Oppose")

o<-ggplot(opposers, aes( x=reorder(Country, -votes, FUN = sum), y = votes, fill= tRFMO ))+
  geom_bar( stat= "identity", position= "stack")+
  scale_fill_viridis(discrete=TRUE)+
  labs(title="Country opposition to fisheries policy",
       y= "Number of votes", x= "Country"  )+
 # facet_wrap(~reorder( tRFMO, -votes, FUN= sum), scales= "free_y",  drop=TRUE)+
  coord_flip()+
  theme_bw()

#proposers 

supporters<-subset(i, stance == "Support")

s<- ggplot(supporters, aes( x=reorder(Country, -votes, FUN = sum), y = votes, fill= tRFMO ))+
  geom_bar( stat= "identity", position= "stack")+
  scale_fill_viridis(discrete=TRUE)+
  labs(title="Country supporters of fisheries policy",
       y= "Number of votes", x= "Country"  )+
  # facet_wrap(~reorder( tRFMO, -votes, FUN= sum), scales= "free_y",  drop=TRUE)+
  coord_flip()+
  theme_bw()

ggarrange(o,s , ncol=2, common.legend=TRUE)

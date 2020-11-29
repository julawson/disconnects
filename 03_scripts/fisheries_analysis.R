
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




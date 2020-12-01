###Loading Packages###
library(here)
library(tidyverse)
library(reshape2)
library(gridExtra)

#Country participation in conservation policy (by number of votes)
#One bar indicating proposal or co-sponsor, another stacked bar indicating support

cons_prop <- read.csv(here("02_processed-data","cons_prop.csv"))
cons_state <- read.csv(here("02_processed-data","cons_state_numeric.csv")) 
all_trfmo_votes <- read.csv(here("01_raw-data","tRFMO_voting_all.csv"),
                      stringsAsFactors = F) %>% 
  mutate(Country = str_replace(Country,"USA", "United States")) %>%
  mutate(Country = str_replace(Country,"Korea", "Republic of Korea")) %>% 
  mutate(Country = str_replace(Country,"Nigaragua", "Nicaragua")) %>% 
  filter(Country != "PNA", Country != "FFA")
  
#Summarize number for conservation agreements
cons_prop_summary <- cons_prop %>% 
  group_by(CountryName) %>% 
  summarize(total = sum(ConsValue)) %>% 
  add_column(position = "propose")
cons_state_support <- cons_state %>% 
  group_by(CountryName) %>% 
  summarize(total = sum(consValue)) %>% 
  add_column(position = "support")
cons_state_oppose <- cons_state %>% 
  group_by(CountryName) %>% 
  filter(consValue == 0) %>% 
  group_by(CountryName) %>% 
  count() %>% 
  add_column(position = "oppose") %>% 
  rename("total" = "n")
cons_summary <- bind_rows(cons_state_support,cons_state_oppose, cons_prop_summary) %>% 
  rename("Country" = 'CountryName')

#pivot database from wide to long
trfmo_summary<-all_trfmo_votes %>% 
  pivot_longer(!Country, names_to = "position", values_to = "votes") %>% 
  mutate(tRFMO= case_when(
    startsWith(position, "iattc") ~ "IATTC" ,
    startsWith(position, "wcpfc") ~ "WCPFC" ,
    startsWith(position, "iotc") ~ "IOTC" ,
    startsWith(position, "iccat") ~ "ICCAT")) %>% 
  mutate(position= case_when(
    endsWith(position, "supported") ~ "support" ,
    endsWith(position, "proposed") ~ "propose" ,
    endsWith(position, "opposed") ~ "oppose")) %>% 
  drop_na() %>% 
  rename("total"="votes")

#Identifying cons countries that also have tRFMOs
trfmo_countries <- trfmo_summary %>% 
  distinct(Country) %>% 
  as.list()
cons_summary_filtered <- trfmo_summary %>% 
  left_join(cons_summary, by="Country")


#plot in facet 

fish <- ggplot(mapping=aes(x=reorder(Country, -total.x), y = total.x, fill = position.x)) +
  geom_bar(data=cons_summary_filtered, stat="identity", position="stack") +
  labs(y= "Number of votes", x= "Country") +
  guides(fill=guide_legend(title=NULL)) +
  facet_wrap(~reorder(tRFMO, -total.x, levels=c("IATTC","IOTC","WCPFC")), scales= "free_y",  drop=TRUE) +
  scale_y_reverse() +
  coord_flip()+
  theme_bw()

cons <- ggplot(mapping=aes(x=reorder(Country, -total.y), y = total.y, fill = position.y)) +
  geom_bar(data=cons_summary_filtered, stat="identity", position="stack") +
  labs(y= "Number of votes", x= "Country") +
  guides(fill=guide_legend(title=NULL)) +
  facet_wrap(~reorder(tRFMO, -total.y, levels=c("IATTC","IOTC","WCPFC")), scales= "free_y",  drop=TRUE) +
  coord_flip()+
  theme_bw()

###Export two plots###
ggsave(plot = fish,
       filename = here("04_results", "trfmo_votes.png"),
       height = 5,
       width = 10)
ggsave(plot = cons,
       filename = here("04_results", "cons_votes.png"),
       height = 5,
       width = 10)

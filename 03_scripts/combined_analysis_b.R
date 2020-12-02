###Loading Packages###
library(here)
library(tidyverse)
library(reshape2)
library(gridExtra)
library(ggpubr)
library(wesanderson)

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

#plot by tRFMO

iattc_toplot <- cons_summary_filtered %>% 
  filter(tRFMO=="IATTC") %>% 
  mutate(total.y = -1*total.y)
break_values <- c(20,0,-20)
iattc_plot <- ggplot(iattc_toplot) +
  geom_bar(aes(x=reorder(Country, -total.x),y = total.y, fill = position.y), stat="identity", position="stack") +
  geom_bar(aes(x=reorder(Country, -total.x),y = total.x, fill = position.x), stat="identity", position="stack") +
  scale_fill_manual(values=wes_palette(n=3, name="Darjeeling1")) +
  geom_hline(yintercept = 0) +
  labs(title=NULL, y= NULL, x=NULL) +
  scale_y_reverse() +
  coord_flip() +
  guides(fill=guide_legend(title=NULL)) +
  scale_y_continuous(breaks = break_values,
                     labels = abs(break_values)) +
  annotate("text", x = "Republic of Korea", y = 25, label = "Fisheries", size = 3.5, fontface = "bold.italic") +
  annotate("text", x = "Republic of Korea", y = -25, label = "Conservation", size = 3.5, fontface = "bold.italic") +
  theme_bw() +
  theme(legend.position = "none")

iotc_toplot <- cons_summary_filtered %>% 
  filter(tRFMO=="IOTC") %>% 
  mutate(total.y = -1*total.y)
break_values2 <- c(30,20,10,0,-10)
iotc_plot <- ggplot(iotc_toplot) +
  geom_bar(aes(x=reorder(Country, -total.x),y = total.y, fill = position.y), stat="identity", position="stack") +
  geom_bar(aes(x=reorder(Country, -total.x),y = total.x, fill = position.x), stat="identity", position="stack") +
  scale_fill_manual(values=wes_palette(n=3, name="Darjeeling1")) +
  geom_hline(yintercept = 0) +
  labs(title=NULL, y= NULL, x=NULL) +
  scale_y_reverse() +
  coord_flip() +
  guides(fill=guide_legend(title=NULL)) +
  scale_y_continuous(breaks = break_values2, 
                     labels = abs(break_values2)) +
#  ylim(-20,30) +
  annotate("text", x = "South Africa", y = 30, label = "Fisheries", size = 3.5, fontface = "bold.italic") +
  annotate("text", x = "South Africa", y = -12, label = "Conservation", size = 3.5, fontface = "bold.italic") +
  theme_bw() +
  theme(legend.position = "none")

wcpfc_toplot <- cons_summary_filtered %>% 
  filter(tRFMO=="WCPFC") %>% 
  mutate(total.y = -1*total.y)
break_values3 <- c(20,0,-20)
wcpfc_plot <- ggplot(wcpfc_toplot) +
  geom_bar(aes(x=reorder(Country, -total.x),y = total.y, fill = position.y), stat="identity", position="stack") +
  geom_bar(aes(x=reorder(Country, -total.x),y = total.x, fill = position.x), stat="identity", position="stack") +
  scale_fill_manual(values=wes_palette(n=3, name="Darjeeling1")) +
  geom_hline(yintercept = 0) +
  labs(title=NULL, y= NULL, x=NULL) +
  scale_y_reverse() +
  coord_flip() +
  scale_y_continuous(breaks = break_values3, 
                     labels = abs(break_values3)) +
  annotate("text", x = "Tuvalu", y = 25, label = "Fisheries", size = 3.5, fontface = "bold.italic") +
  annotate("text", x = "Tuvalu", y = -25, label = "Conservation", size = 3.5, fontface = "bold.italic") +
  theme_bw() +
  theme(legend.position = "none")


###Combine plots###

combotrfmo <- ggarrange(iattc_plot,iotc_plot,wcpfc_plot, nrow = 1, labels = c("IATTC","IOTC","WCPFC"), font.label = list(size=12), common.legend = TRUE, legend="top")

combotrfmo2 <- annotate_figure(combotrfmo, bottom = "Number of Participatory Actions", left = "Party")

facet_wrap( fish, cons)


###Export two plots###
ggsave(plot = combotrfmo2,
       filename = here("04_results", "combined_trfmo_cons.png"),
       height = 5,
<<<<<<< HEAD
       width = 10)




#divergent bar plot
#add column for type of forum
fishery<-rep("fishery", length(trfmo_summary))
trfmo_all <- cbind(trfmo_summary, forum=fishery) 

cons<-rep("conservation", length(cons_summary))
nas<-rep("NA", length(cons_summary))
cons_all <- cbind(cons_summary, forum=cons, tRFMO= nas) 

#add negative values for trfmo votes
trfmo_all<-mutate(trfmo_all, total = -total)

#merge datasets
cons_summary_mirror<- rbind(trfmo_all, cons_all, 
                      by = c("Country", "position", "total", "tRFMO", "forum"  ) ) 
cons_summary_mirror$total<-as.numeric(cons_summary_mirror$total)

#plot
ggplot(cons_summary_mirror,
       aes(x=reorder(Country, -total), y =total, fill= position))+
  geom_bar(stat="identity")+
  coord_flip()+
  theme_bw()
  #facet_wrap(~reorder(tRFMO, -total, levels=c("IATTC","IOTC","WCPFC")), scales= "free_y",  drop=TRUE) +



=======
       width = 15)
>>>>>>> cce2ae781038295b8fc60634cde58609896b2794

#download data at https://data.migrationpatterns.org/MigrationPatternsData.zip
#update URL in ya_migration_data with your desired file path
ya_migration_data<- read.csv("C://Users/oduon/Desktop/MigrationPatternsData/od_race.csv", stringsAsFactors = FALSE)

#load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

#cleaning dataframe
#pipe dataframe to function
o_and_d<-ya_migration_data %>% 
  #combine origin city and origin state columns
  unite("origin_cs", o_cz_name:o_state_name, sep = ", ", remove = FALSE) %>% 
  #combine destination city and destination state columns
  unite("destination_cs", d_cz_name:d_state_name, sep =", ", remove= FALSE) %>% 
  #display origin city state, destination city state, and number of persons columns
  select(origin_cs, destination_cs, n) %>% 
  #filter to only display results where the origin city state does not match the destination city state
  filter(origin_cs != destination_cs) %>% 
  #filter to exclude results with number of persons less than 1
  filter(n>0) %>% 
  #sort by the number of persons in descending order
  arrange(desc(n)) %>% 
  #pull top 10 or bottom results-important! keep slice code you're not using to comments slice_tail(n = 15)
  slice_head(n = 10)

#o_and_d data viz
#attribute dataframe to ggplot
ggplot(data = o_and_d) +
  #assign visualization as scatter plot
  geom_point(mapping = 
               aes(x = destination_cs, 
                   y = n, 
                   color = origin_cs))+
                   #assign labels, make x axis labels vertical
                   labs(title = "top spots to relocate",
                   x = "destination city, state",
                   y = "# of people", 
                   color = "origin city, state")+
                   theme(axis.text.x = 
                           element_text(angle = 90, 
                                        vjust = 0.5,
                                        hjust=1))

#Michigan dataframe
#pipe dataframe to function
mi_asian<- ya_migration_data %>%
  #filter to only show Asians from Michigan
  filter(pool =="Asian", o_state_name == "Michigan") %>% 
  #group by the destination state name
  group_by(d_state_name) %>%
  #add the total number of movers among each state
  summarize(distinct_movers = n_distinct(n))

#generate csv file to upload into Datawrapper
#update the filepath with your desired filepath
write.csv(mi_asian, "C://Users/oduon/Desktop/MigrationPatternsData/od_mi_asian.csv", row.names=FALSE)

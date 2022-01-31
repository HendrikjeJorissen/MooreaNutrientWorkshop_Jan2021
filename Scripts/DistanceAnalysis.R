#Script for mapping closest distance of sampling point to river mouths for Moorea Nutrient Workshop Jan 2022

library(tidyverse)
library(here)

#data file is located here
StreamMouths <- st_read("C:/Users/hendr/Documents/GitHub/MooreaNutrientWorkshop_Jan2021/Data/Lagoon_sites_may21 with streams.kmz.kml")

#use online converter from kmz to csv
#read in data
StreamMouths <- read_csv(file = here("Data","stream_sites.csv"))


StreamMouths <- StreamMouths %>% 
  select(-description) %>% 
  slice(-c(199:204)) #remove unused points


StreamMouths$ZZZ=1 #create dummy variable for joining
SM <- StreamMouths %>% full_join(StreamMouths,c("ZZZ"="ZZZ")) %>% #creates a data frame of all combinations of rows from d
  filter(name.x != name.y) %>%   #filters out the rows with the same ID
  rename(nameY = name.y, nameX = name.x) %>% 
  filter(!grepl('Lag', nameY)) %>% #filter out double entries
  filter(!grepl('Mouth', nameX)) %>% #filter out double entries
  select(-ZZZ)

#calculate distance between 2 vectors
library(geosphere) 
SM$distance<-distHaversine(SM[,1:2], SM[,4:5]) 

# select closest for each sample point the closest distance to stream mouth 
SM_distance <-group_by(SM, nameX) %>% 
  slice(which.min(distance))

#export results
write_csv(SM_distance,here("Output","Distance to river mouth data.csv") )

library(tidyverse)
library(rgbif)
library(usmap)
library(rnaturalearth)
library(sf)
library(data.table)

#Fish Data 
dat <- read.csv("WDFW-SGS.f.csv")

#Skyknomish River Coordinates from Google Maps 
sky_river_cords <- read.csv("sky_river_cords.csv")
sky_cords <- select(sky_river_cords,Lat, Long)

#Get Bird Data
y = "2010,2020"
dat.l <-list()
species<- c("Empidonax difficilis")
for(s in species){
  n.obs <-  occ_data(scientificName = bird_species,
                     year=y,
                     limit=0,
                     basisOfRecord = "HUMAN_OBSERVATION",
                     stateProvince="Washington")$meta$count
  
  print(n.obs)
  
  
  dat.l[[paste0(s)]] <- occ_data(scientificName = bird_species,
                                 year=y,
                                 limit=n.obs,
                                 country="US",
                                 basisOfRecord = "HUMAN_OBSERVATION",
                                 stateProvince="Washington")[[2]]
  
  
}
bird <- rbindlist(dat.l,fill=T)

#Get rid of random columns 
bird.f <- select(bird,key,scientificName, decimalLongitude,decimalLatitude,individualCount,year)

#Get salmon data for Skyknomish River 
sky.salmon <- dat %>%
  filter(SpeciesAbbr %in% c("CHIN","SOCK","COHO","PINK","CHUM","ATLA"),
         !is.na(UpperLatitude), 
         RunYear > 2010, StreamName %in% c("Sky Slough (LB)","Sky Slough A", 
                                           "Sky Slough B","Sky Slough C", "Sky Slough D")) %>%
  group_by(RunYear)%>%
  summarise(count=n())%>%
  rename(year = RunYear)

# Find birds only near Skyknomish River 
bird_sf <- st_as_sf(bird.f, coords = c("decimalLongitude", "decimalLatitude"),crs=4326)
sky_cords_sf <- st_as_sf(sky_cords, coords = c("Long", "Lat"),crs=4326)
bird.f$distance <- apply(st_distance(bird_sf,sky_cords_sf),1,min) 
#this thing turns lat and long into distance in meters and puts the minimun distance to the river as a new data point 

sky.bird <- bird.f %>%
  filter(distance < 2000) %>% #select for 2km away from river, we may want to change that to more or less 
  group_by(year,scientificName)%>%
  summarise(count=n()) %>%
  rename(species = scientificName)

#Join Salmon data to bird data
Final_data <- left_join(sky.bird,sky.salmon, by="year")%>%
  rename(Bird.count = count.x,Salmon.count = count.y)

#Plot 
ggplot(all, aes(x = year)) +
  geom_point(aes(y = count.x, color = "Bird.count"), size = 3) +
  geom_point(aes(y = count.y, color = "Salmon.count"), size = 3) 
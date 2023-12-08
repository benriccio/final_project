library(tidyverse)
library(rgbif)
library(usmap)
library(rnaturalearth)
library(sf)
library(data.table)

#Fish Data 
dat <- read.csv("WDFW-SGS.csv")

#Skyknomish River Coordinates from Google Maps 
sky_river_cords <- read.csv("sky_river_cords.csv")
sky_cords <- select(sky_river_cords,Lat, Long)



#read in bird data
dat <- readRDS("wash_bird_data.RDS")

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


#assessing data: correlation between bird arrival/presence in specific areas 
#problems: server error, max offset error, trying to push fish data set  - try to push as url instead, or start summarise and save that as an rds(way to big)(ben trying to filter out unneeded data)
#create models, - example: lm(bird ~ salmon), t test to see significance, also create plots describing this data
#... time series analysis (ccf)?, have to find a way to assess confounding variables
# month vs day?

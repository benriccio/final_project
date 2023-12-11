library(usmap)
library(lubridate)
library(sf)
library(tidyverse)
library(gridExtra)

#Questions:
#River Coords 
#Bird Data 
#Correlation 

river.coords <- read.csv('sky_river_cords.csv')
sky.river <- select(river.coords,Lat,Long)
bird <- readRDS('wash_bird_data.RDS')
fish <- read.csv('WDFW-SGS.csv')
WA <- map_data('state','washington')


#Get fish data and filter salmon in Skyko river only 
fish <- fish %>% 
  filter(StreamName %in% c("Sky Slough C","Sky Slough B","Sky Slough A","Snohomish R. Cont. As Skykomish R. @ R.M. 20.51")) %>%
  filter(SpeciesAbbr %in% c('CHIN','PINK','COHO','CHUM','SOCK'))

#Change Dates to readable dates and add year and month columns 
fish$SurveyDate <- as.Date(fish$SurveyDate,format="%m/%d/%Y")
fish$month <- month(fish$SurveyDate)
fish$year <- year(fish$SurveyDate)

fish.f <- fish %>% 
  filter(year >2000) %>%
  group_by(year,month) %>%
  summarise(salmon.count = sum(LiveTotal,na.rm=TRUE))


salmon_count_by_month <- ggplot(fish.f,aes(x=month)) +geom_point(aes(y=salmon.count)) + facet_wrap(year~.) +ylim(0,1000)
salmon_count_by_month

#get birds within x km of river 
bird <- bird %>%
  filter(year >2000)

bird_sf <- st_as_sf(bird, coords = c("decimalLongitude", "decimalLatitude"),crs=4326)
sky_cords_sf <- st_as_sf(sky.river, coords = c("Long", "Lat"),crs=4326)
bird$distance <- apply(st_distance(bird_sf,sky_cords_sf),1,min)

skyko.birds.s <- bird %>%
  filter(distance < 3000) %>%
  group_by(year,month,scientificName) %>%
  summarise(bird.count = n())

skyko.birds <- bird %>%
  filter(distance < 3000) %>%
  group_by(year,month) %>%
  summarise(bird.count = n())

bird_count_by_month <- ggplot(skyko.birds.s,aes(x=month))+geom_point(aes(y=bird.count,col=scientificName))+facet_wrap(year~.)
bird_count_by_month

#find bird levels vs salmon levels by month by year 

skyko.birds.f <- skyko.birds %>%
  filter(year>2016)
final.dat <- left_join(skyko.birds.f,fish.f,by=c('year','month'))

final_graph <- ggplot(final.dat,aes(x=month),)+geom_point(aes(y=bird.count),col='blue')+
  geom_point(aes(y=salmon.count),col='red')+facet_wrap(year~.)+ylim(0,50)

final_graph

final.dat.m <- final.dat %>%
  filter(month %in% c('7','8','9','10','11','12'))

final_graph_month_limit <- ggplot(final.dat.m,aes(x=month),)+geom_point(aes(y=bird.count),col='blue')+
  geom_point(aes(y=salmon.count),col='red')+facet_wrap(year~.) +ylim(0,100)

sum(skyko.birds.f$bird.count)

#final graphs 
bird_count_by_month
salmon_count_by_month
final_graph
final_graph_month_limit



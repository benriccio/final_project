library(tidyverse)
library(lubridate)
library(sf)
library(rgbif)
library(lme4)


river.coords <- read.csv("sky_river_cords.csv")
river.coords <- select(river.coords,Lat,Long)
bird <- readRDS("wash_bird_data.RDS")
fish <- read.csv("WDFW-SGS.csv")

#Filter Fish to salmon in select river and add year column 
fish <- fish %>% 
  filter(StreamName %in% c("Sky Slough C","Sky Slough B","Sky Slough A","Snohomish R. Cont. As Skykomish R. @ R.M. 20.51")) %>%
  filter(SpeciesAbbr %in% c('CHIN','PINK','COHO','CHUM','SOCK')) 


fish$SurveyDate <- as.Date(fish$SurveyDate,format="%m/%d/%Y")
fish$year <- year(fish$SurveyDate)
fish.f <- fish %>%
  filter(year > 2008,year <2020) %>%
  group_by(year) %>%
  summarise(salmon.count = sum(LiveTotal,na.rm=TRUE))
fish.f$year <- as.integer(fish.f$year)
fish.f$year <- fish.f$year+1

#Filter birds to relevant years then find birds only within 3km of river 
bird <- bird %>%
  filter(year > 2009)
bird_sf <- st_as_sf(bird, coords = c("decimalLongitude", "decimalLatitude"),crs=4326)
sky_cords_sf <- st_as_sf(river.coords, coords = c("Long", "Lat"),crs=4326)
bird$distance <- apply(st_distance(bird_sf,sky_cords_sf),1,min)

bird.f <- bird %>%
  filter(distance < 3000) %>%
  group_by(year) %>%
  summarise(bird.count= n())


#Change bird count values to percent of total WA observations for 
key <- name_backbone(name='Empidonax difficilis')

y2 <- 2010:2022 %>% as.character
yr_l <- list()
for(i in y2){
  yr_l[[i]] <- occ_count(basisOfRecord = "HUMAN_OBSERVATION",stateProvince="Washington",year=i,country="US",orderKey=key$orderKey)
}

total.obs <- tibble(year=y2 %>% as.integer(),total_n_obs=unlist(yr_l)) %>%
  left_join(bird.f) %>%
  left_join(fish.f)
total.obs$p.bird.count <- total.obs$bird.count/total.obs$total_n_obs

#assessing correlation 

model <- lm(p.bird.count~log(salmon.count)*year,total.obs)
summary(model)
anova(model)

#other things to include in final submission 

#graph showing that we effectively controlled for increasing effort in bird observation data 
bird.sp <-bird %>%
  filter(distance < 3000) %>%
  group_by(year,scientificName) %>%
  summarise(bird.count= n()) %>%
  rename(n_obs=bird.count)

residual.graph <- tibble(year=y2 %>% as.integer(),total_n_obs=unlist(yr_l)) %>% 
  left_join(bird.sp) %>%
  group_by(scientificName) %>% 
  mutate(res=residuals(lm(log(n_obs)~log(total_n_obs)))) %>% 
  ggplot(aes(year,res,col=scientificName))+geom_point()
residual.graph







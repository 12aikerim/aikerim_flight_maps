*Idea and source code was taken from https://github.com/ksenianiglas/mapping_flight_paths/blob/master/flights.R
*Original author of code is Ksenia Niglas.
install.packages(c("maps","rgeos","ggmap","geoshpere","tidyverse","gganimate"))

library(ggplot2)
library(maps)
library(rgeos)
library(maptools)
library(ggmap)
library(geosphere)
library(tidyverse)
library(plyr)
library(gganimate)

#A dataset of my travel routes mannually entered to Google Sheets at https://docs.google.com/spreadsheets/d/1zzfZNwtIEQ2qL6YAh7o8f7Tw9AiUbK42P7-jmwJHGDU/edit?usp=sharing
flights<-read_delim("my travel data.csv",delim = ",")

# Coordinates of cities obtained from https://simplemaps.com/data/world-cities
coords <- read_csv("worldcities.csv") %>%
  select(city_ascii, lat, lng,country)

flights<-flights %>%
  select(Date,From,To,Mode) %>%
  left_join(coords,by=c("From"="city_ascii")) %>% #adding city coordinates and country to Departure cities
  left_join(coords,by=c("To"="city_ascii")) %>% #adding city coordinates and country to Arrival cities
  distinct(Date,From ,To,Mode, .keep_all = TRUE)

#Prepare the Base of world map
worldmap<-map_data("world") #load the mapdata of the world to worldmap var
world<-c(geom_polygon(aes(long,lat,group=group),
                      size=0.1,
                      colour="#e1f7f5",
                      fill="#d7dedd",alpha=0.8, data = worldmap))
fortify.SpatialLinesDataFrame=function(model,data,...){
  ldply(model@lines,fortify)
}
p<-ggplot()+
  world

# Great circle routes
routes <- gcIntermediate(flights[,c('lng.x', 'lat.x')], 
                         flights[,c('lng.y', 'lat.y')], 50, 
                         breakAtDateLine = FALSE, 
                         addStartEnd = TRUE, sp=TRUE)
# Fortify routes to dataframe
fortifiedroutes <- fortify.SpatialLinesDataFrame(routes)

# Add order for animation
fortifiedroutes <- fortifiedroutes %>%
  arrange(group,order) %>%
  mutate(ord = rownames(.) %>% as.numeric()) 

# Create plot + animation
anim <- ggplot() +
  world +
  geom_line(aes(long,lat, group = id), size=.3, data= fortifiedroutes,
            color = "#f26666",
            alpha = .3) +
  theme(panel.background = element_rect(fill='#1f1f1f',colour='#2b2b2b'), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "None",
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) + 
  transition_reveal(fortifiedroutes$ord)

animate(anim, fps = 20, width = 1980, height = 1080, 
        duration = 30, end_pause = 40)
anim_save("flights.gif")

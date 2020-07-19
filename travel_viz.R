#Idea and source code was taken from https://github.com/ksenianiglas/mapping_flight_paths/blob/master/flights.R
#Original author of code is Ksenia Niglas.
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
library(ggrepel)

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
                      fill="#2A7B9A",alpha=0.8, data = worldmap))
# Add city points of visited cities
city_points<-unique(flights %>%
                      select(From,lat.x,lng.x))
city_points$From<-ifelse(city_points$From!="Almaty" & city_points$From!="Seoul" & city_points$From!="Taraz" & city_points$From!="Istanbul","",city_points$From)

g<-ggplot()+
  world+
  geom_point(data=city_points,aes(x=lng.x,y=lat.x,label), color=ifelse(city_points$From=="","white","#C70939"),size=2) +
  coord_fixed(ratio=1.3, xlim=c(-17.4731, 179.3067), ylim=c(-50.3333, 68.9700))+
  geom_text_repel(data=city_points,aes(x=lng.x,y=lat.x,label=From),color="white",size=4,segment.colour = NA) 

g
gfortify.SpatialLinesDataFrame=function(model,data,...){
  ldply(model@lines,fortify)
}

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
anim <- g+
  geom_line(aes(long,lat, group = id), size=1, data= fortifiedroutes,
            color = "#EDDD53",
            alpha = .2) +
  theme(panel.background = element_rect(fill='black',colour='white'), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "None",
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) + 
  transition_reveal(fortifiedroutes$ord)

animate(anim, fps =10,width = 1080, height = 586) #to use ffmeg_renderer install FFmpeg on OS
anim_save("flights.gif")

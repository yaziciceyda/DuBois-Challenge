library(tidyverse)
library(readr)
library(janitor)
library(ggstar)


route_pairs <- read.csv("route-pairs.csv") %>%
  clean_names()

routes <- read.csv("routes.csv")


world <- sf::st_as_sf(countries110)
world <- world %>% clean_names()
africa <- dplyr::filter(world, region_un=='Africa')%>%
  st_transform(crs="+proj=laea +lon_0=18.984375")


america <- dplyr::filter(world, subregion %in% c("Northern America",
                                                 "South America")) %>%
  st_transform(crs = "+proj=laea +lon_0=18.984375")


ggplot(data = america) +
  geom_sf() 


worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
data2 <- worldmap %>%
  unique(continent)

america <- worldmap[worldmap$continent == 'North America' |
                     worldmap$continent == 'South America'  ,]

usa <- ne_states(iso_a2 = 'us', returnclass = "sf")
usa_col[which(usa$name %in% c("Florida", "Georgia", "Alabama", 
                              "Mississippi", "South Carolina"))] <- "black"

usa_col[which(usa$name %in% c("Tennessee", "North Carolina", 
                              "Arkansas", "Louisiana"))] <- "chocolate4"


ggplot() + 
  geom_sf(data = america, fill = "#ffd700") + 
  geom_sf(data = usa, fill = usa_col) +
  geom_circle(aes(x0 = -80, y0 = 15, r = 105)) +
 # geom_star(aes(x = -81.65, y = 30.33), size = 5, fill = "red") +
  coord_sf(xlim = c(28, -195), ylim = c(-90, 120), expand = FALSE) +
  geom_text(aes(x = 20, y = 100, label = "DISTRIBUTION OF"), angle = 130) +
  theme_bw() 


others <- worldmap[worldmap$continent != 'North America' &
                     worldmap$continent != 'South America'&
                   worldmap$continent !=  'Antarctica', ]

ggplot() + geom_sf(data = others,  fill = "#ffd700") + 
  geom_circle(aes(x0 = 80, y0 = 15, r = 115)) +
  coord_sf(xlim = c(-40, 210), ylim = c(-100, 125), expand = FALSE) +
  geom_text(aes(x = -20, y = 100, label = "THE NEGRO RACE"), angle = 90) +
  theme_bw()

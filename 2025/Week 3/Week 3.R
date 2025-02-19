library(tidyverse)
library(janitor)
library(readr)
library(showtext)
library(scales)
library(sf)
library(camcorder)

# Font in the Plot

font_add_google('Public Sans', 'ps')
showtext_auto()


# The data set exists in
# https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge/2025/challenge03

georgia_shp <- sf::read_sf("georgia-1880-county-shapefile")

# John Russell prepared the data to include the colors

data3 <- read.csv("challenge03.csv") %>%
  clean_names() %>%
  mutate(color_code = case_when(
    color == "brown" ~ '#654321',
    color == "crimson" ~ '#dc143c',
    color == "gold" ~ '#ffd700',
    color == "green" ~ '#00aa00',
    color == "lightblue" ~ '#ADD8E6',
    color == "pink" ~ '#ffc0cb',
    color == "tan" ~ '#d2b48c'
  ))


# A quick snapshot of the Georgia map

georgia_shp %>%
  ggplot() +
  geom_sf()


data_georgia <- georgia_shp %>%
  clean_names() %>%
  left_join(data3, by = c("nhgisnam" ="county1890"))




# Define text -------------------------------------------------------------

p <- ggplot(data_georgia) +
  geom_sf(aes(fill = color_code)) +
  geom_sf_text(aes(label = acres_1899), family = "ps") +
  scale_fill_identity() +
    coord_sf(crs = 4326, clip = "off") +
  annotate("text", x = -82, y = 34.3, 
           label = "THE FIGURES INDICATE THE NUMBER OF\nACRES OWNED IN EACH COUNTY IN 1899.",
           family = "ps", hjust = 0, size = 6) +
    xlim(c(-85.5, -79)) +
    ylim(c(30.5, 35)) +
  labs(title = "LAND OWNED BY BLACK PEOPLE IN GEORGIA, U.S.A. 1870 - 1900.",
       caption = "#DuboisChallenge2025 | Week 3 | Prepared by C. YAZICI") +
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.grid = element_blank(),
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(family = "ps", hjust = 0.5, size = 24),
        plot.caption = element_text(family = "ps", hjust = 1, size = 18),
        plot.margin = margin(0, 2.0, 0.5, 2.0, "cm"))

ggsave("Week 3.png", p, width = 15, height = 15, dpi = 72)




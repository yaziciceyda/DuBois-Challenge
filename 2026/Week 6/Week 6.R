library(tidyverse)
library(janitor)
library(readr)
library(showtext)
library(sf)


# Font in the Plot

font_add_google('Public Sans', 'ps')
showtext_auto()

georgia_shp <- sf::read_sf("georgia-1880-county-shapefile") %>%
  clean_names() 

acres_data <- read.csv("ga1899.csv") %>%
  clean_names()  %>%
  select(county1890, acres_1899, color) %>%
  mutate(county1890 = ifelse(county1890 == "McIntosh", "Mcintosh",
                              county1890))

georgia_shp <- georgia_shp %>%
  left_join(acres_data, by = c("nhgisnam" = "county1890")) %>%
  filter(!is.na(color) | color == "")
  
  
p <- ggplot(georgia_shp) +
  geom_sf(aes(fill = color)) +
  geom_sf_text(aes(label = acres_1899), family = "ps", size = 8) +
  scale_fill_identity() +
  coord_sf(crs = 4326, clip = "off") +
  labs(title = "LAND OWNED BY BLACK PEOPLE IN GEORGIA, U.S.A. 1870 - 1900.",
       caption = "#DuboisChallenge26 | Week 6 | Prepared by C. YAZICI") +
  annotate("text", x = -80.5, y = 34.5, family = "ps", hjust = 1, size = 9,
           label = "THE FIGURES INDICATE THE NUMBER OF \nACRES OWNED IN  EACH COUNTY IN 1899.") +
  
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.caption = element_text(family = "ps", size = 30,
                                    hjust = 0.8),
        plot.title = element_text(family = "ps", size = 42,
                                    hjust = 0.5, face = "bold"),
        plot.margin = unit(c(0.6, 0.6, 0.1, 0.6), "cm")) 

# Save the Plot

ggsave("Week6.png", p, width = 20, height = 24, dpi = 72)




library(tidyverse)
library(janitor)
library(readr)
library(showtext)
library(scales)
library(sf)
library(stringr)
library(ggforce)
library(gridExtra)
library(cowplot)

# Font in the Plot

font_add_google('Public Sans', 'ps')
showtext_auto()

georgia_shp <- sf::read_sf("georgia-1880-county-shapefile") %>%
  clean_names() 

data3 <- read.csv("data.csv") %>%
  clean_names() 

pop_data <- read.csv("ga1890pop.csv") %>%
  clean_names() %>%
  mutate(appling01 = str_remove(appling01, "\\s*\\d+$")) 

# John Russell prepared the data to include the colors

data3 <- data3 %>%
  left_join(pop_data, by = c("county" = "appling01")) %>%
  rename(color = gold) %>%
  distinct() %>%
  mutate(color = str_trim(color),
         color_code = case_when(
    color == "chocolate" ~ '#654321',
    color == "crimson" ~ '#dc143c',
    color == "gold" ~ '#ffd700',
    color == "green" ~ '#00aa00',
    color == "lightblue" ~ '#ADD8E6',
    color == "pink" ~ '#ffc0cb',
    color == "tan" ~ '#d2b48c',
    color == "blue" ~ "#28285C",
    color == "black" ~ "black",
  ),
  county = str_to_title(county),
  county = ifelse(trimws(county) == "Dekalb", "DeKalb", trimws(county)))


data_georgia <- georgia_shp %>%
  clean_names() %>%
  left_join(data3, by = c("nhgisnam" ="county"))

# Houston has a different color. It is chocolate in the map.

p1 <- ggplot(data_georgia) +
  geom_sf(aes(fill = color_code)) +
 # geom_sf_text(aes(label = nhgisnam), family = "ps", size = 2) +
  scale_fill_identity() +
  coord_sf(crs = 4326, clip = "off") +
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(1.2, 0.6, 0.1, 0.6), "cm"))


legend_data <- tibble::tibble(
  x = rep(c(5, 14), each = 4),
  y = rep(c(1, 3, 5, 7), 2)
) %>%
  arrange(x, y) %>%
  mutate(level = c("10.000 TO 15.000",
                   "15.000 TO 20.000",
                   "BETWEEN 20.000 AND 30.000",
                   "OVER 30.000",
                   "UNDER 1.000",
                   "1.000 TO 2.500",
                   "2.500 TO 5.000",
                   "5.000 TO 10.000"),
         color_hex = case_when(
           level == "10.000 TO 15.000" ~ "#d2b48c",
           level == "15.000 TO 20.000" ~ "#654321",
           level == "BETWEEN 20.000 AND 30.000" ~ "#28285C",
           level == "OVER 30.000" ~ "black",
           level == "UNDER 1.000" ~ "#00aa00",
           level == "1.000 TO 2.500" ~ "#ffd700",
           level == "2.500 TO 5.000" ~ "#ffc0cb",
           level == "5.000 TO 10.000" ~ "#dc143c",
         ))

p2 <- ggplot(legend_data) +
  geom_circle(aes(x0 = x, y0 = y, r = 0.5, fill = color_hex), color = "black") +
  geom_text(aes(x = x + 0.8, y = y, label = level), hjust = 0,
            family = "ps", size = 10) +
  coord_equal(xlim = c(min(legend_data$x), max(legend_data$x) + 5)) +
  scale_fill_identity() +
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0.6, 0.6, 0.1, 0.6), "cm"))

# The Plots together

p <- grid.arrange(p1, p2, nrow = 2, ncol = 1,
                  heights = c(0.9, 0.4))

p

final_plot <- ggdraw(p) +
  theme(plot.background = element_rect(fill = "#e7d6c5",
                                       color = "#e7d6c5"),
        plot.margin = unit(c(0.1, 0.5, 0.1, 0.5), "cm"),
        aspect.ratio = 9/9) +
  # The title and subtitle of the plot
  draw_label("BLACK POPULATION OF GEORGIA BY COUNTIES.\n\n1890.",
             x = 0.5, y = 0.995,
             fontfamily = "ps", hjust = 0.5, fontface = "bold",
             size = 34) +
  draw_label("#DuboisChallenge26 | Week 3 | Prepared by C. YAZICI",
             x = 0.6, y = 0.001, size = 30,
             fontfamily = "ps", fontface = "bold", hjust = 0.48)


# Save the Plot

ggsave("Week3.png", final_plot, width = 20, height = 22, dpi = 72)





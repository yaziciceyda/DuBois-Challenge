library(tidyverse)
library(readr)
library(showtext)
library(sf)
library(janitor)
library(ggpattern)
library(gridExtra)


# Font in the Plot

font_add_google('Public Sans', 'ps')
showtext_auto()


# import data
data2 <- readr::read_csv("data.csv")
map_file <- st_read("C:\\Users\\Ceyda\\OneDrive\\Desktop\\DataViz\\DuBois\\2026\\Week 2\\cb_2018_us_state_5m\\cb_2018_us_state_5m.shp") 

map_file <- map_file %>%
  clean_names() %>%
  filter(name != "Alaska")

data2 <- data2 %>%
  clean_names() %>%
  mutate(population = case_when(
    state == "UT" ~ "UNDER - 10,000",
    state == "WA" ~ "UNDER - 10,000",
    state == "OR" ~ "UNDER - 10,000",
    state == "ID" ~ "UNDER - 10,000",
    state == "MT" ~ "UNDER - 10,000",
    state == "ND" ~ "UNDER - 10,000",
    state == "SD" ~ "UNDER - 10,000",
    state == "MN" ~ "UNDER - 10,000",
    state == "WI" ~ "UNDER - 10,000",
    state == "WY" ~ "UNDER - 10,000",
    state == "NV" ~ "UNDER - 10,000",
    state == "NM" ~ "UNDER - 10,000",
    state == "CO" ~ "UNDER - 10,000",
    state == "NE" ~ "UNDER - 10,000",
    state == "OK" ~ "UNDER - 10,000",
    state == "ME" ~ "UNDER - 10,000",
    state == "VT" ~ "UNDER - 10,000",
    state == "NH" ~ "UNDER - 10,000",
    state == "FL" ~ "100,000 - 200,000",
    state == "MO" ~ "100,000 - 200,000",
    state == "PA" ~ "100,000 - 200,000",
    state == "LA" ~ "500,000 - 600,000",
    state == "SC" ~ "600,000 - 750,000",
    TRUE ~ population
  ),
  pop_hex = case_when(
    population == "750,000 AND OVER" ~ "black",
    population == "600,000 - 750,000" ~ "#D4CDCD",
    population == "500,000 - 600,000" ~ "#BAABAB",
    population == "300,000 - 500,000" ~ "#654321",
    population == "200,000 - 300,000" ~ "#28285C",
    population == "100,000 - 200,000" ~ "#d2b48c",
    population == "50,000 - 100,000" ~ "#dc143c",
    population == "25,000 - 50,000" ~ "#ffc0cb",
    population == "10,000 - 25,000" ~ "#ffd700",
    population == "UNDER - 10,000" ~ "#d3d3d3",
  ))
  

map_file <- map_file %>%
  left_join(data2, by = c("stusps" = "state")) %>%
  mutate(
    pattern_type = case_when(
      population == "600,000 - 750,000" ~ "crosshatch",
      population == "500,000 - 600,000" ~ "stripe",
      TRUE ~ "none"
    )
  )

map_file <- sf::st_transform(map_file, 4326)
bbox <- st_bbox(c(
  xmin = -130,
  xmax = -67,
  ymin = 22,
  ymax = 52
), crs = 4326)

map_crop <- st_crop(map_file, bbox)

p1 <- ggplot(map_crop) +
  geom_sf(aes(fill = pop_hex)) +
  geom_sf_pattern(aes(pattern = pattern_type), fill = NA,
                  pattern_angle = -45, 
                  pattern_density = 0.1,
                  pattern_spacing = 0.03) +
  scale_pattern_identity() +
  scale_fill_identity() +
  coord_sf(
    xlim = c(-130, -67),
    ylim = c(22, 52),
    expand = FALSE,
    lims_method = "geometry_bbox"
  ) +
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0.6, 0.6, 0.1, 0.6), "cm"))
              
######

# Legend 

legend_data <- tibble::tibble(
  x = rep(c(5.5, 6.5), 5),
  y = rep(c(1, 3, 5, 7, 9), 2)
) %>%
  arrange(x, y) %>%
  mutate(level = c("200,000 - 300,000", "300,000 - 500,000",
                   "500,000 - 600,000", "600,000 - 750,000",
                   "750,000 AND OVER", "UNDER - 10,000",
                   "10,000 - 25,000", "25,000 - 50,000",
                   "50,000 - 100,000", "100,000 - 200,000"),
         fill_hex = case_when(
           level == "750,000 AND OVER" ~ "black",
           level == "600,000 - 750,000" ~ "#D4CDCD",
           level == "500,000 - 600,000" ~ "#BAABAB",
           level == "300,000 - 500,000" ~ "#654321",
           level == "200,000 - 300,000" ~ "#28285C",
           level == "100,000 - 200,000" ~ "#d2b48c",
           level == "50,000 - 100,000" ~ "#dc143c",
           level == "25,000 - 50,000" ~ "#ffc0cb",
           level == "10,000 - 25,000" ~ "#ffd700",
           level == "UNDER - 10,000" ~ "#d3d3d3"),
         pattern_type = case_when(
           level == "600,000 - 750,000" ~ "crosshatch",
           level == "500,000 - 600,000" ~ "stripe",
           TRUE ~ "none"))


  
p2 <- ggplot(legend_data) +
  geom_rect(aes(xmin = x, xmax = x + 0.15,
                ymin = y, ymax = y + 1.5,
                fill = fill_hex), color = "black") +
  geom_text(aes(x = x + 0.30, y = y + 0.65, label = level),
            size = 7, family = "ps",
             hjust = 0)  +
  scale_fill_identity() +
  geom_rect_pattern(aes(xmin = x, xmax = x + 0.15,
                        ymin = y, ymax = y + 1.5,
                        pattern = pattern_type), fill = NA,
                  pattern_angle = -45, 
                  pattern_density = 0.1,
                  pattern_spacing = 0.03) +
  scale_pattern_identity() +
  coord_cartesian(xlim = c(4.9, 7.5),
                  ylim = c(0, 10)) +
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0.1, 0.8, 0.1, 0.8), "cm"))


# The Plots together

p <- grid.arrange(p1, p2, nrow = 2, ncol = 1,
                  heights = c(0.9, 0.4))


final_plot <- ggdraw(p) +
  theme(plot.background = element_rect(fill = "#e7d6c5",
                                       color = "#e7d6c5"),
        plot.margin = unit(c(0.1, 0.5, 0.1, 0.5), "cm"),
        aspect.ratio = 9/9) +
# The title and subtitle of the plot
draw_label("RELATIVE BLACK POPULATION OF THE STATES OF THE\n\n UNITED STATES.",
           x = 0.5, y = 0.97,
           fontfamily = "ps", hjust = 0.5, fontface = "bold",
           size = 33) +
  draw_label("#DuboisChallenge26 | Week 2 | Prepared by C. YAZICI",
             x = 0.56, y = 0.001, size = 23,
             fontfamily = "ps", fontface = "bold", hjust = 0.48)


# Save the Plot

ggsave("Week2.png", final_plot, width = 20, height = 22, dpi = 72)



library(tidyverse)
library(readr)
library(showtext)
library(sf)
library(janitor)
library(ggbrace)


# Font in the Plot

font_add_google('Public Sans', 'ps')
showtext_auto()

# import data
data8 <- readr::read_csv("data.csv")

data8 <- data8 %>%
  clean_names() %>%
  mutate(color_category = case_when(
    category == "Less than 1" ~ "#ffd700",
    category == "1-4" ~ "#43439d",
    category == "4-8" ~ "#dc143c",
    category == "8-15" ~ "#654321",
    category == "15-25" ~ "black",
  ))
  
  
  
map_file <- st_read("C:\\Users\\Ceyda\\OneDrive\\Desktop\\DataViz\\DuBois\\2026\\Week 2\\cb_2018_us_state_5m\\cb_2018_us_state_5m.shp") 

map_file <- map_file %>%
  clean_names() %>%
  filter(name != "Alaska")

map_file <- sf::st_transform(map_file, 4326)
bbox <- st_bbox(c(
  xmin = -130,
  xmax = -67,
  ymin = 22,
  ymax = 52
), crs = 4326)

map_file <- map_file %>%
  left_join(data8, by = c("stusps" = "state")) 
  
  
map_crop <- st_crop(map_file, bbox)


p1 <- ggplot(map_crop) +
  geom_sf(aes(fill = color_category)) +
  coord_sf(
    xlim = c(-130, -67),
    ylim = c(22, 52),
    expand = FALSE,
    lims_method = "geometry_bbox"
  ) +
  scale_fill_identity() +
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(4, 0.5, 0.5, 1), "cm"))



legend_data <- tibble(
  x = c(1, 3, 5, 7, 9)
) %>% 
  mutate(y = 2,
         color_hex = c("#ffd700", "#43439d", "#dc143c", "#654321", "black"),
           label = c("LESS THAN\n1", "1-4", "4-8", "8-15", "15-25"))

brace_data <- tibble(
  x = c(-0.5, 10),
  y = c(1.5, 2.1)
)


p2 <- ggplot(legend_data) +
  geom_point(aes(x = x, y = y), color = "black", size = 31, stroke = 1) +
  geom_point(aes(x = x, y = y, color = color_hex), size = 30) +
  geom_text(aes(x = x, y = 1, label = label), family = "ps", size = 10) +
  scale_color_identity() +
  # Brace
  stat_brace(
    data = brace_data,
    aes(x = x,  y = y),
    rotate = 180,
    width = 1.5
  ) +
  annotate("text", label = "BLACK PEOPLE TO THE SQUARE MILE.",
           x = 5, y = -0.4, family = "ps", size = 10) +
  coord_fixed(xlim = c(-0.9, 10),
              ylim = c(-1.5, 8),
              expand = FALSE) +
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0.5, 7.5, -0.5, 8.5), "cm"))



p <- grid.arrange(p2, p1, nrow = 2,   
                  heights = c(1, 1))

final_plot1 <-  ggdraw(p) +
  theme(plot.background = element_rect(fill = "#e7d6c5", 
                                       color = "#e7d6c5"),
        plot.margin = unit(c(-7, 0, 0.9, 0), "cm")) +
  # title 
  draw_label("DISTRIBUTION OF BLACK PEOPLE IN THE UNITED STATES.",
             x = 0.5, y = 0.88, hjust = 0.5, fontfamily = "ps",
             size = 43) +
  # subtitle 
  draw_label("DISTRIBUTION DES NOIRS DAN LES ETATS UNIS.",
             x = 0.5, y = 0.83, hjust = 0.5, fontfamily = "ps",
             size = 35, color = "grey50") +
  # caption
  draw_label("#DuboisChallenge26| Week 8 | Prepared by C. YAZICI",
             x = 0.85, y = 0.01, hjust = 0.8, fontfamily = "ps",
             size = 30) 
  

#final_plot1

# Save the Plot

ggsave("Week 8.png", final_plot1, width = 24, height = 28, dpi = 72)



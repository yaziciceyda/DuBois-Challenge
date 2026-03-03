library(tidyverse)
library(sf)
library(janitor)
library(showtext)
library(readr)
library(patchwork)

# Font in the Plot

font_add_google('Public Sans', 'ps')
showtext_auto()


# The data set exists in
# https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge/2026/challenge05

# import data
data5 <- readr::read_csv("present.csv") %>%
  clean_names() 

  data5 <- data5 %>%
    mutate(state = as.character(state)) %>%
  add_row(
    state = c("AR"),
    present_location = c(12018)
  ) %>%
  filter(!(state == "MN" & present_location == 38))

map_file <- st_read("C:\\Users\\Ceyda\\OneDrive\\Desktop\\DataViz\\DuBois\\2026\\Week 5\\cb_2018_us_state_5m\\cb_2018_us_state_5m.shp") 

map_file1 <- map_file %>%
  clean_names() %>%
  filter(name != "Alaska") %>%
  full_join(data5, by = c("stusps" = "state")) %>%
  rename(state = stusps) %>%
  mutate(color_hex = case_when(
    state == "WA" ~ "#ebd7bb",
    state == "OR" ~ "#dc143c",
    state == "CA" ~ "#ffd700",
    state == "MT" ~ "#00aa00",
    state == "ID" ~ "#4682b4",
    
    state == "NV" ~ "#ffc0cb",
    state == "UT" ~ "#654321",
    state == "AZ" ~ "#4682b4",
    state == "WY" ~ "#ffd700",
    state == "CO" ~ "#dc143c",
    
    state == "NM" ~ "#d2b48c",
    state == "ND" ~ "#d2b48c",
    state == "SD" ~ "#654321",
    state == "NE" ~ "#ffc0cb",
    state == "KS" ~ "#00aa00",
    
    state == "OK" ~ "#ffd700",
    state == "TX" ~ "#ebd7bb",
    state == "MN" ~ "#ffc0cb",
    state == "IA" ~ "#ebd7bb",
    state == "MO" ~ "#4682b4",
    
    state == "AR" ~ "#dc143c",
    state == "LA" ~ "#ffc0cb",
    state == "MI" ~ "#654321",
    state == "WI" ~ "#4682b4",
    state == "IL" ~ "#ffd700",
    
    state == "IN" ~ "#ffc0cb",
    state == "KY" ~ "#ebd7bb",
    state == "TN" ~ "#654321",
    state == "MS" ~ "#d2b48c",
    state == "OH" ~ "#d2b48c",
    
    
    state == "AL" ~ "#00aa00",
    state == "GA" ~ "black",
    state == "FL" ~ "#ffd700",
    state == "SC" ~ "#4682b4",
    state == "NC" ~ "#ffc0cb",
    
    state == "VA" ~ "#00aa00",
    state == "WV" ~ "#654321",
    state == "MD" ~ "#ffd700",
    state == "PA" ~ "#dc143c",
    state == "NY" ~ "#4682b4",
    
    state == "VT" ~ "#ffc0cb",
    state == "ME" ~ "#ffd700",
    state == "NH" ~ "#ebd7bb",
    state == "MA" ~ "#d2b48c",
    state == "CT" ~ "#ffd700",
    
    state == "RI" ~ "#dc143c",
    state == "NJ" ~ "#ebd7bb",
    state == "DW" ~ "#ebd7bb",
   # state == "DE" ~ "#d2b48c",
    
  ),
  color_label = ifelse(state == "GA", "white", "black")
  )


# Map 

map_file1 <- sf::st_transform(map_file1, 4326)
bbox <- st_bbox(c(
  xmin = -130,
  xmax = -67,
  ymin = 22,
  ymax = 52
), crs = 4326)

map_crop <- st_crop(map_file1, bbox)

state_centroids <- map_crop %>%
  st_point_on_surface()

state_centroids <- state_centroids %>%
  mutate(
    x = st_coordinates(.)[,1],
    y = st_coordinates(.)[,2]
  ) %>%
  mutate(yend = case_when(
    state == "MI" ~ + 1,
    state == "WI" ~  + 1,
    state == "IL" ~  + 1,
    state == "KY" ~ + 1,
    state == "OH" ~ + 1,
    state == "FL" ~ -2,
    .default = 0.6
  ),
  xend = case_when(
    state == "MN" ~ -0.2,
    state == "MI" ~ -0.2,
    state == "WI" ~  -0.2,
    state == "IL" ~ -0.2,
    state == "IN" ~ -0.2,
    state == "KY" ~ -0.2,
    state == "TN" ~ -0.2,
    state == "OH" ~ -0.2,
    state == "PA" ~ +0.7,
    state == "NY" ~ +0.7,
    state == "VT" ~ +0.2,
    state == "ME" ~ -0.2,
    state == "NH" ~ +0.2,
    state == "FL" ~ 0.7,
    .default = -0.5
  ))



p1 <- ggplot() +
  geom_sf(data = map_crop, mapping = aes(fill = color_hex)) +
  geom_sf_text(map_crop, mapping = aes(label = present_location,
                                       color = color_label), size = 4,
               nudge_x = -0.6, nudge_y = 1.0,
               family = "ps") +
  scale_fill_identity() +
  scale_color_identity() +
  geom_segment(
    data = state_centroids,
    aes(
      x = x + 0.3,
      y = y - 0.7,
      xend = x + xend,
      yend = y + yend   # controls arrow length
    ),
    arrow = arrow(length = unit(0.4, "cm")),
    color = "black"
  ) +
  labs(caption = "PRESENT DWELLING PLACE OF BLACK PEOPLE BORN IN GEORGIA.") +
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.caption = element_text(family = "ps", size = 25, color = "grey50",
                                    hjust = 0.5),
        plot.margin = unit(c(0.6, 0.6, 0.1, 0.6), "cm"))
  
#### Second Plot

# import data
birth <- readr::read_csv("birthplace.csv") %>%
  clean_names() 

birth <- birth %>%
  mutate(state = as.character(state)) %>%
  add_row(
    state = c("AR", "WI"),
    birthplace = c(97, 3)
  )

map_file2 <- map_file %>%
  clean_names() %>%
  filter(name != "Alaska") %>%
  full_join(birth, by = c("stusps" = "state")) %>%
  rename(state = stusps) %>%
  mutate(color_hex = case_when(
    state == "WA" ~ "#654321",
    state == "OR" ~ "#ebd7bb",
    state == "CA" ~ "#ffc0cb",
    state == "MT" ~ "#dc143c",
    state == "ID" ~ "#ffd700",
    
    state == "NV" ~ "#00aa00",
    state == "UT" ~ "#4682b4",
    state == "AZ" ~ "#d2b48c",
    state == "WY" ~ "#654321",
    state == "CO" ~ "#ebd7bb",
    
    state == "NM" ~ "#ffc0cb",
    state == "ND" ~ "#ebd7bb",
    state == "SD" ~ "#4682b4",
    state == "NE" ~ "#d2b48c",
    state == "KS" ~ "#00aa00",
    
    state == "OK" ~ "#dc143c",
    state == "TX" ~ "#ffd700",
    state == "MN" ~ "#ffd700",
    state == "IA" ~ "#00aa00",
    state == "MO" ~ "#ffc0cb",
    
    state == "AR" ~ "#ebd7bb",
    state == "LA" ~ "#d2b48c",
    state == "MI" ~ "#ebd7bb",
    state == "WI" ~ "#4682b4",
    state == "IL" ~ "#dc143c",
    
    state == "IN" ~ "#654321", 
    state == "KY" ~ "#ffd700",
    state == "TN" ~ "#00aa00",
    state == "MS" ~ "#4682b4",
    state == "OH" ~ "#ffc0cb",
    
    
    state == "AL" ~ "#ffc0cb",
    state == "GA" ~ "black",
    state == "FL" ~ "#ebd7bb",
    state == "SC" ~ "#dc143c",
    state == "NC" ~ "#654321",
    
    state == "VA" ~ "#ffc0cb",
    state == "WV" ~ "#d2b48c",
    state == "MD" ~ "#00aa00",
    state == "PA" ~ "#4682b4",
    state == "NY" ~ "#ffd700",
    
    state == "VT" ~ "#ebd7bb",
    state == "ME" ~ "#ebd7bb",
    state == "NH" ~ "#4682b4",
    state == "MA" ~ "#dc143c",
    state == "CT" ~ "#00aa00",
    
    state == "RI" ~ "#ffd700",
    state == "NJ" ~ "#d2b48c",
    state == "DE" ~ "#dc143c",
    state == "DC" ~ "#dc143c"
  ),
  color_label = ifelse(state == "GA", "white", "black")
  )

map_file2 <- sf::st_transform(map_file2, 4326)
bbox <- st_bbox(c(
  xmin = -130,
  xmax = -67,
  ymin = 22,
  ymax = 52
), crs = 4326)

map_crop2 <- st_crop(map_file2, bbox)

states_present <- map_file2 %>%
  filter(!is.na(birthplace))

centroids <- states_present %>%
  st_point_on_surface()

centroids <- centroids %>%
  mutate(
    x = st_coordinates(.)[,1],
    y = st_coordinates(.)[,2],
    xend_add = case_when(
      state == "OR" ~ 1.5,
      state == "ID" ~ 1.5,
      state == "UT" ~ 1.5,
      state == "NM" ~ 1.5,
      state == "NE" ~ 1.5,
      state == "KS" ~ 1.5,
      state == "OK" ~ 1.5,
      state == "TX" ~ 2.8,
      state == "MN" ~ 1.4,
      state == "WI" ~ 1.4,
      state == "IL" ~ 1.4,
      state == "MO" ~ 1.4,
      state == "AR" ~ 1.1,
      state == "LA" ~ 1.3,
      state == "MS" ~ 1.0,
      state == "MI" ~ 1.2,
      state == "IN" ~ 0.8,
      state == "KY" ~ 0.8,
      state == "TN" ~ 0.8,
      state == "AL" ~ 1.2,
      state == "FL" ~ -0.6,
      state == "OH" ~ -0.2,
      state == "ME" ~ -0.4,
      state == "NY" ~ -0.4,
      state == "PA" ~ -0.4,
      state == "WV" ~ -0.4,
      state == "VA" ~ -0.4,
      state == "SC" ~ -0.4,
      state == "NC" ~ -0.4,
    ),
    yend_add = case_when(
      state == "OR" ~ -1.4,
      state == "ID" ~ -1.4,
      state == "UT" ~ -1.4,
      state == "NM" ~ 0.2,
      state == "NE" ~ -0.8,
      state == "KS" ~ -0.8,
      state == "OK" ~ -0.8,
      state == "TX" ~ 0.6,
      state == "MN" ~ -1.1,
      state == "WI" ~ -1.1,
      state == "IL" ~ -1.1,
      state == "MO" ~ -1.1,
      state == "AR" ~ -0.8,
      state == "LA" ~ 0.4,
      state == "MS" ~ -0.4,
      state == "MI" ~ -4.2,
      state == "IN" ~ -1.0,
      state == "KY" ~ -1.0,
      state == "TN" ~ -0.8,
      state == "AL" ~ 0.5,
      state == "FL" ~ 1.7,
      state == "OH" ~ -0.9,
      state == "ME" ~ -0.9,
      state == "NY" ~ -0.7,
      state == "PA" ~ -0.7,
      state == "WV" ~ -0.7,
      state == "VA" ~ -0.7,
      state == "SC" ~ -0.7,
      state == "NC" ~ -0.7,
    ),
    x = ifelse(state == "LA", -93, x),
    y = ifelse(state == "LA", 32, y)
  )


p2 <- ggplot() +
  geom_sf(data = map_crop2, mapping = aes(fill = color_hex)) +
  geom_sf_text(map_crop2, mapping = aes(label = birthplace,
                                       color = color_label), size = 4,
               nudge_x = -0.6, nudge_y = -1.0,
               family = "ps") +
  scale_fill_identity() +
  scale_color_identity() +
  geom_segment(
    data = centroids,
    aes(
      x = x + 0.3,
      y = y - 0.7,
      xend = x + xend_add,
      yend = y + yend_add   # controls arrow length
    ),
    arrow = arrow(length = unit(0.4, "cm")),
    color = "black"
  ) +
  labs(caption = "BIRTH PLACE OF BLACK PEOPLE NOW RESIDENT IN GEORGIA.") +
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.caption = element_text(family = "ps", size = 25, color = "grey50",
                                    hjust = 0.2),
        plot.margin = unit(c(0.6, 0.6, 0.1, 0.6), "cm"))


# The Final Plot

final_plot <- p1 + p2 + plot_layout(nrow = 2, 
                                            heights = c(1.0, 1.0)) +
  plot_annotation(
    title = "MIGRATION OF BLACK PEOPLE.\n1890.",
    caption = "#DuboisChallenge26| Week 5 | Prepared by C. YAZICI") &
  theme(plot.title = element_text(family = "ps", hjust = 0.5, 
                                  size = 30,
                                  face = "bold"),
        plot.caption = element_text(family = "ps", size = 20, 
                                    hjust = 1, face = "bold"),
        plot.background = element_rect(color = "#e7d6c5", fill = "#e7d6c5"),
        panel.background = element_rect(color = "#e7d6c5", fill = "#e7d6c5"),
        plot.margin = margin(1.0, 0.7, 0.5, 0.7, "cm"))


# Save the Plot

ggsave("Week5.png", final_plot, width = 16, height = 20, dpi = 72)







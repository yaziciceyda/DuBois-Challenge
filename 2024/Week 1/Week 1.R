library(tidyverse)
library(sf)
library(janitor)
library(showtext)
library(patchwork)

# Font in the Plot

font_add_google('Public Sans', 'ps')
showtext_auto()


# The data set exists in
# https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge/2024/challenge01

georgia_shp <- sf::read_sf("georgia-1880-county-shapefile")

# A quick snapshot of the Georgia map

georgia_shp%>%
  ggplot()+
  geom_sf()


# Data Preparation

data_georgia <- georgia_shp %>%
  clean_names() %>%
   mutate(data1870 = case_when(
    data1870 == "> 1000" ~ "LESS THAN 1,000",
    data1870 == "1000 - 2500" ~ "1,000 TO 2,500",
    data1870 == "2500 - 5000" ~ "2,500 TO 5,000",
    data1870 == "5000 - 10000" ~ "5,000 TO 10,000",
    data1870 == "10000 - 15000" ~ "10,000 TO 15,000",
    data1870 == "15000 - 20000" ~ "15,000 TO 20,000",
    data1870 == "20000 - 30000" ~ "BETWEEN 20,000 AND 30,000",
  ),
  data1880_p = case_when(
    data1880_p == "> 1000" ~ "LESS THAN 1,000",
    data1880_p == "1000 - 2500" ~ "1,000 TO 2,500",
    data1880_p == "2500 - 5000" ~ "2,500 TO 5,000",
    data1880_p == "5000 - 10000" ~ "5,000 TO 10,000",
    data1880_p == "10000 - 15000" ~ "10,000 TO 15,000",
    data1880_p == "15000 - 20000" ~ "15,000 TO 20,000",
    data1880_p == "20000 - 30000" ~ "BETWEEN 20,000 AND 30,000",
  ),
  color_1870 = case_when(
    data1870 == "LESS THAN 1,000" ~ "#00aa00",
    data1870 == "1,000 TO 2,500" ~ "#ffd700",
    data1870 == "2,500 TO 5,000" ~ "#ffc0cb",
    data1870 == "5,000 TO 10,000" ~ "#dc143c",
    data1870 == "BETWEEN 20,000 AND 30,000" ~ "#0000ff",
    data1870 == "15,000 TO 20,0000" ~ "#654321",
    data1870 == "10,000 TO 15,000" ~ "#d2b48c",
    .default = "#e7d6c5"
  ),
  color_1880 = case_when(
    data1880_p == "LESS THAN 1,000" ~ "#00aa00",
    data1880_p == "1,000 TO 2,500" ~ "#ffd700",
    data1880_p == "2,500 TO 5,000" ~ "#ffc0cb",
    data1880_p == "5,000 TO 10,000" ~ "#dc143c",
    data1880_p == "BETWEEN 20,000 AND 30,000" ~ "#0000ff",
    data1880_p == "15,000 TO 20,0000" ~ "#654321",
    data1880_p == "10,000 TO 15,000" ~ "#d2b48c",
    .default = "#e7d6c5"
  ))

# Plot of 1870

p_1870 <- data_georgia %>%
  ggplot()+
  geom_sf(aes(fill = color_1870)) +
  scale_fill_identity() +
  coord_sf(crs = 4326, clip = "off") +
  xlim(c(-85.5, -72)) +
   ylim(c(30.5, 35.2)) +
  # The title as the year (1870)
  annotate("text", x = -84.4, y = 35.15, label = "1870", family = "ps",
           fontface = "bold", color ="#413528", size = 7) +
  # Legend
    # BETWEEN 20,000 AND 30,000
  annotate("point", x = -80, y = 34.5, color = "#0000ff", size = 10,
           stroke = 1) +
  annotate("text", x = -79, y = 34.5, label = "BETWEEN 20,000 AND 30,000",
           color = "#a4917f", size = 8, family = "ps", hjust = 0) +
    # 15,000 TO 20,000
  annotate("point", x = -80, y = 33.7, color = "#654321", size = 10,
           stroke = 1) +
  annotate("text", x = -79, y = 33.7, label = "15,000 TO 20,000",
           color = "#a4917f", size = 8, family = "ps", hjust = 0) +
    # 10,000 TO 15,000
  annotate("point", x = -80, y = 32.9, color = "#d2b48c", size = 10,
           stroke = 1) +
  annotate("text", x = -79, y = 32.9, label = "10,000 TO 15,000",
           color = "#a4917f", size = 8, family = "ps", hjust = 0) +
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.grid = element_blank(),
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.margin = margin(0.5, 0.5, 0.1, 0.5, "cm"))
     

#Plot of 1880

p_1880 <- data_georgia %>%
  ggplot()+
  geom_sf(aes(fill = color_1880)) +
  scale_fill_identity() +
  coord_sf(crs = 4326, clip = "off") +
   xlim(c(-95, -81)) +
   ylim(c(30.5, 35.2)) +
   # The title as the year (1880)
   annotate("text", x = -84.4, y = 35.15, label = "1880", family = "ps",
            fontface = "bold", color ="#413528", size = 7) +
   # Legend
    # 5,000 TO 10,000
   annotate("point", x = -93, y = 34, color = "#dc143c", size = 10,
            stroke = 1) +
   annotate("text", x = -92, y = 34, label = "5,000 TO 10,000",
            color = "#a4917f", size = 8, family = "ps", hjust = 0) +
    # 2,500 TO 5,000
   annotate("point", x = -93, y = 33.3, color = "#ffc0cb", size = 10,
            stroke = 1) +
   annotate("text", x = -92, y = 33.3, label = "2,500 TO 5,000",
            color = "#a4917f", size = 8, family = "ps", hjust = 0) +
    # 1,000 TO 2,500
   annotate("point", x = -93, y = 32.6, color = "#ffd700", size = 10,
            stroke = 1) +
   annotate("text", x = -92, y = 32.6, label = "1,000 TO 2,500",
            color = "#a4917f", size = 8, family = "ps", hjust = 0) +
    # UNDER 1,000
   annotate("point", x = -93, y = 31.9, color = "#00aa00", size = 10,
            stroke = 1) +
   annotate("text", x = -92, y = 31.9, label = "UNDER 1,000",
            color = "#a4917f", size = 8, family = "ps", hjust = 0) +
   theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
         plot.background = element_rect(fill = "#e7d6c5", color = NA),
         panel.grid = element_blank(),
         axis.title = element_blank(), 
         axis.ticks = element_blank(),
         axis.text = element_blank(),
         plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))
        

# The Final Plot

final_plot <- p_1870 + p_1880 + plot_layout(nrow = 2, 
                                            heights = c(1.1, 1.0)) +
  plot_annotation(
    title = "NEGRO POPULATION OF GEORGIA BY COUNTIES",
    caption = "DuBoisChallenge2024 | Week 1 |Prepared by C.YAZICI"
  ) +
  plot_annotation(
    title = "NEGRO POPULATION OF GEORGIA BY COUNTIES.",
    caption = "#DuboisChallenge24| Week 1 | Prepared by C. YAZICI") &
  theme(plot.title = element_text(family = "ps", hjust = 0.5, 
                                  size = 20,
                                  face = "bold"),
        plot.caption = element_text(family = "ps", size = 15, 
                                    hjust = 1, face = "bold"),
        plot.background = element_rect(color = "#e7d6c5", fill = "#e7d6c5"),
        panel.background = element_rect(color = "#e7d6c5", fill = "#e7d6c5"),
        plot.margin = margin(0.5, 0.7, 0.5, 0.7, "cm"))


# Save the Plot

ggsave("Week1.png", final_plot, width = 18, height = 15, dpi = 72)




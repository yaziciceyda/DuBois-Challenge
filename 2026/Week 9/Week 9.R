library(tidyverse)
library(readr)
library(showtext)
library(sf)
library(janitor)
library(cowplot)



# Font in the Plot

font_add_google('Public Sans', 'ps')
showtext_auto()


# import data
data9 <- readr::read_csv("data.csv")


# Spain

esp_file <- st_read("C:\\Users\\Ceyda\\OneDrive\\Desktop\\DataViz\\DuBois\\2026\\Week 9\\esp-shapefile\\world-administrative-boundaries.shp") 

esp_file <- esp_file %>%
  clean_names() 

p1 <- ggplot(esp_file) +
  geom_sf(color = "red", fill = "#e7d6c5", linewidth = 2) +
  geom_sf_text(label = "ESPANA\n17,500,000", family = "ps",
               size = 10, fontface = "bold") +
  coord_sf(xlim = c(-10, 4), ylim = c(35, 45),
           expand = FALSE) +
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        panel.spacing = unit(0, "lines"))

# Australia

aus_file <- st_read("C:\\Users\\Ceyda\\OneDrive\\Desktop\\DataViz\\DuBois\\2026\\Week 9\\aus-shapefile\\world-administrative-boundaries.shp") 

aus_file <- aus_file %>%
  clean_names() 



p2 <- ggplot(aus_file) +
  geom_sf(color = "red", fill = "#e7d6c5", linewidth = 2) +
  geom_sf_text(label = "AUSTRALIA\n3,036,570" , family = "ps",
               size = 8, fontface = "bold") +
  coord_sf(xlim = c(100, 175), 
           ylim = c(-54, 11),
           expand = FALSE) +
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        panel.spacing = unit(0, "lines"))


# Norway  

nor_file <- st_read("C:\\Users\\Ceyda\\OneDrive\\Desktop\\DataViz\\DuBois\\2026\\Week 9\\nor-shapefile\\world-administrative-boundaries.shp") 

nor_file <- nor_file %>%
  clean_names() 

ggplot(nor_file) +
  geom_sf(color = "red", fill = "#e7d6c5") +
  geom_sf_text(label = "NEDERLANDEN\n4,500,000")

# Swiss

swe_file <- st_read("C:\\Users\\Ceyda\\OneDrive\\Desktop\\DataViz\\DuBois\\2026\\Week 9\\swe-shapefile\\world-administrative-boundaries.shp") 

swe_file <- swe_file %>%
  clean_names() 

ggplot(swe_file) +
  geom_sf(color = "red", fill = "#e7d6c5") +
  geom_sf_text(label = "NEDERLANDEN\n4,500,000")

nor_che <- nor_file %>%
  add_row(swe_file) %>%
  mutate(label = ifelse(iso_3_terri == "NOR", "NORVEGE\n2,000,917",
                        "SUEDE\n4,774,409"))

p3 <- ggplot(nor_che) +
  geom_sf(color = "red", fill = "#e7d6c5", linewidth = 2) +
  geom_sf_text(aes(label = label), family = "ps",
               size = 8, fontface = "bold") +
  coord_sf(xlim = c(4, 32), ylim = c(55, 71),
           expand = FALSE) +
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        panel.spacing = unit(0, "lines"))


# Nederland

nld_file <- st_read("C:\\Users\\Ceyda\\OneDrive\\Desktop\\DataViz\\DuBois\\2026\\Week 9\\nld-shapefile\\world-administrative-boundaries.shp") 

nld_file <- nld_file %>%
  clean_names() 

ggplot(nld_file) +
  geom_sf(color = "red") +
  geom_sf_text(label = "NEDERLANDEN\n4,500,000")

# Belgium

bel_file <- st_read("C:\\Users\\Ceyda\\OneDrive\\Desktop\\DataViz\\DuBois\\2026\\Week 9\\bel-shapefile\\world-administrative-boundaries.shp") 

bel_file <- bel_file %>%
  clean_names() 

ggplot(bel_file) +
  geom_sf(color = "red") +
  geom_sf_text(label = "L. BELGIQUE\n6,000,000")

# Nederland & Belgium 

nld_bel <- nld_file %>%
  add_row(bel_file) %>%
  mutate(label = ifelse(iso_3_terri == "NLD", "NEDERLANDEN\n4,500,000",
         "L. BELGIQUE\n6,000,000"))


p4 <- ggplot(nld_bel) +
  geom_sf(color = "red", fill = "#e7d6c5", linewidth = 2) +
  geom_sf_text(aes(label = label), family = "ps",
               size = 8, fontface = "bold") +
  coord_sf(xlim = c(2.5, 7.5), ylim = c(49.5, 53.5),
           expand = FALSE) +
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        panel.spacing = unit(0, "lines"))



# usa

usa_file <- st_read("C:\\Users\\Ceyda\\OneDrive\\Desktop\\DataViz\\DuBois\\2026\\Week 9\\usa-shapefile\\world-administrative-boundaries.shp") 

usa_file <- usa_file %>%
  clean_names()

p5 <- ggplot(usa_file) +
  geom_sf(color = "red", fill = "black", linewidth = 2) +
  annotate("text", x = -100, y = 24,
           label = "U.S.A.\nBLACK POPULATION\nPOPULATION NOIRE\n7,500,00",
           vjust = 1, family = "ps", size = 10,
           lineheight = 0.9, fontface = "bold") +
  coord_sf(
    xlim = c(-130, -67),
    ylim = c(12, 51),
    expand = FALSE) +
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        panel.spacing = unit(0, "lines"))



# switzerland

che_file <- st_read("C:\\Users\\Ceyda\\OneDrive\\Desktop\\DataViz\\DuBois\\2026\\Week 9\\che-shapefile\\world-administrative-boundaries.shp") 

che_file <- che_file %>%
  clean_names() 

p6 <- ggplot(che_file) +
  geom_sf(color = "red", fill = "#e7d6c5", linewidth = 2) +
  geom_sf_text(label = "SUISSE\n2,900,000", family = "ps",
               size = 6, fontface = "bold") +
  coord_sf(
    xlim = c(0, 15),
    ylim = c(40, 50),
    expand = FALSE) +
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        panel.spacing = unit(0, "lines"))


# hungary

hun_file <- st_read("C:\\Users\\Ceyda\\OneDrive\\Desktop\\DataViz\\DuBois\\2026\\Week 9\\hun-shapefile\\world-administrative-boundaries.shp") 

hun_file <- hun_file %>%
  clean_names() 

p7 <- ggplot(hun_file) +
  geom_sf(color = "red", fill = "#e7d6c5", linewidth = 2) +
  geom_sf_text(label = "HONGRIE\n17,500,000", family = "ps", 
               size = 10, fontface = "bold") +
  coord_sf(
    xlim = c(16, 23),
    ylim = c(45.65, 48.75),
    expand = FALSE) +
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        panel.spacing = unit(0, "lines"))


# bayern

bayern_file <- st_read("C:\\Users\\Ceyda\\OneDrive\\Desktop\\DataViz\\DuBois\\2026\\Week 9\\bayern-shapefile\\bayern.shp") 

bayern_file <- bayern_file %>%
  clean_names() 

p8 <- ggplot(bayern_file) +
  geom_sf(color = "red", fill = "#e7d6c5", linewidth = 2) +
  geom_sf_text(label = "BAYERN\n5,800,000", family = "ps",
               size = 10, fontface = "bold") +
  coord_sf(
    xlim = c(8.75, 14.25),
    ylim = c(47.15, 50.65),
    expand = FALSE) +
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        panel.spacing = unit(0, "lines"))


# england

gbr_file <- st_read("C:\\Users\\Ceyda\\OneDrive\\Desktop\\DataViz\\DuBois\\2026\\Week 9\\gbr-shapefile\\world-administrative-boundaries.shp") 

gbr_file <- gbr_file %>%
  clean_names() 

p9 <- ggplot(gbr_file) +
  geom_sf(color = "red", fill = "#e7d6c5", linewidth = 2) +
  geom_sf_text(label = "ENGLAND\n27,500,000", family = "ps",
               size = 10, fontface = "bold") +
  coord_sf(
    xlim = c(-8.2, 2),
    ylim = c(49.7, 58.8),
    expand = FALSE) +
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        panel.spacing = unit(0, "lines"))



# The Final Plot

plots <- align_plots(p1, p2, p3, 
                     p4, p5, p6,
                     p7, p8, p9,
                     align = "hv",
                     axis = "tblr")

title <- ggdraw() +
  draw_label(
    "BLACK POPULATION OF THE UNITED STATES COMPARED WITH THE TOTAL POPULATION OF OTHER COUNTRIES.\n\n
    POPULATION NOIRS DES ETATS UNIS COMPAREE A LA POPULATION TOTALE DES AUTRES PAYS.",
    fontface = 'bold',
    fontfamily = "ps",
    hjust = 0.5,
    x = 0.5,
    y = 0.60,
    size = 33) +
  theme(
    plot.margin = margin(0, 0, -50, 0)
  )

subtitle <- ggdraw() +
  draw_label(
    "DONE BY ATLANTA UNIVERSITY.",
    fontface = 'bold',
    fontfamily = "ps",
    hjust = 0.5,
    x = 0.5,
    y = 0.72,
    size = 30) +
  theme(
    plot.margin = margin(0, 0, -20, 0)
  )


caption <- ggdraw() +
  draw_label(
    "#DuboisChallenge26 | Week 9 | Prepared by C. YAZICI",
    fontface = 'bold',
    fontfamily = "ps",
    hjust = 0.5,
    x = 0.76,
    y = 0.5,
    size = 35)

row1 <- plot_grid(
  plots[[1]], plots[[2]], plots[[3]],
  ncol = 3,
  rel_widths = c(2, 1.2, 2)
  
)

row2 <- plot_grid(
  plots[[4]], plots[[5]], plots[[6]],
  ncol = 3,
  rel_widths = c(2, 1.6, 2)
)

row3 <- plot_grid(
  plots[[7]], plots[[8]], plots[[9]],
  ncol = 3,
  rel_widths = c(2, 1.2, 2)
)



final_plot <- plot_grid(title, subtitle, row1, row2,  row3, caption,
                        labels = "", ncol = 1,
                        rel_heights = c(1.0, 1.0, 1.2, 1.2, 1.2, 0.5),
                        align = "hv") +
  theme(plot.background = element_rect(fill = "#e7d6c5", colour = NA),
        panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.margin = margin(1.5, 4, 1.5, 4, "cm"),
        panel.spacing = unit(0, "lines"))




# Save the Plot

ggsave("Week 9.png", final_plot, width = 35, height = 40, dpi = 72)



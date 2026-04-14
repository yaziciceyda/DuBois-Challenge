library(tidyverse)
library(readr)
library(showtext)
library(sf)
library(janitor)
library(patchwork)
library(cowplot)


# Font in the Plot

font_add_google('Public Sans', 'ps')
showtext_auto()


# usa

usa_file <- st_read("C:\\Users\\Ceyda\\OneDrive\\Desktop\\DataViz\\DuBois\\2026\\Week 10\\usa-shapefile\\world-administrative-boundaries.shp") 

usa_file <- usa_file %>%
  clean_names()



# 1800 
p1 <- ggplot(usa_file) +
  geom_sf(color = "red", fill = "#e7d6c5", linewidth = 1) +
  coord_sf(
    xlim = c(-130, -67),
    ylim = c(19, 56),
    expand = FALSE) +
  annotate("text", x = -98, y = 21, label = "ONE - FIFTH",
           color = "#188118", family = "ps", size = 12,
           fontface = "bold") +
  annotate("text", x = -98, y = 53, label = "1800",
           color = "black", family = "ps", size = 12,
           fontface = "bold") +
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(5, 5, 5, 5), "cm"),
        panel.spacing = unit(0, "lines"))


p2 <- ggplot(usa_file) +
  geom_sf(fill = "black") +
  coord_sf(
    xlim = c(-125, -66),
    ylim = c(25, 51),
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


p1880 <- p1 + inset_element(p2, left = 0.25, right = 0.70,
                            bottom = 0.37,  top = 0.86)  &
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        panel.spacing = unit(0, "lines"))

# 1830

p1 <- ggplot(usa_file) +
  geom_sf(color = "red", fill = "#e7d6c5", linewidth = 1) +
  coord_sf(
    xlim = c(-130, -67),
    ylim = c(19, 55),
    expand = FALSE) +
  annotate("text", x = -98, y = 21, label = "ONE - SIXTH",
           color = "#188118", family = "ps", size = 13,
           fontface = "bold") +
  annotate("text", x = -98, y = 53, label = "1830",
           color = "black", family = "ps", size = 13,
           fontface = "bold") +
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(3, 3, 3, 3), "cm"),
        panel.spacing = unit(0, "lines"))

p2 <- ggplot(usa_file) +
  geom_sf(fill = "black") +
  coord_sf(
    xlim = c(-125, -66),
    ylim = c(25, 51),
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


p1830 <- p1 + inset_element(p2, left = 0.25, right = 0.70,
                            bottom = 0.37,  top = 0.86)  &
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        panel.spacing = unit(0, "lines"))


# 1860

p1 <- ggplot(usa_file) +
  geom_sf(color = "red", fill = "#e7d6c5", linewidth = 1) +
  coord_sf(
    xlim = c(-130, -67),
    ylim = c(19, 55),
    expand = FALSE) +
  annotate("text", x = -98, y = 21, label = "ONE - SEVENTH",
           color = "#188118", family = "ps", size = 15,
           fontface = "bold") +
  annotate("text", x = -98, y = 53, label = "1860",
           color = "black", family = "ps", size = 15,
           fontface = "bold") +
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.spacing = unit(0, "lines"))

p2 <- ggplot(usa_file) +
  geom_sf(fill = "black") +
  coord_sf(
    xlim = c(-125, -66),
    ylim = c(25, 51),
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


p1860 <- p1 + inset_element(p2, left = 0.25, right = 0.70,
                            bottom = 0.37,  top = 0.86)  &
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        panel.spacing = unit(0, "lines"))


# 1890

p1 <- ggplot(usa_file) +
  geom_sf(color = "red", fill = "#e7d6c5", linewidth = 1) +
  coord_sf(
    xlim = c(-130, -67),
    ylim = c(19, 55),
    expand = FALSE) +
  annotate("text", x = -98, y = 21, label = "ONE - EIGHTH",
           color = "#188118", family = "ps", size = 17,
           fontface = "bold") +
  annotate("text", x = -98, y = 53, label = "1890",
           color = "black", family = "ps", size = 17,
           fontface = "bold") +
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        panel.spacing = unit(0, "lines"))


p2 <- ggplot(usa_file) +
  geom_sf(fill = "black") +
  coord_sf(
    xlim = c(-125, -66),
    ylim = c(25, 51),
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


p1890 <- p1 + inset_element(p2, left = 0.25, right = 0.70,
                            bottom = 0.37,  top = 0.86)  &
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

plots <- align_plots(p1880, p1830,
                     p1860, p1890,
                     align = "hv",
                     axis = "tblr")

title <- ggdraw() +
  draw_label(
    "PROPORTION OF BLACK PEOPLE IN THE TOTAL POPULATION OF THE UNITED STATES.\n
    RAPPORT DES NOIRS A LA POPULATION TOTALE DES ETATS UNIS.",
    fontface = 'bold',
    fontfamily = "ps",
    hjust = 0.5,
    x = 0.5,
    y = 0.60,
    size = 40) +
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
    y = 0.92,
    size = 39) +
  theme(
    plot.margin = margin(0, 0, 50, 0)
  )


caption <- ggdraw() +
  draw_label(
    "#DuboisChallenge26 | Week 10 | Prepared by C. YAZICI",
    fontface = 'bold',
    fontfamily = "ps",
    hjust = 0.5,
    x = 0.76,
    y = 0.5,
    size = 35)

row1 <- plot_grid(
  plots[[1]], plots[[2]], 
  ncol = 2,
  rel_widths = c(1, 5)
)

row2 <- plot_grid(
  plots[[3]], plots[[4]], 
  ncol = 2,
  rel_widths = c(1, 1.7)
)



final_plot <- plot_grid(title, subtitle, row1, row2, caption,
                        labels = "", ncol = 1,
                        rel_heights = c(0.5, 0.5, 0.5, 1.2, 0.5),
                        align = "hv") +
  theme(plot.background = element_rect(fill = "#e7d6c5", colour = NA),
        panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.margin = margin(1.5, 4, 1.0, 4, "cm"),
        panel.spacing = unit(0, "lines"))




# Save the Plot

ggsave("Week 10.png", final_plot, width = 35, height = 40, dpi = 72)




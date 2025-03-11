library(tidyverse)
library(readxl)
library(janitor)
library(showtext)
library(showtext)
library(cowplot)



font_add_google('Public Sans', 'ps')
showtext_auto()


data6 <- read_csv("data.csv") %>%
  clean_names() %>%
  mutate(x_max_owners = owners / 100,
         y_max_property =  property_value_dollars * 15/ 1308995,
         fill_city = ifelse(city == "Atlanta", "#4682b4", "#ffd700")) %>%
  arrange(year)


p1 <- ggplot() +
ggchicklet:::geom_rrect(data6,
                        mapping = aes(xmin = -x_max_owners, xmax = x_max_owners, 
                            ymin = -x_max_owners, ymax = x_max_owners, 
                        fill = fill_city), 
                        radius = unit(0.5, units = "cm")) +
  geom_text(data6 %>%
              filter(year != 1880),
            mapping = aes(x = 0, y = -1 * x_max_owners + 1,
                      label = scales::comma(owners)), hjust = 0.5,
            colour = "black",
            size = 7, family = "ps") +
  # Atlanta
  geom_curve(data6 %>%
               filter(year == 1880, 
                      city == "Atlanta"), 
             mapping = aes(x = 5, y = 5, 
                           xend = 12, yend = 12),
             arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
             color = "#4682b4",
             linewidth = 1.2,
             curvature = -0.3) +
  geom_text(data6 %>%
              filter(year == 1880, 
                     city == "Atlanta"), 
            mapping = aes(x = 13, y = 12,
                          label = 639),
            family = "ps", hjust = 0, size = 8) +
  # Savannah
  geom_curve(data6 %>%
               filter(year == 1880, 
                      city == "Savannah"), 
             mapping = aes(x = 7, y = 2, 
                           xend = 13, yend = 7),
             arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
             color = "#ffd700",
             linewidth = 1.2,
             curvature = -0.2) +
  geom_text(data6 %>%
              filter(year == 1880, 
                     city == "Savannah"), 
            mapping = aes(x = 14, y = 6.5,
                          label = 699),
            family = "ps", hjust = 0, size = 8) +
    facet_grid(~year, switch = "both") +
  scale_fill_identity() +
  coord_fixed() +
  labs(title = "OWNERS") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA), 
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(family = "ps", size = 25, 
                                   hjust = 0.5),
        strip.background = element_rect("#e7d6c5"),
        strip.text = element_text(family = "ps", size = 18)) 



p2 <- ggplot() +
  ggchicklet:::geom_rrect(data6 %>%
                            arrange(year, desc(y_max_property)),
                          mapping = aes(xmin = -y_max_property, 
                                        xmax = y_max_property, 
                                        ymin = -y_max_property, 
                                        ymax = y_max_property, 
                                        fill = fill_city), 
                          radius = unit(0.5, units = "cm")) +
  geom_text(data6 %>%
              filter(year != 1880),
            mapping = aes(x = 0, y = -1 * y_max_property + 1,
                  label = paste0("$ ", scales::comma(property_value_dollars))), 
            hjust = 0.5,
            colour = "black",
            size = 7, family = "ps") +
  # Savannah
  geom_curve(data6 %>%
               filter(year == 1880, 
                      city == "Savannah"), 
             mapping = aes(x = 1, y = 2, 
                           xend = 6, yend = 3),
             arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
             color = "#ffd700",
             linewidth = 1.2,
             curvature = -0.2) +
  geom_text(data6 %>%
              filter(year == 1880, 
                     city == "Savannah"), 
            mapping = aes(x = 7, y = 3,
                          label = "$ 194,392"),
            family = "ps", hjust = 0, size = 7) +
  # Atlanta
  geom_curve(data6 %>%
               filter(year == 1880, 
                      city == "Atlanta"), 
             mapping = aes(x = 0, y = 2.5, 
                           xend = 5, yend = 7),
             arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
             color = "#4682b4",
             linewidth = 1.2,
             curvature = -0.2) +
  geom_text(data6 %>%
              filter(year == 1880, 
                     city == "Atlanta"), 
            mapping = aes(x = 6, y = 7,
                          label = "$ 227,955"),
            family = "ps", hjust = 0, size = 7) +
  
  facet_grid(~year, switch = "both") +
  scale_fill_identity() +
  coord_fixed() +
  labs(title = "PROPERTY") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA), 
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(family = "ps", size = 25, hjust = 0.5),
        strip.background = element_rect("#e7d6c5"),
        strip.text = element_text(family = "ps", size = 18))

# The Legend

p3 <- ggplot(data6) +
# SAVANNAH
annotate("rect", xmin = 10.25, xmax = 10.35,
         ymin = -2.35, ymax = -2.15, fill = "#ffd700", colour = "black") +
  annotate("text", x = 9.95, y = -2.25, label = "SAVANNAH",
           hjust = 0, colour = "black", size = 7, family = "ps") +
  # The Legend for the cities
  # ATLANTA
  annotate("rect", xmin = 11.20, xmax = 11.3,
           ymin = -2.35, ymax = -2.15, fill = "#4682b4", colour = "black") +
  annotate("text", x = 10.95, y = -2.25, label = "ATLANTA",
           hjust = 0, colour = "black", size = 7, family = "ps") +
  coord_cartesian() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA), 
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = margin(1.0, 10.0, 1.0, 10, "cm"))


# The Final Plot

plots <- align_plots(p3, p1, p2, align = "v")

title1 <- ggdraw() +
  draw_label(
    "BLACK GEORGIANS' PROPERTY IN TWO CITIES\n\n OF GEORGIA.",
    fontface = 'bold',
    fontfamily = "ps",
    hjust = 0.5,
    x = 0.5,
    size = 40)



caption <- ggdraw() +
  draw_label(
    "#DuboisChallenge2025 | Week 6 | Prepared by C. YAZICI",
    fontface = 'bold',
    fontfamily = "ps",
    hjust = 0,
    x = 0.5,
    y = 0.5,
    size = 18)

top_row <-  plot_grid(
  plots[[1]], plots[[2]], plots[[3]],
  labels = "",
  rel_heights = c(0.2, 1.0, 1.0),
  nrow = 3
)


final_plot <- plot_grid(title1,  top_row, caption,
                        labels = "", ncol = 1,
                        rel_heights = c(0.1, 1.0, 0.1),
                        align = "hv") +
  theme( panel.background = element_rect(fill = "#e7d6c5", color = NA),
         plot.background = element_rect(fill = "#e7d6c5", color = NA), 
        plot.margin = margin(1.0, 1.0, 0.5, 0.5, "cm"))
    
# final_plot

# Save the Plot

ggsave("Week 6_version2.png", final_plot, width = 20, height = 25, dpi = 72)


library(tidyverse)
library(janitor)
library(readr)
library(showtext)
library(scales)
library(camcorder)



# Font in the Plot

font_add_google('Public Sans', 'ps')
showtext_auto()


data4 <- read.csv("data.csv") %>%
  clean_names() %>%
  arrange(year)


gg_record(
  dir = file.path("week 4", "recording"),
  device = "png",
  width = 7,
  height = 7,
  units = "in",
  dpi = 300
)


ggplot(data4) +
  geom_line(data = data4 %>% 
              filter(year <= 1874), 
            aes(x = year, y = property_valuation - 30000)) +
  geom_line(data = data4 %>% 
              filter(year <= 1874), 
            aes(x = year, y = property_valuation + 30000)) +
  geom_line(data = data4 %>% 
              filter(year >= 1874 & year <= 1899),
            aes(x = year, y = property_valuation), linewidth = 4) +
  geom_line(data = data4 %>% 
              filter(year >= 1899), 
            aes(x = year, y = property_valuation - 30000)) +
  geom_line(data = data4 %>% 
              filter(year >= 1899), 
            aes(x = year, y = property_valuation + 30000)) +
  coord_cartesian(expand = FALSE) +
  scale_y_continuous(limits = c(0, 5000000), 
                     breaks = c(seq(1000000, 4000000, 1000000)),
                     minor_breaks = c(seq(0, 5000000, 100000)), 
                     labels = comma) +
  scale_x_continuous(limits = c(1860, 1901), 
                     breaks = c(seq(1870, 1900, 5)), 
                     minor_breaks = c(seq(1870, 1900, 1))) +
  labs(x = "",
       y = "",
       title = "VALUATION OF TOWN AND CITY PROPERTY OWNED\nBY BLACK GEORGIANS.",
       caption = "#DuboisChallenge2025 | Week 4 | Prepared by C. YAZICI") +
  theme(
    panel.background = element_rect(fill = "#e7d6c5", color = NA),
    plot.background = element_rect(fill = "#e7d6c5", color = NA),
    axis.ticks = element_blank(),
    panel.grid.minor = element_line(colour = "#fbaaa4"),
    panel.grid.major = element_line(colour = "#fbaaa4"),
    axis.text.x = element_text(family = "ps", size = 32),
    axis.text.y = element_text(family = "ps", size = 35, hjust = 6),
    plot.title = element_text(family = "ps", hjust = 0.5, size = 42,
                              lineheight = 0.4, face = "bold",
                              margin = margin(0, 0, 6, 0)),
    plot.caption = element_text(family = "ps", hjust = 1, size = 35),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) +
  annotate("text", x = 1880, y = 4100000, 
           label = "RISE OF\n        THE NEW\n             INDUSTRIALISM", 
           family = "ps", hjust = 0, size = 8, lineheight = 0.4) +
  annotate("text", x = 1875, y = 2200000, 
           label = "POLITICAL\n     UNREST", 
           family = "ps", hjust = 0, size = 8, lineheight = 0.4) +
  annotate("text", x = 1872, y = 400000, 
           label = "KU-KLUXISM", 
           family = "ps", hjust = 0, size = 8, lineheight = 0.4,
           angle = 90) +
  annotate("text", x = 1898, y = 2300000, 
           label = "DISFRANCHSMENT\nAND\nPROSCRIPTIVE\nLAWS.", 
           family = "ps", hjust = 0.5, size = 8, lineheight = 0.4) +
  annotate("text", x = 1890.5, y = 1600000, 
           label = "LYNCHING", 
           family = "ps", hjust = 0, size = 8, lineheight = 0.4) +
  annotate("text", x = 1894, y = 300000, 
           label = "FINANCIAL PANIC", 
           family = "ps", hjust = 0, size = 8, lineheight = 0.4,
           angle = 90) +
  annotate("text", x = 1861, y = 400000, 
           label = "$", 
           family = "ps", hjust = 0, size = 12) +
  annotate("text", x = 1861, y = 600000, 
           label = "$", 
           family = "ps", hjust = 0, size = 12) +
  annotate("text", x = 1861, y = 1400000, 
           label = "$", 
           family = "ps", hjust = 0, size = 12) +
  annotate("text", x = 1861, y = 1600000, 
           label = "$", 
           family = "ps", hjust = 0, size = 12) +
  annotate("text", x = 1861, y = 2400000, 
           label = "$", 
           family = "ps", hjust = 0, size = 12) +
  annotate("text", x = 1861, y = 2600000, 
           label = "$", 
           family = "ps", hjust = 0, size = 12) +
  annotate("text", x = 1861, y = 3400000, 
           label = "$", 
           family = "ps", hjust = 0, size = 12) +
  annotate("text", x = 1861, y = 3600000, 
           label = "$", 
           family = "ps", hjust = 0, size = 12) +
  annotate("text", x = 1861, y = 4400000, 
           label = "$", 
           family = "ps", hjust = 0, size = 12) +
  annotate("text", x = 1861, y = 4600000, 
           label = "$", 
           family = "ps", hjust = 0, size = 12) +
  annotate("text", x = 1862.5, y = 4850000, 
           label = "DOLLARS", 
           family = "ps", hjust = 0.5, size = 12) +
  annotate("rect", xmin = 1865, xmax = 1870,
           ymin = 0, ymax = 5000000, fill = "#e7d6c5",
           color = "black") +
  annotate("segment", x = 1865, xend = 1870,
           y = 1000000, yend = 1000000, 
           color = "#fbaaa4") +
  annotate("segment", x = 1865, xend = 1870,
           y = 2000000, yend = 2000000, 
           color = "#fbaaa4") +
  annotate("segment", x = 1865, xend = 1870,
           y = 3000000, yend = 3000000, 
           color = "#fbaaa4") +
  annotate("segment", x = 1865, xend = 1870,
           y = 4000000, yend = 4000000, 
           color = "#fbaaa4") +
  annotate("segment", x = 1865, xend = 1870,
           y = 0, yend = 0, 
           color = "#e7d6c5") +
  annotate("segment", x = 1865, xend = 1870,
           y = 5000000, yend = 5000000, 
           color = "#e7d6c5") 

  
                     
record_polaroid()


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path(paste0("Week 4", ".png")),
  height = 11,
  width = 9,
  bg = "#e7d6c5",
  units = "in",
  dpi = 300
)


library(tidyverse)
library(readr)
library(janitor)
library(showtext)
library(patchwork)

# Font in the Plot

font_add_google('Public Sans', 'ps')
showtext_auto()

# Data Import

data7 <- read.csv("data.csv", header = FALSE) %>%
  rename("country" = V1,
         "percent" = V2) %>%
  mutate(country = ifelse(country == "Negroes, U.S.A.",
                          "Black People, U.S.A", country)) %>%
  arrange(desc(percent))

# The Bar Plot

p1 <- ggplot(data7) +
  geom_col(aes(x = percent, y = reorder(country, percent),
               fill = factor(ifelse(country == "Black People, U.S.A",
                                    "#dc143c",
                                    "#238123")))) +
  scale_fill_identity() +
  coord_cartesian(clip ="off") +
  labs(caption = "#DuboisChallenge24 | Week 7 | Prepared by C. YAZICI") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(hjust = 0, family = "ps", 
                                   size = 18, color = "black"),
        plot.caption = element_text(family = "ps", hjust = 1, 
                                    size = 19),
        plot.margin = margin(0.8, 2.7, 0.8, 2.7, "cm"))

# The Data of Text and Line Segments 

data_text <- tibble(x = c(1, 1.3, 1.3),
                    y = c(3, 2.7, 2.5),
                    label = c("Illiteracy of the American Black People\ncompared with that of other nations.",
                              "Proportion d' illettres parmi les Noirs Americains\ncomparee a celle des autres nations.",
                              "Done by Atlanta University"),
                    size = c(12, 7, 5))

# The Plot of Text and Line Segments 

p2 <- ggplot(data_text) +
  geom_text(aes(x = x, y = y, label = label, size = size),
            family = "ps") +
  scale_size_identity() +
  geom_segment(aes(x = -0.08, xend = 2.45,
                   y = 2.85, yend = 2.85), color = "grey50", 
               linewidth = 1.5)+
  geom_segment(aes(x = -0.08, xend = 2.45,
                   y = 2.55, yend = 2.55), color = "grey70", 
               linewidth = 1.5)+
  coord_cartesian(xlim = c(-6, 9),
                  ylim = c(2.5, 3.2)) +
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.margin = margin(0.5, 0, 0.5, 0, "cm"))

# Te Final Plot

final_plot <- p2 + p1 + plot_layout(nrow = 2, 
                                     heights = c(0.7, 2))  &
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.margin = margin(0.5, 0.8, 0.5, 0.8, "cm"))

# Save the Plot

ggsave("Week7.png", final_plot, width = 18, height = 20, dpi = 72)


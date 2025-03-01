library(tidyverse)
library(readxl)
library(janitor)
library(showtext)
library(ggforce)
library(ggnewscale)


font_add_google('Public Sans', 'ps')
showtext_auto()


data5 <- read_csv("data.csv") %>%
  clean_names() %>%
  arrange(desc(year)) %>%
  distinct(year, valuation_dollars) %>%
  mutate(r = round(valuation_dollars * 20 / 13447423))

# arc_bar

# 1880, 1885, 1890, 1895, 1899
arcbar <- tibble(x0 = c(-2, 2, 4, 3, -2), 
                 y0 = c(-2, -2, 1, 3, 3), 
                 r0 = c(0, 0, 0, 0, 0),
                 r = c(6.2, 9, 14, 14.8, 16.5),
                 start = c(-9, 9.1, 8.1, 7.3, -7),
                 end = c(-8.5, 8.7, 7.7, 6.9, -7.5),
                 fill_arc = c("#ad8761", "#4682b4", "#ffd700",
                                "#d2b48c", "#dc143c")) 

arc_85 <- tibble(x0 = 2, y0 = -2, r0 = 0, r = 6, start = 9.2, end = 8.6)

  
p <- ggplot() +
  geom_circle(data5, mapping = aes(x0 = 0, y0 = 0, r = r, fill = as.factor(1:6),
                                   colour = as.factor(1:6))) + 
  scale_fill_manual(values = c("#dc143c", "#d2b48c", "#ffd700", "#4682b4", 
                               "#ad8761", "black")) +
  scale_colour_manual(values = c("#dc143c", "#d2b48c", "#ffd700", "#4682b4", 
                                 "#ad8761", "black")) +
  new_scale_fill() + 
  new_scale_colour() + 
  geom_arc_bar(arc_85, mapping = aes(x0 = x0, y0 = y0, r0 = r0,
                                     r = r, start = start, end = end), 
                                     fill = "#ad8761", colour = "#ad8761") +
  new_scale_fill() + 
  new_scale_colour() + 
  # 1890
  geom_arc_bar(aes(x0 = 4, y0 = 1, r0 = 0, r = 5, start = 8.35, end = 7.45),
               fill = "#ad8761", colour = "#ad8761") + 
  geom_arc_bar(aes(x0 = 4, y0 = 1, r0 = 0, r = 5.5, start = 8.2, end = 7.6),
               fill = "#4682b4", colour = "#4682b4") +
  # 1895
  geom_arc_bar(aes(x0 = 3, y0 = 3, r0 = 0, r = 5, start = 6.65, end = 7.6),
               fill = "#ad8761", colour = "#ad8761") +
  geom_arc_bar(aes(x0 = 3, y0 = 3, r0 = 0, r = 5.5, start = 6.7, end = 7.55),
               fill = "#4682b4", colour = "#4682b4") +
  geom_arc_bar(aes(x0 = 3, y0 = 3, r0 = 0, r = 9, start = 6.85, end = 7.35),
               fill = "#ffd700", colour = "#ffd700") +
  
  geom_arc_bar(aes(x0 = 3, y0 = 3, r0 = 0, r = 14.8, start = 7.25, end = 7.1),
               fill = "#d2b48c", colour = "#d2b48c") +
  # 1899
  geom_arc_bar(aes(x0 = -2, y0 = 3, r0 = 0, r = 5.5, start = -6.7, end = -7.7),
               fill = "#ad8761", colour = "#ad8761") +
  geom_arc_bar(aes(x0 = -2, y0 = 3, r0 = 0, r = 8, start = -6.8, end = -7.6),
               fill = "#4682b4", colour = "#4682b4") +
  geom_arc_bar(aes(x0 = -2, y0 = 3, r0 = 0, r = 11, start = -6.95, end = -7.55),
               fill = "#ffd700", colour = "#ffd700") +
  geom_arc_bar(aes(x0 = -2, y0 = 3, r0 = 0, r = 15, start = -7, end = -7.4),
               fill = "#d2b48c", colour = "#d2b48c") +
  scale_fill_identity() +
  scale_colour_identity() +
  new_scale_fill() + 
  new_scale_colour() + 
  geom_arc_bar(arcbar, mapping = aes(x0 = x0, y0 = y0, r0 = r0,
                                     r = r, start = start, end = end, 
                                     fill = fill_arc, colour = fill_arc)) +
  scale_fill_identity() +
  scale_colour_identity() +
  annotate("text", x = -2, y = 0, label = "$ 5,393,885", family = "ps",
           colour = "ivory", hjust = 0, size = 8) +
  annotate("text", x = -6, y = -6.8, label = "$ 5,764,293", family = "ps",
           colour = "black", hjust = 0, angle = 50, size = 8) +
  annotate("text", x = 3, y = -3.7, label = "$ 8,153,390", family = "ps",
            colour = "ivory", hjust = 0, angle = -58, size = 8) +
  annotate("text", x = 10, y = 1, label = "$ 12,322,003", family = "ps",
           colour = "black", hjust = 0, size = 8) +
  annotate("text", x = 10, y = 9.5, label = "$ 12,941,230", family = "ps",
           colour = "black", hjust = 0, angle = 45, size = 8) +
  annotate("text", x = -15, y = 12, label = "$ 13,447,423", family = "ps",
           colour = "black", hjust = 0, angle = -40, size = 8) +
  
  annotate("text", x = 0, y = -8.4, label = "1880", family = "ps",
           colour = "black", hjust = 0.5, vjust = 1, size = 8) +
  annotate("text", x = 0, y = -7, label = "1875", family = "ps",
           colour = "ivory", hjust = 0.5, vjust = 1, size = 8) +
  annotate("text", x = 0, y = -11, label = "1885", family = "ps",
           colour = "ivory", hjust = 0.5, vjust = 1, size = 8) +
  annotate("text", x = 0, y = -17, label = "1890", family = "ps",
           colour = "black", hjust = 0.5, vjust = 1, size = 8) +
  annotate("text", x = 0, y = -18.3, label = "1895", family = "ps",
           colour = "black", hjust = 0.5, vjust = 1, size = 8) +
  annotate("text", x = 0, y = -19.4, label = "1899", family = "ps",
           colour = "black", hjust = 0.5, vjust = 1, size = 8) +
  coord_fixed() +
  labs(title = "ASSESSED VALUATION OF ALL TAXABLE PROPERTY\nOWNED BY BLACK GEORGIANS.",
       caption = "#DuboisChallenge2025 | Week 5 | Prepared by C. YAZICI") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA), 
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(family = "ps", hjust = 0.5, size = 35,
                                  lineheight = 1.2, face = "bold"),
        plot.caption = element_text(family = "ps", hjust = 0.9, size = 25),
        plot.margin = unit(c(0.1, 0.5, 0.1, 0.5), "cm"))


ggsave("Week 5.png", p, width = 25, height = 28, dpi = 72)



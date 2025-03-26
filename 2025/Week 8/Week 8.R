
library(tidyverse)
library(readxl)
library(janitor)
library(showtext)
library(scales)
library(ggbrace)


font_add_google('Public Sans', 'ps')
showtext_auto()


data8 <- read_csv("data8.csv") %>%
  clean_names() %>%
  mutate(x_new = count / 63012 * 100) %>%
  arrange(desc(count)) %>%
  mutate(x_new = ifelse(occupation == "AGRICULTURAL LABORERS",
                        100, x_new),
         y = rev(row_number()),
         occupation_text = case_when(
           occupation == "AGRICULTURAL LABORERS" ~ "AGRICULTURAL\n LABORERS",
           occupation == "FARMERS AND PLANTERS" ~ "FARMERS AND\n PLANTERS",
           occupation == "STEAM RAILWAY EMPLOYEES" ~ "STEAM RAILWAY\n EMPLOYEES",
           occupation == "CARPENTERS AND JOINERS" ~ "CARPENTERS AND\n    JOINERS",
           occupation == "SAW AND PLANING MILL EMMPLOYEES" ~ "SAW AND PLANING\nMILL EMMPLOYEES",
           occupation == "BLACKSMITHS AND WHEELWRIGHTS" ~ "BLACKSMITHS\n   AND\n   WHEELWRIGHTS",
           occupation == "BRICK-MAKERS AND POTTERS" ~ "BRICK-MAKERS AND\n    POTTERS",
           occupation == "PAINTERS,GLAZERS AND VARNISHERS" ~ "PAINTERS,GLAZERS\nAND VARNISHERS",
           occupation == "BOOT AND SHOE MAKERS" ~ "BOOT AND SHOE\n  MAKERS",
           occupation == "PROFESSORS AND TEACHERS" ~ "PROFESSORS AND\n  TEACHERS",
           occupation == "LIVERY STABLE KEEPERS" ~ "LIVERY STABLE\n  KEEPERS",
           occupation == "GARDNERS AND FLORISTS" ~ "GARDNERS AND\n FLORISTS",
           .default = occupation
         ))

data_agg <- tibble(x1 = 100, x2 = 100,
                   y1 = 22, y2 = 23)

ggplot() +
  geom_curve(aes(x = x1, y = y1, xend = x2, yend = y2, colour = "curve"), 
             curvature = 1.2, data = data_agg) +
  annotate("segment", x = 0, xend = 100,
           y = 22, yend = 22) +
  annotate("segment", x = 0, xend = 100,
           y = 23, yend = 23) +
  coord_cartesian(xlim = c(0, 125),
                  ylim = c(22, 24))


# sum(data8$count[8:22]) --> 16254
# 16254 / 63012 * 100 = 25.79509

p <- ggplot() +
  geom_segment(data8 %>%
                 filter(occupation != "AGRICULTURAL LABORERS"),
               mapping = aes(x = 0, xend = x_new, 
                             y = reorder(occupation, count), 
                             yend = reorder(occupation, count)),
               linewidth = 10, colour = "black") +
  geom_segment(data8 %>%
  filter(occupation != "AGRICULTURAL LABORERS"),
         mapping = aes(x = 0, xend = x_new, 
                   y = reorder(occupation, count), 
                               yend = reorder(occupation, count)),
               linewidth = 9.5, colour = "#dc143c") +
  geom_curve(data_agg, mapping = aes(x = x1, y = y1, xend = x2, yend = y2), 
             colour = "black", 
             curvature = 1.2, linewidth = 10) +
  geom_curve(data_agg, mapping = aes(x = x1, y = y1, xend = x2, yend = y2), 
                                     colour = "#dc143c", 
             curvature = 1.2, linewidth = 9.5) +
  annotate("segment", x = 100, xend = 56,
           y = 22, yend = 22, colour = "black", linewidth = 10) +
  annotate("segment", x = 100, xend = 56,
           y = 22, yend = 22, colour = "#dc143c", linewidth = 9.5) +
  annotate("segment", x = 0, xend = 100,
           y = 23, yend = 23, colour = "black", linewidth = 10) +
  annotate("segment", x = 0, xend = 100,
           y = 23, yend = 23, colour = "#dc143c", linewidth = 9.5) +
  geom_text(data8 %>%
              filter(occupation != "AGRICULTURAL LABORERS"),
             mapping = aes(x = -6, y = y, label = scales::comma(count)),
            family = "ps", size = 9) +
  geom_text(data8 %>%
              filter(occupation != "AGRICULTURAL LABORERS"),
              mapping = aes(x = -25, y = y, label = occupation_text),
            family = "ps", size = 8) +
  annotate("text", x = -25, y = 23, label = "AGRICULTURAL\n LABORERS",
           family = "ps", size = 8) +
  
  
  annotate("text", x = -6, y = 23, label = "98,400",
           family = "ps", size = 9) +
  annotate("segment", x = 12, xend = 36,
           y = 8, yend = 8, colour = "black", linewidth = 10) +
  annotate("segment", x = 12, xend = 36,
           y = 8, yend = 8, colour = "#dc143c", linewidth = 9.5) +
  
  stat_brace(aes(x = c(-1, 6.5), y = c(0.5, 15.5)),  rotate = 90,
             width = 4, bending = 1) +
  
  annotate("text", x = 41, y = 11, label = "1890.",
           family = "ps", size = 16, fontface = "bold") +

  coord_cartesian(xlim = c(-33, 105),
                  ylim = c(0, 25)) +
  scale_y_discrete(labels = function(x)sub(" ", "-\n", x, fixed = TRUE)) +
  labs(x = "",
       title = "OCCUPATIONS OF BLACK PEOPLE IN GEORGIA.",
       subtitle = "\nMALES OVER 10.",
       caption = "#DuboisChallenge2025 | Week 8 | Prepared by C. YAZICI") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA), 
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
       # axis.text.y = element_text(family = "ps", size = 21, hjust = 0),
        plot.title = element_text(family = "ps", hjust = 0.5, size = 40,
                                 face = "bold"),
        plot.subtitle = element_text(family = "ps", hjust = 0.5, size = 25),
        plot.caption = element_text(family = "ps", hjust = 0.9, size = 25),
        plot.margin = unit(c(1.8, 0.5, 1.8, 0.5), "cm"))


ggsave("Week 8.png", p, width = 27, height = 32, dpi = 72)



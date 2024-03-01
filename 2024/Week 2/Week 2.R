library(tidyverse)
library(readr)
library(janitor)
library(showtext)


# Font in the Plot

font_add_google('Public Sans', 'ps')
showtext_auto()

# Data Import

data2 <- read.csv("data.csv") %>%
  clean_names()

# Data Wrangling

data2_v2 <- data2 %>%
  add_row(year = 1862, slave = 0, free = 3) %>%
  arrange(year)
  
# The Plot
p <- ggplot(data2_v2 %>% 
         filter(year < 1870)) +
  geom_area(aes(year, free), fill = "#CB2A44") +
  geom_ribbon(data2_v2 %>% 
              filter(year == c(1862, 1870)), 
  mapping = aes(x = year, ymin = 0, ymax = 3), fill = "#CB2A44") +
  geom_ribbon(aes(x = year, ymin = free, ymax = 3), fill = "#141414") +
  geom_line(aes(x = year, y = free), colour = "#e7d6c5", linewidth = 0.8) +
  geom_vline(data2_v2 %>%
               filter(year != 1862), mapping = aes(xintercept = year), 
               colour = "#e7d6c5") +
  scale_y_reverse(breaks = c(3, 2, 1), labels = c("3%", "2%", "1%"), 
                  position = "right") +
  scale_x_reverse(limits = c(1870, 1790),
                  breaks = seq(1790, 1870, by = 10)) +
  geom_text(data2 %>% 
              filter(year > 1790 & year < 1870),
              mapping = aes(x = year, y = -0.2, label = free),
            family = "ps", size = 7, colour = "grey30") +
  annotate("text", x = 1870, y = -0.2, label = "100%",
           family = "ps", size = 7, colour = "grey30") +
  annotate("text", x = 1790, y = -0.2, label = "1.3%",
           family = "ps", size = 7, colour = "grey30") +
  coord_flip() +
  labs(title = "SLAVES AND FREE NEGROES.",
       subtitle = "PERCENT\n\nOF   \nFREE NEGROES",
       caption = "#DuboisChallenge24| Week 2 | Prepared by C. YAZICI") +
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA), 
        panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.length.x = unit(0.5, "cm"),
        axis.title = element_blank(),
        axis.text.y = element_text(hjust = 1, family = "ps", size = 16), 
        axis.text.x = element_text(vjust = -1, family = "ps", size = 16),
        plot.title = element_text(family = "ps", hjust = 0.5, size = 25,
                                 face  = "bold"),
        plot.subtitle = element_text(family = "ps", hjust = 1, size = 15,
                                     vjust = 0, colour = "grey30"),
        plot.caption = element_text(family = "ps", hjust = 1, size = 18),
        plot.margin = margin(0.9, 3, 0.8, 3, "cm")) 

# Save the Plot

ggsave("Week2.png", p, width = 16, height = 18, dpi = 72)




 
                  




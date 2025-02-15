library(tidyverse)
library(janitor)
library(readr)
library(showtext)
library(scales)


# Font in the Plot

font_add_google('Public Sans', 'ps')
showtext_auto()

# Data Import & Preprocessing

data3 <- read.csv("data.csv", header = TRUE) %>%
  clean_names() %>%
  arrange(date) %>%
  mutate(date = as.factor(date))

# The Plot

p <- ggplot(data3, aes(x = fct_rev(fct_infreq(date)), y = land)) +
  geom_bar(stat = "identity", fill = "#dc143c", width = 0.7) +
  geom_text(data3 %>% 
              filter(date %in% c(1874, 1899)),
            mapping = aes(x = date, y = land, 
                          label = scales::comma(land), fontface = "bold"),
            position = position_stack(vjust = 0.5), family = "ps", size = 8) +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(caption = "#DuboisChallenge2025| Week 2 | Prepared by C. YAZICI",
       title = "ACRES OF LAND OWNED BY BLACK PEOPLE\nIN GEORGIA.") +
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA), 
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "ps", size = 16, hjust = 0),
        plot.caption = element_text(family = "ps", hjust = 1, size = 20),
        plot.title = element_text(family = "ps", hjust = 0.5, size = 28),
        plot.margin = margin(1.0, 3.7, 1.0, 3.7, "cm"))

# Save the Plot

ggsave("Week2.png", p, width = 15, height = 18, dpi = 72)

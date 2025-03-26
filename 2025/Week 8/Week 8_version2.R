library(ggforce)
library(tidyverse)
library(readxl)
library(janitor)
library(showtext)
library(scales)
install.packages("treemapify")
library(treemapify)
library(paletteer)


font_add_google('Public Sans', 'ps')
showtext_auto()


data8 <- read_csv("data.csv") %>%
  clean_names() %>%
  mutate(x_new = count / 63012 * 100) %>%
  arrange(desc(count)) %>%
  mutate(occupation = str_replace_all(occupation, " ", "\n")) 



p <- ggplot(data8, aes(area = count, fill = count, 
                  label = paste0(occupation, "\n", 
                                 scales::comma(count)))) +
  geom_treemap() +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 5,
                    grow = TRUE,
                    family = "ps") +
  scale_fill_gradient(
    low = "#FFEBEEFF",
    high = "#B71C1CFF"
  ) +
  labs(title = "OCCUPATIONS OF BLACK PEOPLE IN GEORGIA.",
       subtitle = "\nMALES OVER 10.",
       caption = "#DuboisChallenge2025 | Week 8 | Prepared by C. YAZICI") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA), 
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),axis.ticks = element_blank(),
        plot.title = element_text(family = "ps", hjust = 0.5, size = 70,
                                  face = "bold"),
        plot.subtitle = element_text(family = "ps", hjust = 0.5, size = 25),
        plot.caption = element_text(family = "ps", hjust = 0.9, size = 25),
        plot.margin = unit(c(0.8, 0.8, 0.8, 0.8), "cm"))



ggsave("Week 8_treemap.png", p, width = 33, height = 22, dpi = 72)

  
  

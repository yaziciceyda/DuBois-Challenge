library(tidyverse)
library(readxl)
library(janitor)
library(showtext)
library(waffle)
library(paletteer)

font_add_google('Public Sans', 'ps')
showtext_auto()


data9 <- read_csv("data.csv") %>%
  clean_names() %>%
  mutate(group = ifelse(group == "Negroes", "Black People", group))
         


p <- ggplot() +
  geom_waffle(data9,  mapping = aes(fill = occupation, values = percentage),
              n_rows = 10, size = 0.73, colour = "white",
              radius = grid::unit(2.8, "cm")) +
  facet_grid(~group) +
  coord_equal() +
  scale_fill_manual(values=c("Agriculture, Fisheries and Mining" = "#dc143c",
                                 "Manufacturing and Mechanical Industries" = "#4682b4", 
                                 "Domestic and Personal Service" = "#ffd700", 
                                 "Professions" = "#a26e3a", 
                                 "Trade and Transportation" = "#d2b48c")) +
  labs(fill = "",
       title = "OCCUPATIONS OF BLACK PEOPLE AND WHITES IN GEORGIA",
       caption = "#DuboisChallenge2025 | Week 9 | Prepared by C. YAZICI"
       ) +
  theme(legend.position = "top",
        panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA), 
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.direction = "horizontal",
        legend.key.width = unit(1.2, 'cm'),
        legend.key.height = unit(1.5, 'cm'),
        legend.text = element_text(family = "ps", size = 23),
        legend.background = element_rect(fill = "#e7d6c5", color = NA),
        strip.text = element_text(family = "ps", size = 30),
        strip.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.title = element_text(family = "ps", size = 45,
                                  margin = margin(b = 80, unit = "pt")),
        plot.title.position = "plot",
        plot.caption = element_text(family = "ps", size = 30, hjust = 0.9,
                                    margin = margin(t = 80, unit = "pt")),
        plot.margin = unit(c(0.5, 0.9, 0.5, 0.9), "cm")
        ) +
  guides(fill = guide_legend(label.position = "top", title.hjust = 0.5,
                             nrow = 3)) 


ggsave("Week 9_version2.png", p, width = 27, height = 25, dpi = 72)


                    
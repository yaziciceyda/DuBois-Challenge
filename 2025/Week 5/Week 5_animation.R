library(tidyverse)
library(readxl)
library(janitor)
library(showtext)
library(ggtext)
library(gganimate)
library(scales)


font_add_google('Public Sans', 'ps')
showtext_auto()

font_add('fa-solid', 'Font Awesome 6 Free-Solid-900.otf')



data5 <- read_csv("data.csv") %>%
  clean_names() %>%
  arrange(desc(year)) %>%
  distinct(year, valuation_dollars) %>%
  mutate(label_text = "<span style='font-family:fa-solid'>&#xf81d;</span>",
         size_icon = 8 * valuation_dollars / 13447423)


p <- ggplot(data5) +
  geom_line(aes(x = year, y = valuation_dollars), color = "black",
            linewidth = 1.5) +
  geom_richtext(aes(x = year, y = valuation_dollars, label = label_text,
                    colour = valuation_dollars),
                    label.colour = NA, fill = NA,
                    hjust = 0, 
                    size = 8,
                    family = 'fontawesome-webfont') +
  scale_colour_gradient(
    low = "#31c831",
    high = "#1b621b",
    na.value = "grey50") +
  geom_richtext(aes(x = year, y = valuation_dollars, label = label_text,
                    colour = valuation_dollars, 
                    group = year),
                    label.colour = NA, fill = NA,
                    hjust = 0, 
                    size = 8,
                    family = 'fontawesome-webfont') +
  transition_reveal(along = year) + 
  view_follow(fixed_x = TRUE, fixed_y = TRUE) +
  coord_cartesian(clip = 'off') +
  scale_y_continuous(labels = scales::dollar_format(scale = .000001,
                                                    suffix = "M")) +
  labs(title = "ASSESSED VALUATION OF ALL TAXABLE PROPERTY\nOWNED BY BLACK GEORGIANS.",
       caption = "#DuboisChallenge2025 | Week 5 | Prepared by C. YAZICI") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA), 
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "ps", size = 12),
        plot.title = element_text(family = "ps", hjust = 0.5,
                                  lineheight = 1.2, face = "bold", size = 15),
        plot.caption = element_text(family = "ps", hjust = 0.9, vjust = 0,
                                    size = 13,
                                    margin = unit(c(1, 0, 0.5, 0), "cm")),
        plot.margin = unit(c(1.4, 1.2, 1.2, 0.9), "cm")) 


animate(p, duration = 20, fps = 20, width = 600, height = 600,
        renderer = gifski_renderer())

anim_save("Week 5_animation.gif") 



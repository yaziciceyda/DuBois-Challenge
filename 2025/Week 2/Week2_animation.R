library(gganimate)
library(tibble)
library(tidyverse)
library(janitor)
library(scales)

data2 <- read.csv("data.csv", header = TRUE) %>%
  clean_names() %>%
  mutate(land_text = as.character(land),
         land_segment = land) %>%
  add_row(date = 1881, land_text = "NO INFO",
          land_segment = 656431.7) %>%
  arrange(date) 


animated_plot <- ggplot(data2) +
  geom_line(aes(x = date, y = land), color = "black") +
  labs(title = "ACRES OF LAND OWNED BY BLACK PEOPLE 
                          IN GEORGIA.",
       x = "",
       y = "LAND",
       caption = "#DuboisChallenge2025 | Week 2 | Prepared by C. YAZICI") +
  scale_x_continuous(limits = c(1874, 1918)) +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  # Animate the sliding window by showing only the last 'window_size' points
  geom_segment(aes(x = date, y = land_segment,
                   xend = 1901, yend = land_segment),
               linetype = 2, colour = 'black') + 
  geom_point(aes(x = date, y = land), size = 2) + 
  geom_text(aes(y = land_segment,
                x = 1901, label = paste0("Year: ", date, "\n", 
                                         "Land (in acres): ", land_text)),
            hjust = 0, colour = '#dc143c', family = "ps", size = 3) + 
  
  theme(plot.margin = unit(c(2, 1, 1, 1), "cm"),
        legend.position = "none",
        panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA), 
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, 
                                                    b = 0, l = 0),
                                    family = "ps", hjust = 0.5),
        axis.text = element_text(family = "ps", size = 10.5),
        plot.title = element_text(family = "ps", hjust = 0.5, size = 11,
                                  lineheight = 1.9),
        plot.caption = element_text(family = "ps", hjust = 0.9, size = 10,
                                    vjust = 10, lineheight = 1.5)) +
  transition_reveal(date) + 
  coord_cartesian(clip = 'off') 


animate(animated_plot, duration = 20, fps = 20, width = 400, height = 400,
        renderer = gifski_renderer())
anim_save("Week2.gif")


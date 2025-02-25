library(tidyverse)
library(janitor)
library(readr)
library(showtext)
library(ggchicklet)


data4 <- read.csv("data.csv") %>%
  clean_names() %>%
  arrange(year) %>%
  mutate(pct_change = (property_valuation/lag(property_valuation) - 1) * 100,
         label_plot = paste0("Year: ", year, "\nChange (in %): ", 
                             round(pct_change, 2)))
  

p <- ggplot(data4) +
  geom_line(aes(x = year, y = property_valuation), color = "black") +
  ggchicklet:::geom_rrect(aes(xmin = year - 1.2, xmax = year  + 24, 
                              ymin = property_valuation - 230000, 
                              ymax = property_valuation + 300000), 
                          fill = "lightblue", 
                          colour = "black",
                          radius = unit(0.1, units = "cm")) +
  geom_text(aes(x = year, y = property_valuation,
                label = label_plot, color = color_label),
            hjust = 0, family = "ps", colour = '#dc143c', size = 4,
            fontface = "bold") +
  geom_point(aes(x = year, y = property_valuation,
                  group = year), size = 3) + 
  transition_reveal(along = year) + 
  view_follow(fixed_x = TRUE, fixed_y = TRUE) +
  coord_cartesian(clip = 'off') +
  scale_y_continuous(limits = c(0, 5500000), 
                      breaks = c(seq(1000000, 4000000, 1000000)), 
                      labels = comma) +
   scale_x_continuous(limits = c(1858, 1925), 
                      breaks = c(seq(1870, 1900, 5))) +
   labs(x = "",
        y = "",
        title = "VALUATION OF TOWN AND CITY PROPERTY OWNED\nBY BLACK GEORGIANS.",
        caption = "\n#DuboisChallenge2025 | Week 4\n\nPrepared by C. YAZICI") +
   theme(
     panel.background = element_rect(fill = "#e7d6c5", color = NA),
     plot.background = element_rect(fill = "#e7d6c5", color = NA),
     axis.ticks = element_blank(),
     axis.text.x = element_text(family = "ps", size = 9),
     panel.grid = element_blank(),
     plot.caption = element_text(family = "ps", hjust = 1, size = 10,
                                 vjust = 10),
     plot.title = element_text(family = "ps", hjust = 0.5, size = 15,
                                face = "bold",
                               lineheight = 1.2,
                               margin = margin(0, 0, 6, 0)) ,
     plot.margin = unit(c(2, 1, 1, 1), "cm")) +
   annotate("text", x = 1858, y = 4850000, 
            label = "DOLLARS", fontface = "bold",
            family = "ps", hjust = 1.3, size = 4)


animate(p, duration = 20, fps = 20, width = 600, height = 600,
        renderer = gifski_renderer())

anim_save("Week 4_animation.gif")   
 
 
   
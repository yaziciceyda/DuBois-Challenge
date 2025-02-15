library(gganimate)

p <- ggplot(data1, aes(x = year, y = land_value_dollars)) +
  geom_line(linewidth = 2, colour = "#000000") +
  geom_image(aes(x = year, y = land_value_dollars, image = img,
                 group = seq_along(year))) +
  geom_image(aes(x = year, y = land_value_dollars, image = img)) +
  scale_size_identity() +
  transition_reveal(year) +
  labs(title = "VALUE OF LAND OWNED BY BLACK PEOPLE IN GEORGIA",
       caption = "#DuboisChallenge2025| Week 1 | Prepared by C. YAZICI") +
  scale_y_continuous(labels = scales::dollar_format(scale = .000001,
                                                    prefix ="$", suffix = "M")) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA), 
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "ps"),
        plot.title = element_text(family = "ps", hjust = 0.5, size = 9),
        plot.caption = element_text(family = "ps", hjust = 1, size = 7),
        plot.margin = unit(c(2, 1, 2, 1), "cm")) +
  view_follow(fixed_x = TRUE, fixed_y = TRUE)


animate(p, rewind = FALSE)

anim_save("week1.gif", p)

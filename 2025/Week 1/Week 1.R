library(tidyverse)
library(remotes)
remotes::install_gitlab("hrbrmstr/ggchicklet")
library(ggchicklet)
library(readxl)
library(janitor)
library(ggimage)
library(scales)
library(showtext)

###########

font_add_google('Public Sans', 'ps')
showtext_auto()


theta <- seq(0, 2 * pi, length.out = 1000)
a <- 1
b <- 3
k <- 70
r <- k * (cos(theta)^2 + a * cos(theta) + b)

egg_data <- data.frame(
  x = r * sin(theta),
  y = -1 * r * cos(theta)
) %>% mutate(y = ifelse(x > -100 & x < 100 & y > 0, 180, y))



x1 = seq(-100, -120.38, length.out = 120)
y1 = seq(180, 339, length.out = 120)

x2 = seq(100, 100, length.out = 120)
y2 = seq(350, 180, length.out = 120)

x3 = seq(100, -100, length.out = 120)
y3 = seq(180, 180, length.out = 120)


theta2 = x = seq(-38 * pi, 32 * pi, by = 5)

d6 = data.frame(x = x1, y = y1) %>%
  add_row(x = theta2, 
          y = cos(x) * 50 + 300) %>%
  add_row(x = x2, 
          y = y2) %>%
  add_row(x = x3, 
          y = y3)

p <- ggplot(data = d6,
       mapping = aes(x = x, y = y)) +
  geom_polygon(fill = "#ac844e") +
  geom_polygon(data = egg_data,
               mapping = aes(x = x, y = y),
               fill = "#ac844e") +
  ggchicklet:::geom_rrect(aes(xmin = -105, xmax = 105, 
                              ymin = 170, ymax = 190), 
                          fill = "#654321", 
                          radius = unit(0.1, units = "cm")) +
  annotate("segment", x = 0, xend = 50, 
                   y = 180, yend = 0, lineend = "round",
                   colour = "#654321", linewidth = 10) +
  annotate("segment", x = 0, xend = -50, 
           y = 180, yend = 0, lineend = "round",
           colour = "#654321", linewidth = 10) +
  annotate("point", x = 0, y = 185, colour = "#654321", size = 25) +
  coord_cartesian() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA), 
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))

ggsave("money_bag.png", p, width = 22, height = 26, dpi = 72)


data1 <- read_csv("data.csv")

asp_ratio <- 0.618

data1 <- data1 %>%
  clean_names() %>%
  arrange(year) %>%
  mutate(y = c(3, 2.9, 2.8, 2.7, 2.6, 2.5),
         img = paste0(here::here(), "/money_bag.png"),
         image_size = c(0.15, 0.15, 0.15, 0.22, 0.22, 0.22),
         year_text_y = c(rep(0.037, 3), rep(0.049, 3)))
        # image_size = round(land_value_dollars / 10000000, 2))



week1 <- ggplot(data1) +
   geom_image(aes(x = 1, y = y, image = img, size = I(image_size)),
              by = "width", asp = asp_ratio) +
             #    size = I(image_size))) +
  # land_value_dollars
  geom_text(aes(x = 1, y = y - 0.01, label = scales::dollar(land_value_dollars)), 
            size = 8, family = "ps") +
  # year
  geom_text(aes(x = 1, y = y - year_text_y, label = year), 
            size = 10, family = "ps") +
  scale_size_identity() +
  coord_cartesian(ylim = c(2.45, 3.02)) +
  labs(title = "VALUE OF LAND OWNED BY BLACK PEOPLE IN GEORGIA",
       caption = "#DuboisChallenge2025| Week 1 | Prepared by C. YAZICI") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA), 
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(family = "ps", hjust = 0.5, size = 35),
        plot.caption = element_text(family = "ps", hjust = 1, size = 25),
  plot.margin = unit(c(2, 1, 2, 1), "cm"))


ggsave("week1.png", week1, width = 22, height = 30, dpi = 72)




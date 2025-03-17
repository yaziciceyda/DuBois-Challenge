library(tidyverse)
library(readxl)
library(janitor)
library(showtext)
library(scales)
library(gganimate)


font_add_google('Public Sans', 'ps')
showtext_auto()



data7 <- read_csv("data.csv") %>%
  clean_names() %>%
  mutate(y_text = rev(row_number()),
         label_text = case_when(
           year == 1875 ~ "-------$ ", 
           year == 1880 ~ "----$ ",
           year == 1885 ~ "----\"   ",
           year == 1890 ~ "---\"  ",
           .default = "---\" "
         ))


###############

# 1875

a6 <- 10
b6 <- 2
theta6 <- seq(14, 4.5 * pi, 0.01)
r6 <- a6 + b6 * theta6
df6 <- tibble(x = rev(r6 * cos(theta6)), 
                  y = rev(r6 * sin(theta6)),
                    year = rep(1875, each = 14),
              value = rep(21186, each = 14),
              color_year = rep("#ffc0cb", each = 14))


# 1880

a5 <- 9
b5 <- 2
theta5 <- seq(11.4, 4.5 * pi, 0.01)
r5 <- a5 + b5 * theta5
df5 <- tibble(x = rev(r5 * cos(theta5)), 
                  y = rev(r5 * sin(theta5)),
                   year = rep(1880, each = 274),
              value = rep(498532, each = 274),
              color_year = rep("#77a0c2", each = 274))


# 1885

a4 <- 8
b4 <- 2
theta4 <- seq(9.8, 4.5 * pi, 0.01)
r4 <- a4 + b4 * theta4
df4 <- tibble(x = rev(r4 * cos(theta4)), 
                  y = rev(r4 * sin(theta4)),
              year = rep(1885, each = 434),
              value = rep(736170, each = 434),
              color_year = rep("#ad8761", each = 434))

# 1890

a3 <- 7
b3 <- 2
theta3 <- seq(6, 4.5 * pi, 0.01)
r3 <- a3 + b3 * theta3
df3 <- tibble(x = rev(r3 * cos(theta3)), 
                  y = rev(r3 * sin(theta3)),
              year = rep(1890, each = 814),
              value = rep(1173624, each = 814),
              color_year = rep("#ffd700", each = 814))
              

# 1895

a2 <- 6
b2 <- 2
theta2 <- seq(4.4, 4.5 * pi, 0.01)
r2 <- a2 + b2 * theta2
df2 <- data.frame(x = rev(r2 * cos(theta2)), 
                  y = rev(r2 * sin(theta2)),
                  year = rep(1895, each = 974),
                  value = rep(1322694, each = 974),
                  color_year = rep("#d2b48c", each = 974))

                  
# 1899

a <- 5
b <- 2
theta <- seq(2, 4.5 * pi, 0.01)
r <- a + b * theta
df <- data.frame(x = rev(r * cos(theta)), 
                 y = rev(r * sin(theta)),
                 year = rep(1899, each = 1214),
                 value = rep(1434975, each = 1214),
                 color_year = rep("#dc143c", each = 1214))
                 


all_data <- df6 %>%
  add_row(df5) %>%
  add_row(df4) %>%
  add_row(df3) %>%
  add_row(df2) %>%
  add_row(df) %>%
  mutate(r = row_number(),
         y_text = case_when(
           year == 1875 ~ 38,
           year == 1880 ~ 37,
           year == 1885 ~ 36,
           year == 1890 ~ 35,
           year == 1895 ~ 34,
           year == 1899 ~ 33))



p <- ggplot(all_data) +
  geom_point(aes(x, y, group = r),  size = 4,
             color = "black") +
  geom_point(aes(x, y, color = color_year,
                 group = r),  size = 3) +
  scale_colour_identity() +
  geom_text(aes(x = 5, y = 4, 
                label =  year), family = "ps", hjust = 0.5, size = 5) +
  geom_text(aes(x = 5, y = 0, 
                label =  paste0("$", scales::comma(value))), 
                family = "ps", hjust = 0.5, size = 5) +
  transition_reveal(along = r) + 
  view_follow(fixed_x = TRUE, fixed_y = TRUE) +
  coord_cartesian(clip = 'off') +
  labs(title = "ASSESSED VALUE OF HOUSEHOLD AND KITCHEN FURNITURE\n
                     OWNED BY BLACK GEORGIANS.",
       caption = "#DuboisChallenge2025 | Week 7 | Prepared by C. YAZICI") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA), 
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(family = "ps", hjust = 0.5, size = 13,
                                  lineheight = 0.8, face = "bold"),
        plot.caption = element_text(family = "ps", hjust = 0.9, size = 12),
        plot.margin = unit(c(2.0, 1.0, 0.5, 1), "cm")) 

  



animate(p, duration = 25, fps = 20, width = 600, height = 600,
        renderer = gifski_renderer())

anim_save("Week 7_animation.gif") 


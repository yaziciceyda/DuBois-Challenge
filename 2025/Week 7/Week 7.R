library(tidyverse)
library(readxl)
library(janitor)
library(showtext)
library(scales)



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


### 

t <- seq(3.5 * pi, 0, length.out = 1000) 

spiral <- data.frame(x    = sin(t) * 1:1000, 
                     y    = cos(t) * 1:1000)

ggplot() + 
  geom_point(data=spiral, aes(x,y), col='red3', size=2)

###############



theta1 <- seq(2.7, 4.5*pi, 0.01)
r1 <- 0.5 + 0.5*theta1
df1 <- data.frame(x=r1*cos(theta1), y=r1*sin(theta1)) 


ggplot() + 
  geom_point(data=df1, aes(x,y), col='red3', size=2)




# 1899
a <- 5
b <- 2
theta <- seq(2, 4.5 * pi, 0.01)
r <- a + b * theta
df <- data.frame(x = r * cos(theta), 
                 y = r * sin(theta)) # Cartesian coords

ggplot(df, aes(x, y)) + 
  geom_path(col = 'red')


# 1895

a2 <- 6
b2 <- 2
theta2 <- seq(4.4, 4.5 * pi, 0.01)
r2 <- a2 + b2 * theta2
df2 <- data.frame(x = r2 * cos(theta2), 
                  y = r2 * sin(theta2)) # Cartesian coords

ggplot(df2, aes(x, y)) + 
  geom_path(col = 'red')

# 1890

a3 <- 7
b3 <- 2
theta3 <- seq(6, 4.5 * pi, 0.01)
r3 <- a3 + b3 * theta3
df3 <- data.frame(x = r3 * cos(theta3), 
                  y = r3 * sin(theta3)) # Cartesian coords

ggplot(df3, aes(x, y)) + 
  geom_path(col = 'red')



# 1885

a4 <- 8
b4 <- 2
theta4 <- seq(9.8, 4.5 * pi, 0.01)
r4 <- a4 + b4 * theta4
df4 <- data.frame(x = r4 * cos(theta4), 
                  y = r4 * sin(theta4)) # Cartesian coords

ggplot(df4, aes(x, y)) + 
  geom_path(col = 'red')


# 1880


a5 <- 9
b5 <- 2
theta5 <- seq(11.4, 4.5 * pi, 0.01)
r5 <- a5 + b5 * theta5
df5 <- data.frame(x = r5 * cos(theta5), 
                  y = r5 * sin(theta5)) # Cartesian coords

ggplot(df5, aes(x, y)) + 
  geom_path(col = 'red')


# 1875

a6 <- 10
b6 <- 2
theta6 <- seq(14, 4.5 * pi, 0.01)
r6 <- a6 + b6 * theta6
df6 <- data.frame(x = r6 * cos(theta6), 
                  y = r6 * sin(theta6)) # Cartesian coords

ggplot(df6, aes(x, y)) + 
  geom_path(col = 'red')




p <- ggplot() +
  geom_path(df, mapping = aes(x, y), col = "black", size = 12) +
  geom_path(df, mapping = aes(x, y), col = "#dc143c", size = 11.5) +
  geom_path(df2, mapping = aes(x, y), col = "black", size = 12) +
  geom_path(df2, mapping = aes(x, y), col = "#d2b48c", size = 11.5) +
  geom_path(df3, mapping = aes(x, y), col = "black", size = 12) +
  geom_path(df3, mapping = aes(x, y), col = "#ffd700", size = 11.5) +
  geom_path(df4, mapping = aes(x, y), col = "black", size = 12) +
  geom_path(df4, mapping = aes(x, y), col = "#ad8761", size = 11.5) +
  geom_path(df5, mapping = aes(x, y), col = "black", size = 12) +
  geom_path(df5, mapping = aes(x, y), col = "#77a0c2", size = 11.5) +
  geom_path(df6, mapping = aes(x, y), col = "black", size = 12) +
  geom_path(df6, mapping = aes(x, y), col = "#ffc0cb", size = 11.5) +
  geom_text(data7, mapping = aes(x = -1.5, y = y_text + 32, 
                             label = paste0(year, label_text, 
                                            scales::comma(houshold_value_dollars))),
            family = "ps", size = 8, hjust = 1) +
  coord_fixed() +
  labs(title = "ASSESSED VALUE OF HOUSEHOLD AND KITCHEN FURNITURE\nOWNED BY BLACK GEORGIANS.",
       caption = "#DuboisChallenge2025 | Week 7 | Prepared by C. YAZICI") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA), 
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(family = "ps", hjust = 0.5, size = 40,
                                  lineheight = 1.2, face = "bold"),
        plot.caption = element_text(family = "ps", hjust = 0.9, size = 25),
        plot.margin = unit(c(0.5, 1, 0.5, 1), "cm")) 

  

p




ggsave("Week 7.png", p, width = 24, height = 30, dpi = 72)






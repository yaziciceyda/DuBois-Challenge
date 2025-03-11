library(tidyverse)
library(readxl)
library(janitor)
library(showtext)
library(scales)
install.packages("ggbrace")
library(ggbrace)

font_add_google('Public Sans', 'ps')
showtext_auto()



data6 <- read_csv("data.csv") %>%
  clean_names() %>%
  mutate(x_max_owners = owners / 100,
         y_max_owners = case_when(
           year == 1880 & city == "Savannah" ~ 15,
           year == 1880 & city == "Atlanta" ~ 14.5,
           year == 1890 & city == "Savannah" ~ 9,
           year == 1890 & city == "Atlanta" ~ 8.5,
           year == 1898 & city == "Savannah" ~ 3.5,
           year == 1898 & city == "Atlanta" ~ 3,
         ),
         y_max_property =  property_value_dollars * 15/ 1308995, 
         x_max_property = case_when(
           year == 1898 & city == "Savannah" ~ 16,
           year == 1898 & city == "Atlanta" ~ 17,
           year == 1890 & city == "Savannah" ~ 9.5,
           year == 1890 & city == "Atlanta" ~ 10.5,
           year == 1880 & city == "Savannah" ~ 3,
           year == 1880 & city == "Atlanta" ~ 4,
         ),
         fill_city = ifelse(city == "Atlanta", "#4682b4", "#ffd700"))

######################

p <- ggplot() +
  geom_rect(data6 %>%
              filter(year == 1898),
            mapping = aes(xmin = x_max_property - 1, 
                          xmax = x_max_property,
                          ymin = 0,
                          ymax = y_max_property,
                          fill = fill_city),
            colour = "black") +
  geom_rect(data6 %>%
              filter(year <= 1890),
            mapping = aes(xmin = 0, xmax = x_max_owners,
                          ymin = y_max_owners - 0.5,
                          ymax = y_max_owners,
                          fill = fill_city),
            colour = "black") +
  geom_rect(data6 %>%
              filter(year == 1890),
            mapping = aes(xmin = x_max_property - 1, 
                          xmax = x_max_property,
                          ymin = 0,
                          ymax = y_max_property,
                          fill = fill_city),
            colour = "black") +
  geom_rect(data6 %>%
              filter(year == 1898),
            mapping = aes(xmin = 0, xmax = x_max_owners,
                          ymin = y_max_owners - 0.5,
                          ymax = y_max_owners,
                          fill = fill_city),
            colour = "black") +
  geom_rect(data6 %>%
              filter(year == 1880),
            mapping = aes(xmin = x_max_property - 1, 
                          xmax = x_max_property,
                          ymin = 0,
                          ymax = y_max_property,
                          fill = fill_city),
            colour = "black") +
  scale_fill_identity() +
  # The values for owners
  geom_text(data6, mapping = aes(x = x_max_owners / 2, y = y_max_owners - 0.25,
                label = scales::comma(owners)), hjust = 0.5, colour = "black",
            size = 10, family = "ps") +
  # The values for property_value_dollars
  geom_text(data6 %>%
              filter(year != 1890), mapping = aes(x = x_max_property - 0.4, 
                                                  y = y_max_property / 2,
                  label = paste0("$ ", scales::comma(property_value_dollars))), 
            hjust = 0.5, colour = "black", family = "ps",
            size = 10, angle = 90) +
  geom_text(data6 %>%
              filter(year == 1890), mapping = aes(x = x_max_property - 0.4, 
                                                  y = y_max_property / 2 + 1,
            label = paste0("$ ", scales::comma(property_value_dollars))), 
            hjust = 0, colour = "black", family = "ps",
            size = 10, angle = 90) +
  # The years for owners
  annotate("text", x = 3, y = -0.2, label = "1880",
           hjust = 0.5, colour = "black", size = 10, family = "ps") +
  annotate("text", x = 9.5, y = -0.2, label = "1890",
           hjust = 0.5, colour = "black", size = 10, family = "ps") +
  annotate("text", x = 16, y = -0.2, label = "1898",
           hjust = 0.5, colour = "black", size = 10, family = "ps") +
  # The curly braces in the y-axis 
  # 1898
  stat_brace(aes(x = c(-0.01, -0.21), y = c(2.4, 3.55)),  rotate = 270,
             width = 0.8) +
  annotate("text", x = -2.45, y = 2.95, label = "1898",
           hjust = 0, colour = "black", size = 9, family = "ps") +
  # The curly braces in the y-axis 
  # 1890
  stat_brace(aes(x = c(-0.01, -0.21), y = c(7.9, 9.05)),  rotate = 270,
             width = 0.8) +
  annotate("text", x = -2.45, y = 8.45, label = "1890",
           hjust = 0, colour = "black", size = 9, family = "ps") +
  # The curly braces in the y-axis 
  # 1880
  stat_brace(aes(x = c(-0.01, -0.21), y = c(13.9, 15.05)),  rotate = 270,
             width = 0.8) +
  annotate("text", x = -2.45, y = 14.45, label = "1880",
           hjust = 0, colour = "black", size = 9, family = "ps") +
  # The overall curly braces for Owners
  stat_brace(aes(x = c(-2.5, -1), y = c(2, 15.5)),  rotate = 270,
             width = 0.8) +
  annotate("text", x = -7, y = 8.8, label = "OWNERS",
           hjust = 0, colour = "black", size = 9, family = "ps") +
  # The overall curly braces for Property
  stat_brace(aes(x = c(1.7, 17.6), y = c(0.1, -0.1)),  rotate = 180,
             width = 0.8) +
  annotate("text", x = 9.8, y = -1.5, label = "PROPERTY",
           hjust = 0.5, colour = "black", size = 9, family = "ps") +
  # The Legend for the cities
  # SAVANNAH
  annotate("rect", xmin = 2, xmax = 4,
           ymin = -2.5, ymax = -2, fill = "#ffd700", colour = "black") +
  annotate("text", x = 4.1, y = -2.25, label = "= SAVANNAH",
           hjust = 0, colour = "black", size = 9, family = "ps") +
  # The Legend for the cities
  # ATLANTA
  annotate("rect", xmin = 15, xmax = 17,
           ymin = -2.5, ymax = -2, fill = "#4682b4", colour = "black") +
  annotate("text", x = 11.5, y = -2.25, label = "ATLANTA = ",
           hjust = 0, colour = "black", size = 9, family = "ps") +
  coord_cartesian(xlim = c(-7, 21),
                  ylim = c(-3, 16)) +
  labs(title = "BLACK GEORGIANS' PROPERTY IN TWO CITIES\n OF GEORGIA.",
       caption = "#DuboisChallenge2025 | Week 6 | Prepared by C. YAZICI") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA), 
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(family = "ps", hjust = 0.5, size = 35,
                                  lineheight = 1.2, face = "bold"),
        plot.caption = element_text(family = "ps", hjust = 0.9, size = 25),
        plot.margin = unit(c(1.2, 0.5, 1.2, 0.5), "cm")) 

p

ggsave("Week 6.png", p, width = 24, height = 30, dpi = 72)


Week10 <- function()
{

library(tidyverse)
library(readxl)
library(janitor)
library(showtext)
library(cowplot)
library(ggbrace)

font_add_google('Public Sans', 'ps')
showtext_auto()


data10 <- read_csv("data10.csv") %>%
  clean_names() %>%
  mutate(y_segment = rev(seq(2, 14, by = 2)),
         class = ifelse(class == "Over $1000", 
                        "$1,000\n
                        AND OVER",
                        class))


p <- ggplot() +
  # rent
  geom_rect(data10, mapping = aes(xmin = 0, xmax = rent,
                   ymin = y_segment, ymax = y_segment + 1), fill = "black") +
  geom_text(data10 %>%
              filter(rent != 0),
            mapping = aes(x = rent / 2, y  = y_segment  + 0.5, 
                label = paste0(rent, "%")),
            family = "ps", hjust = 0.5, color = "ivory", size = 10) +
  # food
  geom_rect(data10, mapping = aes(xmin = rent, xmax = rent + food,
                ymin = y_segment, ymax = y_segment + 1), fill = "#ec87ec",
            color = "black") +
  geom_text(data10 %>%
              filter(food != 0),
            mapping = aes(x = (2 * rent + food) / 2, y  = y_segment  + 0.5, 
                label = paste0(food, "%")),
            family = "ps", hjust = 0.5, color = "black", size = 10) +
  # clothes
  geom_rect(data10, mapping = aes(xmin = rent + food, xmax = rent + food + clothes,
                ymin = y_segment, ymax = y_segment + 1), fill = "#ec878a",
            color = "black") +
  geom_text(data10 %>%
              filter(clothes != 0),
            mapping = aes(x = (2 * (rent + food) + clothes) / 2, 
                          y  = y_segment  + 0.5, 
                label = paste0(clothes, "%")),
            family = "ps", hjust = 0.5, color = "black", size = 10) +
  # tax
  geom_rect(data10, mapping = aes(xmin = rent + food + clothes, 
                                  xmax = rent + food + clothes + tax,
                ymin = y_segment, ymax = y_segment + 1), fill = "#8c8ae2",
            color = "black") +
  geom_text(data10 %>%
              filter(tax != 0),
            mapping = aes(x = (2 * (rent + food + clothes) + tax) / 2, 
                          y  = y_segment  + 0.5, 
                label = paste0(tax, "%")),
            family = "ps", hjust = 0.5, color = "black", size = 8) +
  # other
  geom_rect(data10, mapping = aes(xmin = rent + food + clothes + tax, 
                xmax = rent + food + clothes + tax + other,
                ymin = y_segment, ymax = y_segment + 1), fill = "#afadde",
            color = "black") +
  geom_text(data10 %>%
              filter(other != 0),
            mapping = aes(x = (2 * (rent + food + clothes + tax) + other) / 2, 
                          y  = y_segment  + 0.5, 
                label = paste0(other, "%")),
            family = "ps", hjust = 0.5, color = "black", size = 10) +
  # Legends at the top
  annotate("rect", xmin = 0, xmax = 19.5,
           ymin = 16, ymax = 16.7, fill = "black") +
  annotate("rect", xmin = 19.5, xmax = 39.5,
           ymin = 16, ymax = 16.7, fill = "#ec87ec", color = "black") +
  annotate("rect", xmin = 39.5, xmax = 59,
           ymin = 16, ymax = 16.7, fill = "#ec878a", color = "black") +
  annotate("rect", xmin = 59, xmax = 78.7,
           ymin = 16, ymax = 16.7, fill = "#8c8ae2", color = "black") +
  annotate("rect", xmin = 78.7, xmax = 98.5,
           ymin = 16, ymax = 16.7, fill = "#afadde", color = "black") +
  # Table
  geom_text(data10, mapping = aes(x = -15, y = y_segment + 0.5, label = class),
            family = "ps", size = 7, hjust = 1) +
  geom_text(data10, mapping = aes(x = -12, y = y_segment + 0.5, 
                label = paste0("$", actual_average)),
            family = "ps", size = 7, hjust = 0) +
  geom_segment(data10, mapping = aes(x = -24, xend = 4, 
                   y = y_segment - 0.5, yend = y_segment - 0.5),
               arrow = arrow()) + 
  annotate("segment", x = -24, xend = -24, 
                   y = 1.5, yend = 16) +
  annotate("segment", x = -24, xend = -24, 
           y = 1.5, yend = 16) +
  annotate("segment", x = -24, xend = 4, 
           y = 15.5, yend = 15.5, arrow = arrow()) +
  annotate("text", x = -18.5, y = 15.7, label = "CLASS",
           family = "ps", size = 6, hjust = 0.5) +
  annotate("text", x = -8, y = 15.7, label = "ACTUAL AVERAGE",
           family = "ps", size = 6, hjust = 0.5) +
  annotate("segment", x = -24, xend = -1.5, 
           y = 16, yend = 16) +
  annotate("segment", x = -1.5, xend = -1.5, 
           y = 1.5, yend = 16) +
  annotate("segment", x = -14.5, xend = -14.5, 
           y = 1.5, yend = 16) +
  # lines connecting bars
  # legend
  annotate("segment", x = 19.5, xend = 19, 
           y = 16, yend = 15) +
  # rent
  annotate("segment", x = 19, xend = 22, 
           y = 14, yend = 13) +
  annotate("segment", x = 22, xend = 23, 
           y = 12, yend = 11) +
  annotate("segment", x = 23, xend = 18, 
           y = 10, yend = 9) +
  annotate("segment", x = 18, xend = 13, 
           y = 8, yend = 7) +
  annotate("segment", x = 13, xend = 0, 
           y = 6, yend = 5) +
  # food
  annotate("segment", x = 40, xend = 62, 
           y = 16, yend = 15) +
  annotate("segment", x = 62, xend = 69, 
           y = 14, yend = 13) +
  annotate("segment", x = 69, xend = 66, 
           y = 12, yend = 11) +
  annotate("segment", x = 66, xend = 55, 
           y = 10, yend = 9) +
  annotate("segment", x = 55, xend = 44, 
           y = 8, yend = 7) +
  annotate("segment", x = 44, xend = 37, 
           y = 6, yend = 5) +
  annotate("segment", x = 37, xend = 29, 
           y = 4, yend = 3) +
  # clothes
  annotate("segment", x = 60, xend = 90, 
           y = 16, yend = 15) +
  annotate("segment", x = 90, xend = 92, 
           y = 14, yend = 13) +
  annotate("segment", x = 92, xend = 84, 
           y = 12, yend = 11) +
  annotate("segment", x = 84, xend = 70, 
           y = 10, yend = 9) +
  annotate("segment", x = 70, xend = 61, 
           y = 8, yend = 7) +
  annotate("segment", x = 61, xend = 56, 
           y = 6, yend = 5) +
  annotate("segment", x = 56, xend = 45, 
           y = 4, yend = 3) +
  # tax
  annotate("segment", x = 79.7, xend = 90, 
           y = 16, yend = 15) +
  annotate("segment", x = 90, xend = 96, 
           y = 14, yend = 13) +
  annotate("segment", x = 96, xend = 88.5, 
           y = 12, yend = 11) +
  annotate("segment", x = 88.5, xend = 75.5, 
           y = 10, yend = 9) +
  annotate("segment", x = 75.5, xend = 66, 
           y = 8, yend = 7) +
  annotate("segment", x = 66, xend = 64, 
           y = 6, yend = 5) +
  annotate("segment", x = 64, xend = 49.5, 
           y = 4, yend = 3) +
  # the curly braces at the right
  stat_brace(aes(x = c(98, 101.5), y = c(15.2, 11.8)),  rotate = 90,
             width = 0.8) +
  stat_brace(aes(x = c(98, 101.5), y = c(11.2, 7.8)),  rotate = 90,
             width = 0.8) +
  stat_brace(aes(x = c(98, 101.5), y = c(7.2, 3.8)),  rotate = 90,
             width = 0.8) +
  stat_brace(aes(x = c(98, 101.5), y = c(3.2, 1.8)),  rotate = 90,
             width = 0.8) +
  annotate("text", x = 103.5, y = 13.5, label = "POOR.", angle = 90,
           family = "ps", size = 6) +
  annotate("text", x = 103.5, y = 9.5, label = "FAIR.", angle = 90,
           family = "ps", size = 6) +
  annotate("text", x = 103.5, y = 5.5, label = "COMFORTABLE.", angle = 90,
           family = "ps", size = 6) +
  annotate("text", x = 103.5, y = 2.5, label = "WELL-TO-DO", angle = 90,
           family = "ps", size = 6) +
  
  coord_cartesian(xlim = c(-19, 105),
                  ylim = c(0, 23)) +
  theme(
        panel.background = element_rect(fill = "#d0bba8", color = NA),
        plot.background = element_rect(fill = "#d0bba8", color = NA), 
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())


p_final <- ggdraw() +
  draw_plot(p) +annotate("segment", x = 79.7, xend = 90, 
           y = 16, yend = 15) +
  draw_image("C:/Users/Ceyda/OneDrive/Desktop/DataViz/DuBois/2025/Week 10/image1.jpg", 
             x = -0.41, y = 0.3, scale = 0.2) +
  
  draw_image("C:/Users/Ceyda/OneDrive/Desktop/DataViz/DuBois/2025/Week 10/image2.jpg", 
             x = 0.04, y = 0.31, scale = 0.73) +
  # Title
  draw_label(x = 0.49, y = 0.95, 
             label = "INCOME AND EXPENDITURE OF 150 BLACK FAMILIES IN ATLANTA, GA., U.S.A",
             fontfamily = "ps",
             hjust = 0.5, size = 40, fontface = "bold") +
  # Caption
  draw_label(x = 00.43, y = 0.05, 
             label = "#DuboisChallenge2025 | Week 10 | Prepared by C. YAZICI",
             fontfamily = "ps",
             hjust = 0.0, size = 25, fontface = "bold") +
  # Title at the bottom
  draw_label(x = 0.32, y = 0.11, 
             label = "FOR FURTHER STATISTICS RAISE THIS FRAME.",
             fontfamily = "ps",
             hjust = 0.0, size = 28)
  

  
ggsave("Week 10.png", p_final, width = 32, height = 26, dpi = 72)

return(p_final)
}

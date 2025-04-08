library(tidyverse)
library(readxl)
library(janitor)
library(showtext)
library(ggbrick)



font_add_google('Public Sans', 'ps')
showtext_auto()


data10 <- read_csv("data.csv") %>%
  clean_names() %>%
  mutate(x = row_number())  
  


data10_long <- data10 %>%
  pivot_longer(cols = rent:other,
               names_to = "category",
               values_to = "percentage") 
  
legend_data <- tibble(
  category = c("rent", "food", "clothes", "tax", "other"),
  x = seq(1.5, 5.5)
)

p <- ggplot() +
  geom_brick(data10_long, mapping = aes(class, percentage, fill = category),
             gap = 0.02) +
  geom_text(data10_long, mapping = aes(x = x, y = 103, 
                                       label = paste0("$", actual_average)),
                color = "black",
            size = 10, family = "ps", 
            check_overlap = TRUE) +
  coord_brick(xlim = c(0, 7.5),
              ylim = c(-7, 120)) +
  scale_fill_manual("", values = c("rent" = "black",
                                 "food" = "#ec87ec", 
                                 "clothes" = "#ec878a", 
                                 "tax" = "#7c7aeb", 
                                 "other" = "#afadde")) +
  # $100 - 200
  annotate("text", x = 1, y = 10, label = "19%",
           size = 10, family = "ps", color = "ivory",
           fontface = "bold") +
  annotate("text", x = 1, y = 40, label = "43%",
           size = 10, family = "ps", color = "black",
           fontface = "bold") +
  annotate("text", x = 1, y = 75, label = "28%",
           size = 10, family = "ps", color = "black",
           fontface = "bold") +
  annotate("text", x = 1, y = 95, label = "9.9%",
           size = 10, family = "ps", color = "black",
           fontface = "bold") +
  # $200 - 300
  annotate("text", x = 2, y = 10, label = "22%",
           size = 10, family = "ps", color = "ivory",
           fontface = "bold") +
  annotate("text", x = 2, y = 47, label = "47%",
           size = 10, family = "ps", color = "black",
           fontface = "bold") +
  annotate("text", x = 2, y = 80, label = "23%",
           size = 10, family = "ps", color = "black",
           fontface = "bold") +
  annotate("text", x = 2, y = 93.5, label = "4%",
           size = 8, family = "ps", color = "black") +
  annotate("text", x = 2, y = 97, label = "4%",
           size = 8, family = "ps", color = "black") +
  # $300 - 400
  annotate("text", x = 3, y = 10, label = "23%",
           size = 10, family = "ps", color = "ivory",
           fontface = "bold") +
  annotate("text", x = 3, y = 47, label = "43%",
           size = 10, family = "ps", color = "black",
           fontface = "bold") +
  annotate("text", x = 3, y = 76, label = "18%",
           size = 10, family = "ps", color = "black",
           fontface = "bold") +
  annotate("text", x = 3, y = 85, label = "4.5%",
           size = 8, family = "ps", color = "black") +
  annotate("text", x = 3, y = 95, label = "11.5%",
           size = 9, family = "ps", color = "black",
           fontface = "bold") +
  # $400 - 500
  annotate("text", x = 4, y = 10, label = "18%",
           size = 10, family = "ps", color = "ivory",
           fontface = "bold") +
  annotate("text", x = 4, y = 38, label = "37%",
           size = 10, family = "ps", color = "black",
           fontface = "bold") +
  annotate("text", x = 4, y = 64, label = "15%",
           size = 10, family = "ps", color = "black",
           fontface = "bold") +
  annotate("text", x = 4, y = 73, label = "5.5%",
           size = 8, family = "ps", color = "black") +
  annotate("text", x = 4, y = 88, label = "24.5%",
           size = 9, family = "ps", color = "black",
           fontface = "bold") +
  # $500 - 750
  annotate("text", x = 5, y = 7, label = "13%",
           size = 10, family = "ps", color = "ivory",
           fontface = "bold") +
  annotate("text", x = 5, y = 28, label = "31%",
           size = 10, family = "ps", color = "black",
           fontface = "bold") +
  annotate("text", x = 5, y = 52, label = "17%",
           size = 10, family = "ps", color = "black",
           fontface = "bold") +
  annotate("text", x = 5, y = 62, label = "5%",
           size = 8, family = "ps", color = "black") +
  annotate("text", x = 5, y = 81, label = "34%",
           size = 9, family = "ps", color = "black",
           fontface = "bold") +
  # $750 - 1000
  annotate("text", x = 6, y = 18, label = "37%",
           size = 10, family = "ps", color = "black",
           fontface = "bold") +
  annotate("text", x = 6, y = 48, label = "19%",
           size = 10, family = "ps", color = "black",
           fontface = "bold") +
  annotate("text", x = 6, y = 60, label = "8%",
           size = 10, family = "ps", color = "black",
           fontface = "bold") +
  annotate("text", x = 6, y = 81, label = "36%",
           size = 9, family = "ps", color = "black",
           fontface = "bold") +
  # Over $1000
  annotate("text", x = 7, y = 18, label = "29%",
           size = 10, family = "ps", color = "black",
           fontface = "bold") +
  annotate("text", x = 7, y = 38, label = "16%",
           size = 10, family = "ps", color = "black",
           fontface = "bold") +
  annotate("text", x = 7, y = 46, label = "4.5%",
           size = 8, family = "ps", color = "black",
           fontface = "bold") +
  annotate("text", x = 7, y = 76, label = "50.5%",
           size = 10, family = "ps", color = "black",
           fontface = "bold") +
  annotate("text", x = 0.01, y = 103, label = "ACTUAL\nAVERAGE",
           size = 10, family = "ps", color = "black",
           fontface = "bold") +
  geom_rect(legend_data, mapping = aes(xmin = x, xmax = x + 0.5,
                ymin = 110, ymax = 115, fill = category),
                color = "black") +
  geom_text(legend_data, mapping = aes(x = x + 0.25, y = 118, 
                                       label = toupper(category)),
            family = "ps", size = 10, hjust = 0.5) +
  annotate("segment", x = 0.35, xend = 0.75, 
           y = 103, yend = 103, arrow = arrow()) + 
  
  stat_brace(aes(x = c(0.55, 2.45), y = c(0.9, -3)),  rotate = 180,
             width = 1.8) +
  annotate("text", x = 1.5, y = -6.5, label = "POOR", 
           family = "ps", size = 8, hjust = 0.5) +
  stat_brace(aes(x = c(2.55, 4.45), y = c(0.9, -3)),  rotate = 180,
             width = 1.8) +
  annotate("text", x = 3.5, y = -6.5, label = "FAIR", 
           family = "ps", size = 8, hjust = 0.5) +
  stat_brace(aes(x = c(4.55, 6.45), y = c(0.9, -3)),  rotate = 180,
             width = 1.8) +
  annotate("text", x = 5.5, y = -6.5, label = "COMFORTABLE", 
           family = "ps", size = 8, hjust = 0.5) +
  stat_brace(aes(x = c(6.55, 7.45), y = c(0.9, -3)),  rotate = 180,
             width = 1.8) +
  annotate("text", x = 7, y = -6.5, label = "WELL-TO-DO", 
           family = "ps", size = 8, hjust = 0.5) +
  
  labs(x = "",
       y = "",
       title = "INCOME AND EXPENDITURE OF 150 BLACK FAMILIES IN ATLANTA, GA., U.S.A",
       caption = "#DuboisChallenge2025 | Week 10 | Prepared by C. YAZICI") +
  
  theme(
    panel.background = element_rect(fill = "#d0bba8", color = NA),
    plot.background = element_rect(fill = "#d0bba8", color = NA),
    panel.border = element_rect(colour = NA, fill = NA),
    panel.grid = element_blank(),
    axis.text.x = element_text(family = "ps", hjust = 0.5, size = 25),
    axis.text.y = element_blank(),
    plot.title = element_text(family = "ps", size = 40, hjust = 0.2,
                              margin = margin(30, 0, 0, 0)),
    plot.title.position = "plot",
    plot.caption = element_text(family = "ps", hjust = 0.9, size = 30,
                                vjust = 0.2,
                                margin = margin(0, 0, 30, 0)),
    legend.position = "none",
    legend.text = element_text(family = "ps", size = 23),
    plot.margin = unit(c(0.3, 0.8, 0.1, 0.8), "cm"),
    aspect.ratio = 0.7) 


ggsave("Week 10_version2.png", p, width = 32, height = 26, dpi = 72)



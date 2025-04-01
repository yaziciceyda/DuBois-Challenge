Week9 <- function()
{

library(tidyverse)
library(readxl)
library(janitor)
library(showtext)
library(forcats)
library(cowplot)
library(grid)

# Adapted from https://rpubs.com/xamanthalc/dubois and 
# https://github.com/nrennie/dubois_challenge/blob/main/2021/challenge_03.R


font_add_google('Public Sans', 'ps')
showtext_auto()


data9 <- read_csv("data9.csv") %>%
  clean_names() %>%
  add_row(group = "Negroes", occupation = "Dummy", percentage = 70)  %>%
  add_row(group = "Whites", occupation = "Dummy", percentage = 70) %>%
  mutate(r = row_number(),
         r = factor(r, levels = c(1, 3, 2, 5, 4, 11, 6, 8, 7, 10, 9, 12),
         ordered = TRUE),
         percentage = case_when(
           r == 4 ~ 1.5,
           r == 1 ~ 61,
           .default = percentage
         )) 


p <- ggplot() +
  geom_bar(data9,
             mapping = aes(x = "", y = percentage,
                                     fill = occupation, group = r),
           stat = "identity", width = 1) +
  scale_y_reverse() +
  coord_polar("y", start = 5.3, direction = 1) +
  scale_fill_manual("", values=c("Agriculture, Fisheries and Mining" = "#dc143c",
                                 "Manufacturing and Mechanical Industries" = "#4682b4", 
                                 "Domestic and Personal Service" = "#ffd700", 
                                 "Professions" = "#a26e3a", 
                                 "Trade and Transportation" = "#d2b48c", 
                                 "Dummy"="#e7d6c5")) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA), 
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())



g1 <- grid::circleGrob(gp = grid::gpar(fill = "#dc143c", color = "black"))
g2 <- grid::circleGrob(gp = grid::gpar(fill = "#4682b4", color = "black"))
g3 <- grid::circleGrob(gp = grid::gpar(fill = "#ffd700", color = "black"))
g4 <- grid::circleGrob(gp = grid::gpar(fill = "#a26e3a", color = "black"))
g5 <- grid::circleGrob(gp = grid::gpar(fill = "#d2b48c", color = "black"))



p_final <- ggdraw(p) +
  draw_label(x = 0.5, y = 0.85, label = "BLACK PEOPLE.", fontfamily = "ps",
             hjust = 0.5, size = 25) +
  draw_label(x = 0.5, y = 0.15, label = "WHITES.", fontfamily = "ps",
             hjust = 0.5, size = 25) +
  draw_label(x = 0.36, y = 0.77, label = "62%", fontfamily = "ps",
             hjust = 0.5, size = 25) +
  draw_label(x = 0.64, y = 0.77, label = "28%", fontfamily = "ps",
             hjust = 0.5, size = 25) +
  draw_label(x = 0.74, y = 0.74, label = "5%", fontfamily = "ps",
             hjust = 0.5, size = 25) +
  draw_label(x = 0.765, y = 0.72, label = "4.5%", fontfamily = "ps",
             hjust = 0.5, size = 25) +
  draw_label(x = 0.79, y = 0.715, label = "0.5%", fontfamily = "ps",
             hjust = 0.5, size = 12) +
  draw_label(x = 0.61, y = 0.24, label = "64%", fontfamily = "ps",
             hjust = 0.5, size = 25) +
  draw_label(x = 0.41, y = 0.19, label = "55%", fontfamily = "ps",
             hjust = 0.5, size = 25) +
  draw_label(x = 0.34, y = 0.22, label = "13.5%", fontfamily = "ps",
             hjust = 0.5, size = 25) +
  draw_label(x = 0.28, y = 0.25, label = "13%", fontfamily = "ps",
             hjust = 0.5, size = 25) +
  draw_label(x = 0.23, y = 0.29, label = "4%", fontfamily = "ps",
             hjust = 0.5, size = 25) +
  # Legends
  draw_grob(g1, x = -0.34, y = 0.06, scale = 0.04) +
  draw_label(x = 0.275, y = 0.56, label = "AGRICULTURE.FISHERIES\n
             AND MINING.",
             fontfamily = "ps",
             hjust = 0.5, size = 20) +
  draw_grob(g2, x = -0.34, y = -0.05, scale = 0.04) +
  draw_label(x = 0.27, y = 0.45, label = "MANUFACTURING AND\n 
          MECHANICAL INDUSTRIES.",
             fontfamily = "ps",
             hjust = 0.5, size = 20) +
  draw_grob(g3, x = 0.36, y = 0.1, scale = 0.04) +
  draw_label(x = 0.74, y = 0.6, label = "  DOMESTIC AND\n 
        PERSONAL SERVICE.",
             fontfamily = "ps",
             hjust = 0.5, size = 20) +
  draw_grob(g4, x = 0.36, y = 0.0, scale = 0.04) +
  draw_label(x = 0.78, y = 0.50, label = "PROFESSIONS.",
             fontfamily = "ps",
             hjust = 0.5, size = 20) +
  draw_grob(g5, x = 0.36, y = -0.1, scale = 0.04) +
  draw_label(x = 0.75, y = 0.40, label = "  TRADE AND\n 
             TRANSPORTATION.",
             fontfamily = "ps",
             hjust = 0.5, size = 20) +
  # Title
  draw_label(x = 0.49, y = 0.89, 
             label = "OCCUPATIONS OF BLACK PEOPLE AND WHITES IN GEORGIA.",
             fontfamily = "ps",
             hjust = 0.5, size = 40, fontface = "bold") +
  # Caption
  draw_label(x = 0.43, y = 0.11, 
             label = "#DuboisChallenge2025 | Week 9 | Prepared by C. YAZICI",
             fontfamily = "ps",
             hjust = 0.0, size = 30, fontface = "bold")
 

ggsave("Week 9.png", p_final, width = 27, height = 32, dpi = 72)

return(p_final)

}

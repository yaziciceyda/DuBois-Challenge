library(tidyverse)
library(janitor)
library(readr)
library(showtext)

# Font in the Plot

font_add_google('Public Sans', 'ps')
showtext_auto()

# Data Import & Wrangling

data5 <- read.csv("data.csv") %>%
  clean_names() %>%
  mutate(category = rev(category),
         percentage = rev(percentage),
         colours_text = case_when(
           category == "Black" ~ "ivory",
           category == "Brown" ~ "#dc143c",
           category == "Yellow" ~ "#000000"
         ))

# Plot 

p <- ggplot(data5, aes(x = 1, y = percentage)) +
  geom_col(aes(fill = category)) +
  geom_text(aes(x = 1, y = percentage, label = paste0(percentage, "%"),
                colour = category), family = "ps", size = 10,
            position = position_stack(vjust = 0.5)) +
  scale_colour_manual(values = c("Black" = "ivory",
                                 "Brown" = "#dc143c",
                                 "Yellow" = "#000000")) +
  scale_fill_manual(values = c("Black" = "#000000",
                               "Brown" = "#654321",
                               "Yellow" = "#ffd700")) +
  annotate("text", x = 0, y = 90, label = "BLACK.", family = "ps",
           color = "#000000", size = 12, hjust = 0, vjust = 0) +
  annotate("text", x = 0.15, y = 85, 
           label = "I.E. FULL-BLOODED\nNEGROES.", family = "ps",
           color = "#000000", size = 5, hjust = 0, vjust = 0) +
  
  
  annotate("text", x = 0, y = 48, label = "BROWN.", family = "ps",
           color = "#000000", size = 12, hjust = 0, vjust = 0) +
  annotate("text", x = 0.15, y = 36, 
           label = "I.E. PERSONS WITH\nSOME WHITE BLOOD\nOR DESCENDANTS\nOF LIGHT COLORED\nAFRICANS.", 
           family = "ps",
           color = "#000000", size = 5, hjust = 0, vjust = 0) +
  
  
  annotate("text", x = 0, y = 12, label = "YELLOW.", family = "ps",
           color = "#000000", size = 12, hjust = 0, vjust = 0) +
  annotate("text", x = 0.15, y = 5, 
           label = "I.E. PERSONS WITH\nMORE WHITE THAN\nNEGRO BLOOD.", 
           family = "ps",
           color = "#000000", size = 5, hjust = 0, vjust = 0) +
  coord_cartesian(xlim = c(0, 1.5)) +
  labs(title = "RACE AMALGAMATION IN GEORGIA .",
       subtitle = "\nBASED ON A STUDY OF 40,000 INDIVIDUALS OF NEGRO DESCENT.",
       caption = "#DuboisChallenge24| Week 5 | Prepared by C. YAZICI") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA), 
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.caption = element_text(family = "ps", hjust = 1, 
                                    size = 20),
        plot.title = element_text(family = "ps", hjust = 0.5, 
                                  size = 32, face = "bold"),
        plot.subtitle = element_text(family = "ps", hjust = 0.5, 
                                  size = 20),
        plot.margin = margin(0.8, 3.7, 0.8, 3.7, "cm")
        )

# Save the Plot

ggsave("Week5.png", p, width = 15, height = 18, dpi = 72)



  



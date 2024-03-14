library(tidyverse)
library(janitor)
library(ggpattern)
library(showtext)

# Font in the Plot

font_add_google('Public Sans', 'ps')
showtext_auto()

# Data Import & Wrangling

data6 <- read_csv("data.csv") %>%
  clean_names() %>%
  add_row(year = c(1800, 1840),
          negroes = c(1014077, 2535192),
          mulattoes = c(0, 205807)) %>%
  pivot_longer(-year, names_to = "population", 
               values_to = "values") %>%
  group_by(year) %>%
  mutate(sum = sum(values),
         percent = round(values / sum * 100)) %>%
  arrange(year) %>%
  ungroup()

# Preparation for the first yellow region 

yellow_data <- tibble(x = c(1800, 1890),
                         y = c(0, -1014077))

# Preparation for the second yellow region 

yellow2_data <- tibble(x = c(1800, 1890),
                      y = c(0, -2028154))

# # Preparation for the last region 

stripe_data <- tibble(x = c(1890, 1800, 1890, 1800),
                       y = c(0, -3028154, -3028154, 0)) 

# Title of the plot

title_text <- "The Amalgamation of the White and Black elements of the population\nin the United States."

# The Plot

p <- ggplot(data6) +
  # Black Region of the Plot
  geom_line(aes(x = year, y = sum)) +
  # geom_hline(yintercept = 1014077) +
  geom_area(aes(year, sum), fill = "black") +
  # Brown Region of the Plot
  geom_line(data = data6 %>%
              filter(population == "mulattoes"),
            mapping = aes(x = year, y = values), color = "#654321") +
  geom_area(data = data6 %>%
              filter(population == "mulattoes"),
            mapping = aes(year, values), fill = "#654321") +
 
  # Yellow Part of the Plot
  geom_segment(aes(x = 1800, xend = 1890,
               y = 0, yend = -1014077), color = "black")  +
  geom_area_pattern(data = stripe_data, 
                    mapping = aes(x = x, y = y), fill = "#e7d6c5",
                    pattern = 'stripe') +
  geom_area_pattern(data = yellow2_data, 
            mapping = aes(x = x, y = y), fill = "#f5e592",
            pattern = 'stripe') + 
  geom_area(data = yellow_data, 
               mapping = aes(x = x, y = y), fill = "#ffd700",
            color = "black") +
  geom_segment(aes(y = -3028154, yend = 1014077,
                   x = 1800, xend = 1800), color = "grey80", 
               linewidth = 1.5) +
  geom_segment(aes(y = -3028154, yend = 2740999,
                   x = 1840, xend = 1840), color = "grey80", 
               linewidth = 1.5) +
  geom_segment(aes(y = -3028154, yend = 3953760,
                   x = 1860, xend = 1860), color = "grey80", 
               linewidth = 1.5) +
  geom_text(data = data6 %>% filter(year != 1890),
            mapping = aes(x = year, y = sum + 500000, label = year),
            size = 8, hjust = 0.5, family = "ps", fontface = "bold") +
  geom_text(data = data6 %>% filter(year >= 1860 &
                                      year <= 1890,
                                    population == "negroes"),
            mapping = aes(x = year - 3, y = sum / 2, 
                          label = format(values, big.mark = ".",
                                  scientific = FALSE)),
            size = 8, color = "ivory", hjust = 0.5, family = "ps") +
  geom_text(data = data6 %>% filter(year >= 1860 &
                                      year <= 1890,
                                    population == "mulattoes"),
            mapping = aes(x = year - 3, y = 0, 
                          label = format(values, big.mark = ".",
                                        scientific = FALSE)),
            size = 8, color = "grey10", hjust = 0.5, family = "ps") +
  scale_y_reverse() +
  scale_x_reverse(breaks = c(1890, 1860, 1840, 1800),
                  labels = c("1890", "1860", "1840", "1800")) +
  annotate("text", x = 1863, y = 1971074, label = "90%",
           size = 7, color = "ivory", hjust = 0.5, family = "ps",
           fontface = "bold") +
  annotate("text", x = 1872, y = 1971074, label = "BLACK",
           size = 7, color = "ivory", hjust = 0.5, family = "ps",
           fontface = "bold") +
  annotate("text", x = 1863, y = 0, label = "10%",
           size = 7, color = "grey10", hjust = 0.5, family = "ps",
           fontface = "bold") +
  annotate("text", x = 1872, y = 0, label = "MIXED\nRACE",
           size = 7, color = "grey10", hjust = 0.5, family = "ps",
           fontface = "bold") +
  annotate("text", x = 1872, y = -2228154, label = "WHITES",
           size = 7, color = "grey10", hjust = 0.5, family = "ps",
           fontface = "bold") +
  # x axis
  annotate("text", x = 1892, y = 7600000, label = "1890",
           size = 9, color = "grey10", hjust = 0, family = "ps",
           fontface = "bold") +
  annotate("text", x = 1892, y = 3725522, label = "85%",
           size = 7, color = "grey10", hjust = 0.5, family = "ps",
           fontface = "bold") +
  annotate("text", x = 1892, y = 0, label = "15%",
           size = 7, color = "grey10", hjust = 0.5, family = "ps",
           fontface = "bold") +
 # The first line
  geom_segment(aes(x = 1770, xend = 1770, y = 1150000, yend = 3900000),
               color = "grey50", linewidth = 1.5) +
  annotate("text", x = 1775, y = 7000000, 
           label = "Amalgamation des elements blancs et noirs parmi la population Americaine.",
           hjust = 0, size = 8) +
  # The second line
  geom_segment(aes(x = 1780, xend = 1780, y = 1150000, yend = 3900000),
               color = "grey50", linewidth = 1.5) +
  annotate("text", x = 1785, y = 2200000, 
           label = "Done by Atlanta University.",
           hjust = 0.5, size = 5.5) +
  labs(title = title_text,
       caption = "#DuboisChallenge24 | Week 6 | Prepared by C. YAZICI") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5, size = 30, vjust = 0),
        plot.caption = element_text(hjust = 1, size = 20),
        panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA), 
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = margin(0.8, 2.7, 0.8, 2.7, "cm"))

# Save the Plot

ggsave("Week6.png", p, width = 20, height = 22, dpi = 72)


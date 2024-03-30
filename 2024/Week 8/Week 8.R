library(tidyverse)
library(readr)
library(janitor)
library(showtext)
library(gridExtra)
library(cowplot)

# Font in the Plot

font_add_google('Public Sans', 'ps')
showtext_auto()

# Data Import & Wrangling

data8 <- read_csv("data.csv") %>%
  clean_names()
data_1860 <- data8[1,]
data_1890 <- data8[3,] 
colnames(data_1890) <-  c("year",  "owners", "tenants")

str(data_1890)

# Data Preparation for  1890

data_1890 <- data_1890 %>%
  pivot_longer(-year, names_to = "category", values_to = "values") %>%
  mutate(values = as.numeric(values))

# The Plot of 1890

p1 <- ggplot(data_1890, aes(x = 1.6, y = values, fill = category)) +
  geom_col(position = "fill", width = 0.4) + 
  annotate("text", x = 1.6, y = 0.405, 
           label = "81%\nTENANTS\nMETAYERS", hjust = 0.5, 
           vjust = 0.5, fontface = "bold", family = "ps",
           size = 5) +
  annotate("text", x = 1.6, y = 0.905, 
           label = "19%\nPEASANT PROPRIETORS\nPAYSANS PROPRIETAIRES",
           hjust = 0.5, vjust = 0.5, fontface = "bold",
           family = "ps", size = 5) +
  annotate("text", x = 1.6, y = 1.05, 
           label = "1890",
           hjust = 0.5, vjust = 0.5, fontface = "bold", 
           family = "ps", size = 12) +
  annotate("text", x = 0, y = 0.92, 
           label = "IN 1890 NEARLY ONE FIFTH OF THEM OWNED THEIR OWN HOMES AND FARMS.\nTHIS ADVANCE WAS ACCOMPLISHED ENTIRELY WITHOUT STATE AID, AND IN THE\nFACE OF PROSCRIPTIVE LAWS.",
           hjust = 0, vjust = 0.5,
           family = "ps", size = 6) +
  annotate("text", x = 0, y = 0.71, 
           label = "EN 1890 ENVIRON UN CINQUIÈME ÉTAIENT PROPRIÉTAIRES DELEURS HAB-\nITATIONS ET DE LEURS FERMES. CE PROGRÈS S'EST ACCOMPLI SANS\nSECOURS AUCUN DE L'ETAT ET EN PRÉSENCE DE LOIS DÉFAVORABLES.",
           hjust = 0, vjust = 0.5,
           family = "ps", size = 6) +
  annotate("text", x = 0.05, y = 0.40, 
           label = "IN 1860 NEARLY 90% OF THE AFRICAN PEOPLE WERE SLAVES.",
           hjust = 0, vjust = 0.5,
           family = "ps", size = 6) +
  annotate("text", x = 0.05, y = 0.25, 
           label = "EN 1860, PRÈS DE 90 % DE LA POPULATION AFRICAINE ÉTAIT DES ESCLAVES.",
           hjust = 0, vjust = 0.5,
           family = "ps", size = 6) +
  scale_fill_manual(values = c("#dc143c", "#238123")) +
  scale_x_continuous(limits = c(0, 1.8)) +
  labs(title = "THE RISE OF THE BLACK PEOPLE FROM SLAVERY TO FREEDOM IN ONE GENERATION.\n\n PROGRÉS GRADUEL DES NOIRS DE L'ESCLAVAGE À LA LIBERTÉ EN UNE GÉNÉRATION.\n\n",
  subtitle = "DONE BY ATLANTA UNIVERSITY.") +
  coord_cartesian(ylim = c(0, 1.1)) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA), 
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(family = "ps", size = 25, 
                                  hjust = 0.5, face = "bold",
                                  margin = unit(c(1, 0.5, 0.6, 0.5),
                                                "cm")),
        plot.subtitle = element_text(family = "ps", size = 20, 
                                  hjust = 0.5, face = "bold"),
        plot.margin = unit(c(0.6, 0, 0.6, 0), "cm"))


str(data_1860)

# Data Preparation for  1860

data_1860 <- data_1860 %>%
  pivot_longer(-year, names_to = "category", values_to = "values") %>%
  mutate(values = as.numeric(values))

# The Plot of 1860

p2 <- ggplot(data_1860, aes(x = 2, y = values, fill = category)) +
  geom_col(position = "fill", width = 0.2) +
  annotate("text", x = 2, y = 0.8, 
           label = "89%\nSLAVES\nESCLAVES", hjust = 0.5, 
           vjust = 0.5, fontface = "bold", family = "ps",
           color = "#dc143c", size = 6) +
  annotate("text", x = 1.92, y = 0.94, 
           label = "11%", hjust = 0.5, 
           vjust = 0.5, fontface = "bold", family = "ps",
           color = "#000000", size = 8) +
  annotate("text", x = 2, y = 0.94, 
           label = "FREE LABORERS\nOUVRIERS LIBRES", hjust = 0.5, 
           vjust = 0.5, fontface = "bold", family = "ps",
           color = "#000000", size = 6) +
  annotate("text", x = 2, y = 1.02, 
           label = "1860", hjust = 0.5, 
           vjust = 0.5, fontface = "bold", family = "ps",
           color = "#000000", size = 12) +
  scale_fill_manual(values = c("#238123", "#000000")) +
  scale_x_continuous(limits = c(1.6, 2.4)) +
  coord_cartesian(ylim = c(0.7, 1.1)) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA), 
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0, 0, 1.9, 0), "cm"))


# The Final Plot

p <- grid.arrange(p1, p2, nrow = 2)


final_plot <- ggdraw(p) +
  theme(plot.background = element_rect(fill = "#e7d6c5", 
                                       color = "#e7d6c5"),
        plot.margin = unit(c(0.1, 0.5, 0.8, 0.5), "cm")) +
  draw_line(x = c(0.61, 0.755), 
          y = c(0.36, 0.775), colour = "grey50", size = 0.8,
          linetype = "dashed") +
  draw_line(x = c(0.615, 0.755), 
            y = c(0.36, 0.68), colour = "grey50", size = 0.8,
            linetype = "dashed") +
  draw_line(x = c(0.615, 0.755), 
            y = c(0.29, 0.62), colour = "grey50", size = 0.8,
            linetype = "dashed") +
  draw_line(x = c(0.615, 0.755), 
            y = c(0.29, 0.58), colour = "grey50", size = 0.8,
            linetype = "dashed") +
  draw_line(x = c(0.615, 0.755), 
            y = c(0.25, 0.53), colour = "grey50", size = 0.8,
            linetype = "dashed") +
  
  
 
  # Caption
  draw_label("#DuboisChallenge24| Week 8 | Prepared by C. YAZICI",
             x = 0.95, y = 0.01, size = 25,
             fontfamily = "ps", fontface = "bold", hjust = 1) 
 
  

  
  





ggsave("Week8.png", final_plot, width = 22, height = 26, dpi = 72)



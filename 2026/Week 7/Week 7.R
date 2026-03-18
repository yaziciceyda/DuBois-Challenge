library(tidyverse)
library(readr)
library(janitor)
library(showtext)
library(cowplot)
library(gridExtra)
library(stringr)
library(ggstar)
library(ggforce)

# Font in the Plot

font_add_google('Public Sans', 'ps')
showtext_auto()

data7 <- read.csv("data.csv") %>%
  clean_names() %>%
  arrange(desc(percentage)) %>%
  mutate(occupation = factor(occupation, 
                             levels = c("Teachers", "House Wives",
                                        "Other Professions", 
                                        "Government Service",
                                        "Business", "Ministers")))
                                    
legend_data <- tibble(occupation = c("Teachers", "Ministers",
                                     "Government Service",
                                      "Business",  "Other Professions",
                                      "House Wives"),
                      french_occup = c("PROFESSEURS ET INSTITUTEURS",
                                       "MINISTRES DE L'EVANGLE",
                                       "EMPLOYES DU GOVERNMENT",
                                       "MARCHANDS",
                                       "MEDONS,ADVOCATS,ETUDIANTS",
                                       "MERES DE FAMILLE"),
                                            x = 0,
                      y = c(6:1)) %>%
  mutate(occupation = factor(occupation, 
                             levels = c("Teachers", "House Wives",
                                        "Other Professions", 
                                        "Government Service",
                                        "Business", "Ministers")))


# Pie Chart


p1 <-  ggplot(data7, aes(x = "", y = percentage, fill = occupation)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(percentage, "%")),
            position = position_stack(vjust = 0.45),
            family = "ps", fontface = "bold", size = 6) +
  coord_polar("y", start =  2*pi/3) +
  scale_fill_manual(values = c("#dc143c", "#ffd700",
                                "grey50", "#ffc0cb",
                                  "#d2b48c", "#4682b4")) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())



p2 <- ggplot(legend_data) +
  geom_circle(aes(x0 = x, y0 = y, r = 0.3, fill = occupation)) +
  geom_text(aes(x = x + 0.5, y = y, label = toupper(occupation)),
            family = "ps", hjust = 0, size = 6) +
  scale_fill_manual(values = c("#dc143c", "#ffd700",
                               "grey50", "#ffc0cb",
                               "#d2b48c", "#4682b4")) +
  scale_x_continuous(expand = c(0, 0), limits = c(-1, 4)) +
  coord_fixed() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())


p3 <- ggplot(legend_data) +
  geom_circle(aes(x0 = x, y0 = y, r = 0.3, fill = french_occup)) +
  geom_text(aes(x = x - 0.5, y = y, label = toupper(french_occup)),
            family = "ps", hjust = 1, size = 6) +
  scale_fill_manual(values = c("#dc143c", "#ffd700",
                               "grey50", "#ffc0cb",
                               "#d2b48c", "#4682b4")) +
  scale_x_continuous(expand = c(0, 0), limits = c(-5.5, 0.5)) +
  coord_fixed() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())


################# MAP

usa_map <- map_data("state") %>%
  mutate(region = str_to_title(region),
         color_state = case_when(
           region == "Alabama" ~ "#dc143c",
           region ==   "Arizona" ~ "grey80",
           region ==   "Arkansas" ~ "grey50",
           region ==   "California" ~ "#dc143c",
           region ==   "Colorado" ~ "#dc143c",
           region ==   "Connecticut" ~ "#ffd700",
           region ==   "Delaware" ~ "#dc143c",
           region ==   "Florida" ~ "grey50",
           region ==   "Georgia" ~ "black",
           region ==   "Idaho" ~ "#ffc0cb",
           region ==   "Illinois" ~ "ivory",
           region ==   "Indiana" ~ "#ffc0cb",
           region ==   "Iowa" ~ "grey50",
           region ==   "Kansas" ~ "ivory",
           region ==   "Kentucky" ~ "#654321",
           region ==   "Louisiana" ~ "sandybrown",
           region ==   "Maine" ~ "#00aa00",
           region ==   "Maryland" ~ "#654321",
           region ==   "Massachusetts" ~ "#dc143c",
           region ==   "Michigan" ~ "navyblue",
           region ==   "Minnesota" ~ "blue",
           region ==   "Mississippi" ~ "blue",
           region ==   "Missouri" ~ "#dc143c",
           region ==   "Montana" ~ "grey80",
           region ==   "Nebraska" ~ "#ffd700",
           region ==   "Nevada" ~ "grey60",
           region ==   "New Hampshire" ~ "#ffd700",
           region ==   "New Jersey" ~ "grey50",
           region ==   "New Mexico" ~ "grey50",
           region ==   "New York" ~ "ivory",
           region ==   "North Carolina" ~ "ivory",
           region ==   "North Dakota" ~ "ivory",
           region ==   "Ohio" ~ "blue",
           region ==   "Oklahoma" ~ "ivory",
           region ==   "Oregon" ~ "grey50",
           region ==   "Pennsylvania" ~ "navyblue",
           region ==   "Rhode Island" ~ "#ffd700",
           region ==   "South Carolina" ~ "#ffd700",
           region ==   "South Dakota" ~ "#654321",
           region ==   "Tennessee" ~ "navyblue",
           region ==   "Texas" ~ "#ffd700",
           region ==   "Utah" ~ "#654321",
           region ==   "Vermont" ~ "blue",
           region ==   "Virginia" ~ "blue",
           region ==   "Washington" ~ "#ffd700",
           region ==   "West Virginia" ~ "ivory",
           region ==   "Wisconsin" ~ "#ffd700",
           region ==   "Wyoming" ~ "navyblue"
         ),
         region = str_to_lower(region))


# Plot the map with matching state names
p_map <- ggplot() +
  geom_polygon(data = usa_map, aes(x = long, y = lat, 
                                   group = group, 
                                   fill = color_state)) +
  geom_star(aes(x = -83.5, y = 32), fill = "ivory", size = 4) +
  geom_point(aes(x = -84.5, y = 34), color = "ivory", size = 4) +
  scale_fill_identity() +
  #  coord_sf(xlim = c(-153, -40)) 
  coord_sf(xlim = c(-173, 2),
           ylim = c(0, 112)) +
  annotate("text", x = -178, y = 40,
           label = "PREPARED AND EXECUTED BY\nBLACK STUDENTS UNDER THE\n                DIRECTION OF\n    ATLANTA UNIVERSITY\n             ATLANTA,GA.\n UNITED STATES OF AMERICA.",
           hjust = 0, family = "ps", size = 7) +
  annotate("rect", xmin = -126, xmax = -120,
           ymin = 15, ymax = 25, fill = "black") +
  annotate("point", x = -123, y = 23.5, color = "ivory", size = 8) +
  annotate("star", x = -123, y = 18, fill = "ivory", size = 8) +
  annotate("text", x = -118, y = 20,
           label = "CENTRE OF BLACK POPULATION.\nATLANTA UNIVERSITY.",
           hjust = 0, family = "ps", size = 7) +
  annotate("text", x = -66, y = 38, 
           label = "PREPAREES ET EXECUTEES PAR\nDES ETUDIANTS NOIRS SOUS\nLA DIRIECTION DE L'UNIVERSITE\n             D'ATLANTA.\n         ETAT DE GEORGIE.\n   ETATS UNIS D'AMERIQUE.", 
           hjust = 0, family = "ps", color = "#dc143c", size = 7) +
  annotate("text", x = -160, y = 12,
           label = "THE UNIVERSITY WAS FOUNDED IN 1867. IT HAS INSTRUCTED 6000 BLACK STUDENTS.",
           hjust = 0, family = "ps", size = 7) +
  annotate("text", x = -166, y = 8,
           label = " L'UNIVERSITE A ETE FONDEE EN 1867. ELLE A DONNE L'INSTRUCTION A'6000 ETUDIANTS NOIRS.",
           hjust = 0, family = "ps", size = 7, color = "#dc143c") +
  annotate("text", x = -134, y = 4,
           label = "IT HAS GRADUATED 330 BLACKS AMONG WHOM ARE:",
           hjust = 0, family = "ps", size = 7) +
  annotate("text", x = -136, y = 0,
           label = "ELLE A DELIVRE DES DIPLOMES A 330 NOIRS DONT :",
           hjust = 0, family = "ps", size = 7, color = "#dc143c") +
  annotate("text", x = -172, y = 70,
           label = "       UNE SERIE DE CARTES ET DIAGRAMMES STATISTIQUES MONTRANT LA\nCONDITION PRESENTE DES DESCENDANTS DES ANCIENS ESCLAVES AFRI-\nCAINS ACTUELLMENT ETABLIS DANS LES ETATS UNIS D AMERIQUE.",
           hjust = 0, family = "ps", size = 10, color = "#dc143c") +
  annotate("text", x = -160, y = 99,
           label = "      A SERIES OF STATISTICAL CHARTS, ILLUSTRA-\nTING THE CONDITION OF THE DESCENDANTS OF FOR-\nMER AFRICAN SLAVES NOW RESIDENT IN THE UNITED\nSTATES OF AMERICA.",
           hjust = 0, family = "ps", size = 12, fontface = "bold") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0.1, 0, 0.1, 0), "cm"))


# The Plots together

p <- grid.arrange(p2, p1, p3, nrow = 1, ncol = 3,  
                widths = c(0.4, 0.5, 0.4))


# The final plot

final_plot1 <-  ggdraw(p) +
  theme(plot.background = element_rect(fill = "#e7d6c5", 
                                       color = "#e7d6c5"),
        plot.margin = unit(c(-10, 4, 0.1, 4), "cm")) +
draw_label("THE UNIVERSITY HAS 20 PROFESSORS AND INSTRUCTORS AND 250 STUDENTS AT PRESENT.\n
IT HAS FIVE BUILDINGS, 60 ACRES OF CAMPUS, AND A LIBRARY OF 11,000 VOLUMES. IT AIMS TO RAISE\n
AND CIVILIZE THE SONS OF THE FREEDMEN BY TRAINING THEIR MORE CAPABLE MEMBERS IN THE LIBER-\n
AL ARTS ACCORDING TO THE BEST STANDARDS OF THE DAY.\n
THE PROPER ACCOMPLISHMENT OF THIS WORK DEMANDS AN ENDOWMENT FUND OF $500,000.\n
L' UNIVERSITE A ACTUELLEMENT 20 PROFESSEURS ET INSTRUCTEURS ET 250 ETUDIANTS.\n
ELLE EST COMPOSEE DE CINC BATIMENTS, 60 ACRES(ENVIRON 26 HECTARES)DE TERRAIN SERVANT DE\n
COUR ET DE CHAMP DE RECREATION, ET DUNE BIBLIOTHEQUE CONTENANT 11,000 VOLUMES.\n
SON BUT EST O'ELEVER ET DE CIVILISER LES FILS DES NOIRS AFFRANCHIS EN DONNANT AUX MIEUX\n
DOUES UNE EDUCATION DANS LES ARTS LIBERAUX EN ACCORD AVEC LES IDEES LES PLUS PROGESS-\n
SISTES DE L'EPOQUE.\n
L'ACCOMPLISSEMENT DE CETTE OEUVRE DEMANDE UNE DOTATION DE $500,000 (2,500,000 FRANCS).", 
             x = 0.09, y = 0.08,
             fontfamily = "ps", hjust = 0, 
             size = 20)


plots <- align_plots(p_map, final_plot1,  align = 'r', axis = 'r')


caption <- ggdraw() + 
  draw_label(
    "#DuboisChallenge26| Week 7 | Prepared by C. YAZICI", 
    fontface = 'bold',
    fontfamily = "ps",
    hjust = 1,
    x = 0.9,
    y = 0.1, 
    size = 24,
    color = "black"
  ) 

bottom_row <- plot_grid(
  plots[[1]], plots[[2]],
  labels = "",
  rel_heights = c(0.5, 0.5), 
  ncol = 1
)

final_plot <- plot_grid( bottom_row, caption, labels = "", ncol = 1,
                         rel_heights = c(0.9, 0.1)) +
  theme(plot.background = element_rect(fill = "#e7d6c5", colour = "#e7d6c5"),
        plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"))



# Save the Plot

ggsave("Week 7.png", final_plot, width = 25, height = 34, dpi = 72)



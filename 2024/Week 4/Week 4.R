library(tidyverse)
library(readr)
library(janitor)
library(ggstar)
library(sf)
library(cowplot)
library(showtext)
library(gridExtra)
library(rnaturalearth)
library(geomtextpath)

# Font in the Plot

font_add_google('Public Sans', 'ps')
showtext_auto()

route_pairs <- read.csv("route-pairs.csv") %>%
  clean_names()

routes <- read.csv("routes.csv")

# The World data 

world <- sf::st_as_sf(countries110)
world <- world %>% clean_names()

# Africa Data

africa <- dplyr::filter(world, region_un == 'Africa')%>%
  st_transform(crs = "+proj=laea +lon_0=18.984375")

# America Data

america <- dplyr::filter(world, subregion %in% c("Northern America",
                                                 "South America")) %>%
  st_transform(crs = "+proj=laea +lon_0=18.984375")


# The world data 

worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')


america <- worldmap[worldmap$continent == 'North America' |
                     worldmap$continent == 'South America'  ,]
# USA

usa <- ne_states(iso_a2 = 'us', returnclass = "sf")
usa_col <- rep("#e3b425", length(usa$name))
usa_col[which(usa$name %in% c("Florida", "Georgia", "Alabama", 
                              "Mississippi", "South Carolina",
                              "Louisiana"))] <- "black"

usa_col[which(usa$name %in% c("Tennessee", "North Carolina", 
                              "Arkansas", "Louisiana",
                              "Texas", "Oklahoma"))] <- "#654321"
world2 <- ne_states(returnclass = "sf")

# Mexico

mexico <-  ne_states(iso_a2 = 'MX', returnclass = "sf")
mexico_col <- rep("#e3b425", length(mexico$name))
mexico_col[which(mexico$name %in% c("Coahuila", "Nuevo León",
                              "Tamaulipas", "Veracruz",
                              "Tabasco", "Campeche",
                              "Yucatán", "Quintana Roo"))] <- "#654321"
# Belize

belize <-  ne_states(iso_a2 = 'BZ', returnclass = "sf")
belize_col <- rep("#e3b425", length(belize$name))

# Guatemala

guatemala <-  ne_states(iso_a2 = 'GT', returnclass = "sf")
guatemala_col <- rep("#e3b425", length(guatemala$name))
guatemala_col[which(guatemala$name %in% c("Izabal"))] <- "#654321"


# Honduras

honduras <-  ne_states(iso_a2 = 'HN', returnclass = "sf")
honduras_col <- rep("#e3b425", length(honduras$name))
honduras_col[which(honduras$name %in% c("Cortés", "Atlántida",
                        "Colón", "Gracias a Dios"))] <- "#654321"

# Nicaragua

nigaragua <-  ne_states(iso_a2 = 'NI', returnclass = "sf")
nigaragua_col <- rep("#e3b425", length(nigaragua$name))
nigaragua_col[which(nigaragua$name %in% c("Atlántico Norte", 
                                          "Atlántico Sur",
                               "Rio San Juan"))] <- "#654321"

# Costa Rica

costa_rica <-  ne_states(iso_a2 = 'CR', returnclass = "sf")
costa_rica_col <- rep("#e3b425", length(costa_rica$name))
costa_rica_col[which(costa_rica$name %in% c("Heredia", 
                                          "Limón",
                                          "Rio San Juan"))] <- "#654321"

# Panama

panama <-  ne_states(iso_a2 = 'PA', returnclass = "sf")
panama_col <- rep("#e3b425", length(panama$name))
panama_col[which(panama$name %in% c("Bocas del Toroa", 
                                            "Ngöbe Buglé",
                                            "Colón",
                                            "Kuna Yala"))] <- "#654321"

# Colombia

colombia <-  ne_states(iso_a2 = 'CO', returnclass = "sf")
colombia_col <- rep("#e3b425", length(colombia$name))
colombia_col[which(colombia$name %in% c("Córdoba", 
                                    "Sucre",
                                    "Bolívar",
                                    "Atlántico",
                                    "Magdalena",
                                    "La Guajira",
                                    "Cesar"))] <- "#654321"

# Venezuela

venezuela <-  ne_states(iso_a2 = 'VE', returnclass = "sf")
venezuela_col <- rep("#e3b425", length(venezuela$name))
venezuela_col[which(venezuela$name %in% c("Zulia", "Trujillo",
                                        "NA", "Falcón",
                                        "Distrito Capital",
                                        "Vargas", "Yaracuy",
                                        "Carabobo", "Aragua",
                                        "Distrito Capital",
                                        "Miranda", "Sucre",
                                        "Delta Amacuro",
                                        "Mérida", "Trujillo",
                                        "Lara", "Cojedes",
                                        "Guárico", "Anzoátegui",
                                        "Monagas",
                                        "Barinas",
                                        "Portuguesa",
                                        "Bolívar",
                                        "Apure"))] <- "#654321"

# Guyana

guyana <-  ne_states(iso_a2 = 'GY', returnclass = "sf")
guyana_col <- rep("#e3b425", length(guyana$name))
guyana_col <- "#654321"


# Suriname

suriname <-  ne_states(iso_a2 = 'SR', returnclass = "sf")
suriname_col <- rep("#e3b425", length(suriname$name))
suriname_col <-"#654321"

                                       
# French Guiana

fr_guiana <- ne_states(iso_a2 ='FR', returnclass = "sf") %>%
  filter(gu_a3 == "GUF")
fr_guiana_col <- rep("#e3b425", length(fr_guiana$name))
fr_guiana_col <-"#654321"

# Brazil

brazil <- ne_states(iso_a2 ='BR', returnclass = "sf") 
brazil_col <- rep("#e3b425", length(brazil$name)) 
brazil_col[which(brazil$name %in% c("Roraima", "Amapá",
                                    "Pará", "Mato Grosso",
                                    "Mato Grosso do Sul",
                                    "São Paulo", "Paraná",
                                    "Santa Catarina",
                                    "Rio Grande do Sul"))] <- "#654321"

brazil_col[which(brazil$name %in% c("Maranhão", "Piauí",
                                    "Ceará", "Rio Grande do Norte",
                                    "Paraíba", "Pernambuco",
                                    "Alagoas", "Sergipe",
                                    "Tocantins", "Bahia",
                                    "Goiás", "Distrito Federal",
                                    "Minas Gerais",
                                    "Espírito Santo",
                                    "Rio de Janeiro"))] <- "black"
# Cuba

cuba <- ne_states(iso_a2 = 'CU', returnclass = "sf") 
cuba_col <- rep("#e3b425", length(cuba$name)) 
cuba_col <-"black"


# Haiti

haiti <- ne_states(iso_a2 = 'HT', returnclass = "sf") 
haiti_col <- rep("#e3b425", length(haiti$name)) 
haiti_col <-"black"

# Dominican Republic

d_rep <- ne_states(iso_a2 = 'DO', returnclass = "sf") 
d_rep_col <- rep("#e3b425", length(d_rep$name)) 
d_rep_col <-"black"


# Puerto Rico

p_rico <- ne_states(iso_a2 = 'PR', returnclass = "sf") 
p_rico_col <- rep("#e3b425", length(p_rico$name)) 
p_rico_col <-"black"

# Jamaica

jamaica <- ne_states(iso_a2 = 'JM', returnclass = "sf") 
jamaica_col <- rep("#e3b425", length(jamaica$name)) 
jamaica_col <-"black"

# The data to create the circle with the label

data_circle <- tibble(x = -80 + 105 * cos(seq(0, 2 * pi, 
                                              length.out = 100)),
                       y = 15 + 105 * sin(seq(0, 2 * pi, 
                                              length.out = 100)))
p1 <- ggplot() + 
  geom_sf(data = america, fill = "#e3b425", colour = NA) + 
  geom_sf(data = usa, fill = usa_col, colour = NA) +
  geom_sf(data = mexico, fill = mexico_col, colour = NA) +
  geom_sf(data = belize, fill = belize_col, colour = NA) +
  geom_sf(data = guatemala, fill = guatemala_col, colour = NA) +
  geom_sf(data = honduras, fill = honduras_col, colour = NA) +
  geom_sf(data = nigaragua, fill = nigaragua_col, colour = NA) +
  geom_sf(data = costa_rica, fill = costa_rica_col, colour = NA) +
  geom_sf(data = panama, fill = panama_col, colour = NA) +
  geom_sf(data = colombia, fill = colombia_col, colour = NA) +
  geom_sf(data = venezuela, fill = venezuela_col, colour = NA) +
  geom_sf(data = guyana, fill = guyana_col, colour = NA) +
  geom_sf(data = suriname, fill = suriname_col, colour = NA) +
  geom_sf(data = fr_guiana, fill = fr_guiana_col, colour = NA) +
  geom_sf(data = brazil, fill = brazil_col, colour = NA) +
  geom_sf(data = cuba, fill = cuba_col, colour = NA) +
  geom_sf(data = haiti, fill = haiti_col, colour = NA) +
  geom_sf(data = d_rep, fill = d_rep_col, colour = NA) +
  geom_sf(data = p_rico, fill = p_rico_col, colour = NA) +
  geom_sf(data = jamaica, fill = jamaica_col, colour = NA) +
  geom_star(aes(x = -81.65, y = 30.33), size = 7, fill = "ivory") +
  coord_sf(xlim = c(28, -195), ylim = c(-90, 120), expand = FALSE) +
  geom_path(data = data_circle, aes(x = x, y = y)) +
  geom_labelpath(data = data_circle, aes(x = x, y = y,
              label = "DISTRIBUTION OF"), hjust = 0.11, vjust = 0,
              family = "ps", fill = "#e7d6c5",
              size = 8) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA),
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

# The Second Map


others <- worldmap[worldmap$continent != 'North America' &
                     worldmap$continent != 'South America'&
                     worldmap$continent !=  'Antarctica', ]


africa <- worldmap[worldmap$continent == 'Africa',]
africa_col <- rep("#e3b425", length(africa$name)) 
africa_col <-"black"

other_afr_countires <- africa[africa$admin %in% c('South Africa',
                      'Morocco', 'Algeria', 'Tunisia', 'Libya', 'Egypt',
                      'Western Sahara', 'Mauritania', 'Mali', 'Niger',
                      'Chad', 'Sudan', 'Eritrea'),]

other_afr_countires_col <- rep("#e3b425", 
                               length(other_afr_countires$name)) 
other_afr_countires_col <-"#654321"

# Madagascar

madagascar <- ne_states(iso_a2 = 'MG', returnclass = "sf") 
madagascar_col <- rep("#e3b425", length(madagascar$name)) 

# Lesotho

lesotho <- ne_states(iso_a2 = 'LS', returnclass = "sf") 
lesotho_col <- rep("#654321", length(lesotho$name)) 

# Swaziland

swaziland <- ne_states(iso_a2 = 'SZ', returnclass = "sf") 
swaziland_col <- rep("#654321", length(swaziland$name)) 

# Papua New Guinea

papua <- ne_states(iso_a2 = 'PG', returnclass = "sf") 
papua_col <- rep("#654321", length(papua$name)) 

# Indonesia

indonesia <- ne_states(iso_a2 = 'ID', returnclass = "sf") 
indonesia_col <- rep("#e3b425", length(indonesia$name)) 
indonesia_col[which(indonesia$name %in% c("Papua", 
                                          "Papua Barat"))] <- "#654321"

# Code snippet to check the map 

# ggplot() + 
#  geom_sf(data = australia, fill = australia_col) +
#  geom_text(data = australia, aes(x = longitude,
# y = latitude, label = name))

# The data to create the circle with the label

data_circle2 <- tibble(x = 80 + 116 * cos(seq(0, 2 * pi, 
                                              length.out = 100)),
                       y = 15 + 116 * sin(seq(0, 2 * pi, 
                                              length.out=100)))

p2 <- ggplot() + geom_sf(data = others,  fill = "#e3b425", colour = NA) + 
  geom_sf(data = africa, fill = africa_col, colour = NA) +
  geom_sf(data = other_afr_countires, fill = other_afr_countires_col, 
          colour = NA) +
  geom_sf(data = madagascar, fill = madagascar_col, colour = NA) +
  geom_sf(data = lesotho, fill = lesotho_col, colour = NA) +
  geom_sf(data = swaziland, fill = swaziland_col, colour = NA) +
  geom_sf(data = papua, fill = papua_col, colour = NA) +
  geom_sf(data = indonesia, fill = indonesia_col, colour = NA) +
  geom_curve(aes(x = 10, xend = -5, 
                 y = 3, yend = 37), curvature = -2, linewidth = 0.8) +
  geom_path(data = data_circle2, aes(x = x, y = y)) +
  geom_labelpath(data = data_circle2, aes(x = x, y = y,
                 label = "THE NEGRO RACE"), 
                 hjust = 0.35, vjust = 0,
                 family = "ps", fill = "#e7d6c5", size = 8) +
  coord_sf(xlim = c(-67, 220), ylim = c(-102, 135), expand = FALSE) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#e7d6c5", color = NA),
        plot.background = element_rect(fill = "#e7d6c5", color = NA), 
        panel.border = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0.6, 0.5, 0.6, 0.5), "cm"))

# The Plots together

p <- grid.arrange(p1, p2, nrow = 1, ncol = 2,  
                  widths = c(0.9, 1))
p

# The region in Australia

g1 <- grid::circleGrob(gp = grid::gpar(fill = "#654321",
                                       colour = "#654321"))
# The final plot

final_plot <- ggdraw(p) +
  theme(plot.background = element_rect(fill = "#e7d6c5", 
                                       color = "#e7d6c5"),
        plot.margin = unit(c(0.1, 0.5, 0.1, 0.5), "cm"),
        aspect.ratio = 9/9) +
  # The location of the region in Australia
  draw_grob(g1, x = 0.335, y = -0.075, scale = 0.015) +
  # Lines from bottom to top
  draw_line(x = c(0.32, 0.64), 
            y = c(0.45, 0.43), colour = "black", size = 0.8) +
  draw_line(x = c(0.255, 0.62), 
            y = c(0.51, 0.475), colour = "black", size = 0.8) +
  draw_line(x = c(0.24, 0.62), 
            y = c(0.53, 0.475), colour = "black", size = 0.8) +
  draw_line(x = c(0.24, 0.62), 
            y = c(0.54, 0.475), colour = "black", size = 0.8) +
  # The title, subtitle and the other text in the plot
  draw_label("THE GEORGIA NEGRO.", x = 0.5, y = 0.99,
            fontfamily = "ps", hjust = 0.5, fontface = "bold",
            size = 35) +
  draw_label("A SOCIAL STUDY\nBY\nW. E. BURGHARDT DU BOIS.",
             x = 0.5, y = 0.9,
             fontfamily = "ps", hjust = 0.5, fontface = "bold",
             size = 30) +
  draw_label("___", x = 0.35, y = 0.25,
             fontfamily = "ps", hjust = 0.5, size = 25) +
  draw_label("ROUTES OF THE AFRICAN SLAVE TRADE.", 
             x = 0.64, y = 0.25,
             fontfamily = "ps", hjust = 0.5, size = 25) +
  geom_star(aes(x = 0.35, y = 0.2), fill = "ivory", size = 8) +
  draw_label("THE STATE OF GEORGIA.", 
             x = 0.56, y = 0.2,
             fontfamily = "ps", hjust = 0.5, size = 25) +
  draw_label("THIS CASE IS DEVOTED TO A SERIES OF CHARTS , MAPS AND OTHER DEVI-\nCES DESIGNED TO ILLUSTRATE THE DEVELOPMENT OF THE AMERICAN NEGRO IN A\nSINGLE TYPICAL STATE OF THE UNITED STATES.", 
             x = 0.50, y = 0.13,
             fontfamily = "ps", hjust = 0.5, size = 25) +
  draw_label("THE PROBLEM OF THE 20TH CENTURY IS THE PROBLEM OF THE\nCOLOR-LINE.",
              x = 0.56, y = 0.05,
             fontfamily = "ps", hjust = 0.5, size = 25) +
  draw_label("#DuboisChallenge24| Week 4 | Prepared by C. YAZICI",
             x = 0.56, y = 0.001, size = 25,
             fontfamily = "ps", fontface = "bold", hjust = 0.48) 


# Save the Plot

ggsave("Week4.png", final_plot, width = 20, height = 22, dpi = 72)

  


  

mygrid <- getGridLayer(
  x = data_georgia, 
  cellsize = median(as.numeric(st_area(data_georgia))), 
  var = "acres_1899",
  type = "hexagonal"
)


png(file = "saving_plot2.png", width = 600, height = 550)

# plot municipalities (only the backgroung color is plotted)
plot(st_geometry(data_georgia), col = NA, border = NA, bg = "#e7d6c5")

# Plot the population density
choroLayer(x = mygrid, var = "acres_1899", method = "geom", nclass=10, 
           col = carto.pal(pal1 = "red.pal", n1 = 10), border = "grey80", 
           lwd = 0.5, legend.pos = "bottomleftextra", add = TRUE,
           legend.title.txt = "Land (in acres)")

layoutLayer(title = "LAND OWNED BY BLACK PEOPLE IN GEORGIA, U.S.A. 1870 - 1900.", 
            sources = "Sources: https://github.com/ajstarks/dubois-data-portraits",
            author = "#DuboisChallenge2025 | Week 3 | Prepared by C. YAZICI ", 
            frame = TRUE, north = FALSE, tabtitle = FALSE, scale = 100, 
            theme = "kaki.pal")

dev.off()


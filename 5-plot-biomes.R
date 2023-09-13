
require(magrittr)
require(tmap)

origbiomes2004 <- sf::read_sf("results/biomes-2004-before-intersec.gpkg") %>% dplyr::arrange(code_biome)
origbiomes2019 <- sf::read_sf("results/biomes-2019-before-intersec.gpkg") %>% dplyr::arrange(code_biome)

biomes2004 <- sf::read_sf("results/biomes-2004.gpkg")
biomes2019 <- sf::read_sf("results/biomes-2019.gpkg")

biomes <- c("Amazonia", "Caatinga", "Cerrado", "Mata Atlantica", "Pampa", "Pantanal")
origbiomes2004$biome <- biomes
origbiomes2019$biome <- biomes
biomes2004$biome <- biomes
biomes2019$biome <- biomes

colors = c("#b8fd00", "#fffdae", "#fdc5b0", "#d9fbab", "#fceed3", "#fee7ff")

orig2004 <- 
  tmap::tm_shape(origbiomes2004) +
  tmap::tm_polygons("biome", palette = colors) +
  tmap::tm_layout(legend.show = FALSE) +
  tmap::tm_credits("Biomes in 2004\nas defined by IBGE", position = "left", align = "left")

orig2019 <- tmap::tm_shape(origbiomes2019) +
  tmap::tm_polygons("biome", palette = colors) +
  tmap::tm_layout(legend.show = FALSE) +
  tmap::tm_credits("Biomes in 2019\nas defined by IBGE", position = "left", align = "left")

final2004 <- tmap::tm_shape(biomes2004) +
  tmap::tm_polygons("biome", palette = colors) +
  tmap::tm_layout(legend.show = FALSE) +
  tmap::tm_credits("Biomes in 2004\nafter processing", position = "left", align = "left")

final2019 <- tmap::tm_shape(biomes2019) +
  tmap::tm_polygons("biome", palette = colors) +
  tmap::tm_layout(legend.show = FALSE) +
  tmap::tm_credits("Biomes in 2019\nafter processing", position = "left", align = "left")

origbiomes2019$Biomes <- origbiomes2019$biome

legend.map <- 
  tmap::tm_shape(origbiomes2019) +
  tmap::tm_polygons("Biomes", palette = colors) +
  tm_layout(legend.only = TRUE, 
            legend.title.size = 1.5, frame.double.line = TRUE,
            legend.text.size = 1.1,
            legend.position = c("left","top"))

pdf("biomes-orig.pdf", width = 6, height = 3)
grid::grid.newpage()
page.layout <- grid::grid.layout(nrow = 1, ncol = 2)
grid::pushViewport(grid::viewport(layout = page.layout))

print(orig2004, vp=grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
print(orig2019, vp=grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()

pdf("biomes-final.pdf", width = 6, height = 3)
grid::grid.newpage()
page.layout <- grid::grid.layout(nrow = 1, ncol = 2)
grid::pushViewport(grid::viewport(layout = page.layout))

print(final2004, vp=grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
print(final2019, vp=grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()

pdf("biomes-legend.pdf", width = 2, height = 3)
print(legend.map)
dev.off()


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

origbiomes2019$Biomes <- origbiomes2019$biome
biomes2019$Biomes <- biomes2019$biome

orig2004 <- tmap::tm_shape(origbiomes2004) +
  tmap::tm_polygons("biome", palette = colors) +
  tmap::tm_credits("Biomes in 2004\nas defined by IBGE", size = 1.4, position = "left", align = "left") +
  tm_legend(
    scale = 0.4,
    legend.show = FALSE)

orig2019 <- tmap::tm_shape(origbiomes2019) +
  tmap::tm_polygons("Biomes", palette = colors) +
  tmap::tm_credits("Biomes in 2019\nas defined by IBGE", size = 1.4, position = "left", align = "left") +
  tm_legend(
    scale = 0.4,
    title.size = 1.5,
    text.size = 1.1,
    position = c("right","bottom"))

final2004 <- tmap::tm_shape(biomes2004) +
  tmap::tm_polygons("biome", palette = colors) +
  tmap::tm_credits("Biomes in 2004\nafter processing", size = 1.4, position = "left", align = "left") + tm_legend(#legend.only = TRUE, 
    scale = 0.4,
    show = FALSE)

final2019 <-  
  tmap::tm_shape(biomes2019) +
  tmap::tm_polygons("Biomes", palette = colors) +
  tmap::tm_credits("Biomes in 2019\nafter processing", size = 1.4, position = "left", align = "left") +
  tm_legend(
          scale = 0.4,
          title.size = 1.5,
          text.size = 1.1,
          position = c("right","bottom"))

pdf("biomes-orig.pdf", width = 6, height = 3)
grid::grid.newpage()
page.layout <- grid::grid.layout(nrow = 1, ncol = 2)
grid::pushViewport(grid::viewport(layout = page.layout))

print(orig2004, vp=grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
print(orig2019, vp=grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()

pdf("biomes-final2.pdf", width = 6, height = 3)
grid::grid.newpage()
page.layout <- grid::grid.layout(nrow = 1, ncol = 2)
grid::pushViewport(grid::viewport(layout = page.layout))

print(final2004, vp=grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
print(final2019, vp=grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()

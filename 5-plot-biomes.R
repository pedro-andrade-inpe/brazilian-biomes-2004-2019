
require(magrittr)
require(tmap)

munic <- geobr::read_municipality() %>% 
  sf::st_make_valid()

origbiomes2004 <- sf::read_sf("results/biomes-2004-before-intersec.gpkg") %>% dplyr::arrange(code_biome)
origbiomes2019 <- sf::read_sf("results/biomes-2019-before-intersec.gpkg") %>% dplyr::arrange(code_biome)

tmap::tmap_options(check.and.fix = TRUE)

sf::sf_use_s2(FALSE)

pdf("biomes-border.pdf", width = 3, height = 2)
tmap::tm_shape(origbiomes2019, bbox = c(-60.05, -15.3, -54, -11)) +
  tmap::tm_graticules(ticks = FALSE, col = "gray", lwd = 0.6) +
  tmap::tm_polygons(alpha = 0.5, border.col = "blue") +
  tmap::tm_shape(origbiomes2004) +
  tmap::tm_polygons(alpha = 0.5, border.col = "orange") +
  tmap::tm_add_legend(
    type = "line",
    labels = c("Biomes 2004", "Biomes 2019"),
    col = c("orange", "blue")) +
  tmap::tm_legend(position = c("right","top")) +
  tmap::tm_scale_bar(breaks = c(0, 50, 100, 200), position = "RIGHT") +
  tmap::tm_compass(position = c("left", "bottom"), size = 2, text.size = 0.4, type = "8star")
dev.off()

biomes2004 <- sf::read_sf("results/biomes-2004.gpkg")
biomes2019 <- sf::read_sf("results/biomes-2019.gpkg")

biomes <- c("Amazônia", "Caatinga", "Cerrado", "Mata Atlântica", "Pampa", "Pantanal")
origbiomes2004$biome <- biomes
origbiomes2019$biome <- biomes
biomes2004$biome <- biomes
biomes2019$biome <- biomes

colors = c("#b8fd00", "#fffdae", "#fdc5b0", "#d9fbab", "#fceed3", "#fee7ff")

origbiomes2019$Biomes <- origbiomes2019$biome
biomes2019$Biomes <- biomes2019$biome

origbiomes2004$Biomes <- origbiomes2004$biome
biomes2004$Biomes <- biomes2004$biome

################################################################################
orig2004 <- tmap::tm_shape(origbiomes2004) +
  tmap::tm_graticules(ticks = FALSE, col = "gray", lwd = 0.6, n.x = 4, n.y = 4, labels.size = 0.8, labels.inside.frame = TRUE) +
  tmap::tm_polygons("Biomes", palette = colors) +
  tmap::tm_credits("Biomes in 2004\nas defined by IBGE\n\n", size = 1.4, position = "right", align = "right") +
  tm_legend(
    scale = 0.4,
    title.size = 1.5,
    text.size = 1.1,
    bg.color = "grey85",
    frame = "black",
    position = c("left","bottom")) +
  tmap::tm_scale_bar(breaks = c(0, 250, 500, 1000), position = "RIGHT", text.size = 1.4) +
  tmap::tm_compass(position = c("right", "top"), size = 4, text.size = 0.8, type = "8star")

orig2019 <- tmap::tm_shape(origbiomes2019) +
  tmap::tm_graticules(ticks = FALSE, col = "gray", lwd = 0.6, n.x = 4, n.y = 4, labels.size = 0.8, labels.inside.frame = TRUE) +
  tmap::tm_polygons("Biomes", palette = colors) +
  tmap::tm_credits(
    "Biomes in 2019\nas defined by IBGE\n\n", size = 1.4, position = "right", align = "right") +
  tm_legend(
    scale = 0.4,
    title.size = 1.5,
    text.size = 1.1,
    bg.color = "grey85",
    frame = "black",
    position = c("left","bottom")) +
  tmap::tm_scale_bar(breaks = c(0, 250, 500, 1000), position = "RIGHT", text.size = 1.4) +
  tmap::tm_compass(position = c("right", "top"), size = 4, text.size = 0.8, type = "8star")

final2004 <- tmap::tm_shape(biomes2004) +
  tmap::tm_graticules(ticks = FALSE, col = "gray", lwd = 0.6, n.x = 4, n.y = 4, labels.size = 0.8, labels.inside.frame = TRUE) +
  tmap::tm_polygons("Biomes", palette = colors) +
  tmap::tm_credits("Biomes in 2004\nafter processing\n\n", size = 1.4, position = "right", align = "right") + 
  tmap::tm_scale_bar(breaks = c(0, 250, 500, 1000), position = "RIGHT", text.size = 1.4) +
  tm_legend(
    scale = 0.4,
    title.size = 1.5,
    text.size = 1.1,
    bg.color = "grey85",
    frame = "black",
    position = c("left","bottom")) +
  tmap::tm_compass(position = c("right", "top"), size = 4, text.size = 0.8, type = "8star")

final2019 <- tmap::tm_shape(biomes2019) +
  tmap::tm_graticules(ticks = FALSE, col = "gray", lwd = 0.6, n.x = 4, n.y = 4, labels.size = 0.8, labels.inside.frame = TRUE) +
  tmap::tm_polygons("Biomes", palette = colors) +
  tmap::tm_credits("Biomes in 2019\nafter processing\n\n", size = 1.4, position = "right", align = "right") +
  tmap::tm_scale_bar(breaks = c(0, 250, 500, 1000), position = "RIGHT", text.size = 1.4) +
  tm_legend(
    scale = 0.4,
    title.size = 1.5,
    text.size = 1.1,
    bg.color = "grey85",
    frame = "black",
    position = c("left","bottom")) +
  tmap::tm_compass(position = c("right", "top"), size = 4, text.size = 0.8, type = "8star")


pdf("biomes-orig.pdf", width = 6, height = 3)
grid::grid.newpage()
page.layout <- grid::grid.layout(nrow = 1, ncol = 2)
grid::pushViewport(grid::viewport(layout = page.layout))

print(orig2004, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
print(orig2019, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()

pdf("biomes-final.pdf", width = 6, height = 3)
grid::grid.newpage()
page.layout <- grid::grid.layout(nrow = 1, ncol = 2)
grid::pushViewport(grid::viewport(layout = page.layout))

print(final2004, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
print(final2019, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()

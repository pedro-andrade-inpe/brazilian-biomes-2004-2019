require(magrittr)
require(tmap)

states <- geobr::read_state() %>%
  sf::st_make_valid()

intersec <- sf::read_sf("results/biomes-intersec.gpkg")
intersec <- intersec[-c(1, 4, 9, 14, 17, 20), ] # remove the intersections with the biome itself

to <- intersec %>%
  dplyr::group_by(code_biome.1) %>%
  dplyr::summarize()

colors = c("#b8fd00", "#fffdae", "#fdc5b0", "#d9fbab", "#fceed3", "#fee7ff")
to["New Biome"] <- c("Amazonia", "Caatinga", "Cerrado", "Mata Atlantica", "Pampa", "Pantanal")

pdf("changes-biomes-states.pdf", width = 3, height = 3)
tmap::tm_shape(states) +
  tmap::tm_polygons(col = "#404143") +
#  tmap::tm_polygons(col = "#80B1D3") +
  tmap::tm_shape(to) +
  tmap::tm_polygons("New Biome", palette = colors, lty=0) +
  tmap::tm_legend(
    scale = 0.4,
    title.size = 1.5,
    text.size = 1.1,
    position = c("right","bottom"))
dev.off()

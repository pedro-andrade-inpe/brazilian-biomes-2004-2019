require(magrittr)
require(tmap)

states <- geobr::read_state() %>%
  sf::st_make_valid()

intersec <- sf::read_sf("biomes-intersec.gpkg")
intersec <- intersec[-c(1, 4, 9, 14, 17, 20), ] # remove the intersections with the biome itself

RColorBrewer::brewer.pal(7, "Set3")

pdf("changes-biomes-states.pdf")
tmap::tm_shape(states) +
  tmap::tm_polygons(col = "#80B1D3") +
  tmap::tm_shape(intersec) +
  tmap::tm_polygons(col = "orange", lwd=0)
dev.off()

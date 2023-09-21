require(magrittr)
require(tmap)

legalamaz <- geobr::read_amazon() %>%
  sf::st_make_valid()

states <- geobr::read_state() %>%
  sf::st_make_valid()

intersec <- sf::read_sf("results/biomes-intersec.gpkg")
intersec <- intersec[-c(1, 4, 9, 14, 17, 20), ] # remove the intersections with the biome itself

legalamzintersec <- dplyr::filter(intersec, code_biome == "AMZ" | code_biome.1 == "AMZ" )

biomes2004 <- sf::read_sf("results/biomes-2004.gpkg")
biomes2019 <- sf::read_sf("results/biomes-2019.gpkg")

amz <- rbind(biomes2004, biomes2019) %>% 
  dplyr::filter(code_biome == "AMZ") %>%
  sf::st_union() %>%
  sf::st_intersection(legalamaz)

mat <- rbind(biomes2004, biomes2019) %>% 
  dplyr::filter(code_biome == "MAT") %>%
  sf::st_union()

to <- intersec %>%
  dplyr::group_by(code_biome.1) %>%
  dplyr::summarize()

colors = c("#b8fd00", "#fffdae", "#fdc5b0", "#d9fbab", "#fceed3", "#fee7ff")
to["New Biome"] <- c("Amazonia", "Caatinga", "Cerrado", "Mata Atlantica", "Pampa", "Pantanal")

pdf("changes-biomes-states.pdf", width = 3, height = 3)
tmap::tm_shape(states, bbox = c(-60, -31.0, -27.7, -2.7)) +
  tmap::tm_polygons(col = "#404143", alpha = 0.7) +
#  tmap::tm_shape(amz) +
#  tmap::tm_polygons(alpha = 0.4, col = "blue", border.col = "blue", lty = 0) +
#  tmap::tm_shape(mat) +
#  tmap::tm_polygons(alpha = 0.4, col = "red", border.col = "red", lty = 0) +
  tmap::tm_shape(to) +
  tmap::tm_polygons("New Biome", palette = colors, lty = 0) +
#  tmap::tm_shape(legalamaz) +
#  tmap::tm_polygons(alpha = 0, border.col = "red") +
#  tmap::tm_add_legend(
#    type = "fill",
#    labels = c("Subject to Law 12,651/2012", "Subject to Law 11,428/2006"),
#    col = c("#4949b0", "#af494a"),) +
  tmap::tm_legend(
    scale = 0.4,
    title.size = 1.5,
    text.size = 1.1,
    position = c("right","bottom"))
dev.off()

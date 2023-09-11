
require(dplyr)
units::install_unit("Mha", "1e6 ha")

origbiomes2004 <- sf::read_sf("biomes-2004-before-intersec.gpkg")
origbiomes2019 <- sf::read_sf("biomes-2019-before-intersec.gpkg")

biomes2004 <- sf::read_sf("biomes-2004.gpkg")
biomes2019 <- sf::read_sf("biomes-2019.gpkg")

par(mfcol=c(1,2))
plot(sf::st_geometry(biomes2019))
plot(sf::st_geometry(biomes2004))

area2004 <- round(sf::st_area(biomes2004) %>% units::set_units("Mha"), 2)
area2019 <- round(sf::st_area(biomes2019) %>% units::set_units("Mha"), 2)

area2004 <- c(area2004, sum(area2004))
area2019 <- c(area2019, sum(area2019))

brazil <- geobr::read_country()

sf::st_area(brazil) %>% units::set_units("Mha")

total_biomes <- tibble::tibble(name = c(biomes2004$code_biome, "Brazil"), area2004, area2019) %>%
  dplyr::mutate(delta = round((units::drop_units(area2019 / area2004) - 1) * 100, 2)) %>%
  as.data.frame()

total_biomes %>%
  units::drop_units() %>%
  kableExtra::kbl(format = "latex")

intersec <- sf::st_intersection(biomes2004, biomes2019)
sf::write_sf(intersec, "biomes-intersec.gpkg")

intersec <- intersec %>%
  dplyr::mutate(area = round(sf::st_area(.)  %>% units::set_units("Mha"), 2)) %>%
  sf::st_drop_geometry() 

result <- intersec %>%
  dplyr::mutate(area = round(area, 2)) %>%
  tidyr::pivot_wider(names_from = code_biome, values_from = area, values_fill = list(area = units::set_units(0, "Mha")))

result <- result[, c(1, 2, 5, 3, 6, 7, 4)] %>%
  janitor::adorn_totals( c("row", "col"))

colnames(result)[8] <- "Total 2019"
result$code_biome.1[7] <- "Total 2004"

result %>%
  units::drop_units() %>%
  kableExtra::kbl(format = "latex")


require(magrittr)
require(tmap)

units::install_unit("Mha", "1e6 ha")

legalamaz <- geobr::read_amazon() %>%
  sf::st_make_valid()

biomes2004 <- sf::read_sf("results/biomes-2004.gpkg")
biomes2019 <- sf::read_sf("results/biomes-2019.gpkg")

amaz2004 <- biomes2004[1,]
amaz2019 <- biomes2019[1,]

diff2004 <- sf::st_difference(amaz2004, legalamaz)
diff2019 <- sf::st_difference(amaz2019, legalamaz)

sf::st_area(diff2004) %>% units::set_units("Mha")
sf::st_area(diff2019) %>% units::set_units("Mha")

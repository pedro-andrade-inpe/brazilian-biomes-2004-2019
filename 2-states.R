

states <- geobr::read_state()

sf::write_sf(states, "states.gpkg")

intersec <- sf::read_sf("biomes-intersec.gpkg")

intersec2 <- sf::st_intersection(states, intersec)

intersec <- intersec2 %>%
  dplyr::mutate(area = round(sf::st_area(.)  %>% units::set_units("Mha"), 2)) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(name_state, area) %>%
  dplyr::group_by(name_state) %>%
  dplyr::summarise(overlap = sum(area))


states2 <- states %>%
  dplyr::mutate(area = round(sf::st_area(.)  %>% units::set_units("Mha"), 2)) %>%
  dplyr::left_join(intersec, "name_state") %>%
  sf::st_drop_geometry() %>%
  dplyr::select(name_state, area, overlap) %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(perc = round(units::drop_units(overlap / area * 100), 2)) %>%
  dplyr::arrange(desc(perc))


states2[1:10,] %>%
  units::drop_units() %>%
  kableExtra::kbl(format = "latex")


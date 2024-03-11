require(magrittr)

units::install_unit("Mha", "1e6 ha")

intersec <- sf::read_sf("results/biomes-intersec.gpkg")

intersec <- intersec[-c(1, 4, 9, 14, 17, 20),]

munic <- geobr::read_municipality(year = 2022) %>%
  dplyr::mutate(name = paste0(name_muni, "-", abbrev_state)) %>%
  sf::st_make_valid() %>%
  sf::st_transform(sf::st_crs(intersec))

# this operation takes a while to be executed
intersec2 <- sf::st_intersection(munic, intersec) %>%
  sf::st_make_valid()

intersec <- intersec2 %>%
  dplyr::mutate(area = round(sf::st_area(.)  %>% units::set_units("Mha"), 2)) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(name, area, abbrev_state) %>%
  dplyr::group_by(name) %>%
  dplyr::summarise(overlap = sum(area))

munic2 <- munic %>%
  dplyr::mutate(area = round(sf::st_area(.)  %>% units::set_units("Mha"), 2)) %>%
  dplyr::left_join(intersec, "name") %>%
  sf::st_drop_geometry() %>%
  dplyr::select(name, area, overlap, abbrev_state) %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(perc = round(units::drop_units(overlap / area * 100), 2)) %>%
  dplyr::arrange(desc(perc))

write.csv(munic2, "results/overlap-munic2.csv")

munic100 <- munic2 %>% 
  dplyr::filter(perc >= 100)

dim(munic100) # 160 municipalities changed 100% of their biomes
  
munic1 <- munic2 %>% 
  dplyr::filter(perc > 0)

dim(munic1) # 879 municipalities changed part of their biomes
dim(munic1) / dim(munic2) # 15.7% of all the municipalities

munic90 <- munic2 %>% 
  dplyr::filter(perc >= 90) %>%
  dplyr::group_by(abbrev_state) %>%
  dplyr::count() %>%
  dplyr::mutate(n90 = n) %>%
  dplyr::select(abbrev_state, n90)

munic50 <- munic2 %>% 
  dplyr::filter(perc >= 50) %>%
  dplyr::group_by(abbrev_state) %>%
  dplyr::count() %>%
  dplyr::mutate(n50 = n) %>%
  dplyr::select(abbrev_state, n50)

munic5 <- munic2 %>% 
  dplyr::filter(perc >= 5) %>%
  dplyr::group_by(abbrev_state) %>%
  dplyr::count() %>%
  dplyr::mutate(n5 = n) %>%
  dplyr::select(abbrev_state, n5)

munic2 %>% 
  dplyr::filter(perc >= 5) %>%
  dim()

result <- dplyr::inner_join(munic5, munic50, by = "abbrev_state") %>%
  dplyr::inner_join(munic90, by = "abbrev_state") %>%
  dplyr::arrange(desc(n5)) %>%
  janitor::adorn_totals( c("row"))


result %>%
  units::drop_units() %>%
  kableExtra::kbl(format = "latex")

biomes2004 <- sf::read_sf("results/biomes-2004.gpkg")
biomes2019 <- sf::read_sf("results/biomes-2019.gpkg")

overlap2004 <- sf::st_intersects(biomes2004, munic)
overlap2019 <- sf::st_intersects(biomes2019, munic)

for(i in 1:6){
  biome <- biomes2004$code_biome[i]
  munic[[paste0(biome, 2004)]] <- 0
  munic[[paste0(biome, 2004)]][overlap2004[[i]]] <- 1
  
  munic[[paste0(biome, 2019)]] <- 0
  munic[[paste0(biome, 2019)]][overlap2019[[i]]] <- 1
  
  sum(munic[[paste0(biome, 2004)]])
  sum(munic[[paste0(biome, 2019)]])
  
  munic[[biome]] <- paste0(munic[[paste0(biome, 2004)]], munic[[paste0(biome, 2019)]])
}  

sf::write_sf(munic, "results/municipalities.gpkg")

result <- data.frame()
for(i in 1:6){
  biome <- biomes2004$code_biome[i]
  mtable <- table(munic[[biome]])
  result <- rbind(result, as.numeric(mtable))
  rownames(result)[i] <- biome
}

colnames(result) <- c("not", "added", "removed", "stayed")

result <- result %>%
  dplyr::mutate(tot2004 = removed + stayed) %>%
  dplyr::mutate(tot2019 = added + stayed) %>%
  dplyr::select(tot2004, added, removed, tot2019)

result %>%
  kableExtra::kbl(format = "latex")

require(magrittr)

brazil <- geobr::read_country()

biomes2019 <- geobr::read_biomes(year = 2019) %>%
  dplyr::filter(name_biome != "Sistema Costeiro") %>%
  dplyr::arrange(name_biome) %>%
  dplyr::mutate(code_biome = name_biome) %>%
  dplyr::select(code_biome) %>%
  dplyr::mutate(code_biome = dplyr::recode(code_biome,   
                                           "Amazônia" = "AMZ",
                                           "Caatinga" = "CAAT",
                                           "Cerrado" = "CER",
                                           "Mata Atlântica" = "MAT",
                                           "Pampa" = "PMP",
                                           "Pantanal" = "PTN"))

sf::write_sf(biomes2019, "biomes-2019-before-intersec.gpkg")

sf::st_agr(biomes2019) <- "constant"
biomes2019 <- sf::st_intersection(biomes2019, brazil) %>% sf::st_make_valid()

sf::write_sf(biomes2019, "biomes2019-after-intersec.gpkg")

diff2019 <- sf::st_difference(brazil, sf::st_union(biomes2019) %>% sf::st_make_valid()) %>%
  sf::st_cast("POLYGON")

dim(diff2019) # 13285 polygons in brazil but not in biomes 2004 that need to be handled

sum(units::set_units(sf::st_area(diff2019), "km^2")) #   5486.6 [km^2]

sf::write_sf(diff2019, "diff-2019.gpkg")

# the buffer and overlap takes a while to process (even more in the 2019 data)
relations <- sf::st_overlaps(biomes2019, diff2019 %>% sf::st_buffer(0.00001)) # 1.11m of buffer

diff2019$code_biome <- ""
for(i in 1:6){
  diff2019$code_biome[relations[[i]]] <- biomes2019$code_biome[i]
}

# ignore those polygons that have relation with more than one biome
repeated <- unlist(relations)[which(duplicated(unlist(relations)))]
sf::write_sf(diff2019[repeated, ], "repeated-2019.gpkg")

diff2019[repeated, ]$code_biome = ""

sum(sf::st_area(diff2019[repeated, ])) %>% units::set_units("km^2") # 194.74km2

sf::st_agr(diff2019) <- "constant"
sf::st_agr(biomes2019) <- "constant"

# compute the intersection area of those polygons with more than one biome
# the biome with greater intersection is used for the respective polygon
for(i in 1:length(repeated)){
  cat(paste0(i, "/", length(repeated), "\n"))
  biome <- sf::st_intersection(diff2019[repeated[i], ], biomes2019)
  
  if(dim(biome)[1] > 1) 
    biome <- biome %>%
      sf::st_make_valid() %>%
      dplyr::mutate(area = sf::st_area(.)) %>%
      dplyr::arrange(rev(area))
  
    biome <- biome %>%
      as.data.frame() %>%
      .[1, "code_biome.1"]
  
  diff2019$code_biome[repeated[i]] <- biome
}

# there are some problems with grouping 4 polygons. they are ignored
# their areas are very small:
sf::st_area(diff2019[12138,]) # 34k m2
sf::st_area(diff2019[12266,]) # 13k m2
sf::st_area(diff2019[12674,]) # 39k m2
sf::st_area(diff2019[12985,]) #  8k m2

rbind(biomes2019, diff2019[c(1:12137, 12139:12265, 12267:12673, 12675:12984, 12986:13285),]) %>%
  dplyr::filter(code_biome != "") %>%
  dplyr::group_by(code_biome) %>%
  dplyr::summarise() %>%
  sf::st_cast() %>%
  sf::st_make_valid() %>%
  sf::write_sf("biomes-2019.gpkg")

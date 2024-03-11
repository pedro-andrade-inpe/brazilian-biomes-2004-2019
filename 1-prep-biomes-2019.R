require(magrittr)

brazil <- geobr::read_country() %>% sf::st_make_valid()
units::install_unit("Mha", "1e6 ha")

# File downloaded from
# https://geoftp.ibge.gov.br/informacoes_ambientais/estudos_ambientais/biomas/vetores/Biomas_250mil.zip
# This file is not yet available in geobr
biomes2019 <- sf::read_sf("Biomas_250mil/lm_bioma_250.shp") %>%
  sf::st_transform(sf::st_crs(brazil)) %>%
  sf::st_make_valid() %>%
  dplyr::mutate(name_biome = Bioma) %>%
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

sf::write_sf(biomes2019, "results/biomes-2019-before-intersec.gpkg")

sf::st_agr(biomes2019) <- "constant"
sf::sf_use_s2(FALSE)
biomes2019 <- sf::st_intersection(biomes2019, brazil) %>% sf::st_make_valid()

sf::write_sf(biomes2019, "results/biomes2019-after-intersec.gpkg")

diff2019 <- sf::st_difference(brazil, sf::st_union(biomes2019) %>% sf::st_make_valid()) %>%
  sf::st_cast("POLYGON")

dim(diff2019) # 13285 polygons in Brazil but not in biomes 2019 that need to be handled
# 10224 in the newest version

sum(units::set_units(sf::st_area(diff2019), "Mha")) #   0.54 [Mha]

sf::write_sf(diff2019, "results/diff-2019.gpkg")

# the buffer and overlap takes a while to process (even more for the 2019 data)
relations <- sf::st_overlaps(biomes2019, diff2019 %>% sf::st_buffer(0.00001)) # 1.11m of buffer

diff2019$code_biome <- ""
for(i in 1:6){
  diff2019$code_biome[relations[[i]]] <- biomes2019$code_biome[i]
}

# ignore those polygons that have relation with more than one biome
repeated <- unlist(relations)[which(duplicated(unlist(relations)))]
sf::write_sf(diff2019[repeated, ], "results/repeated-2019.gpkg")

diff2019[repeated, ]$code_biome = ""

sum(sf::st_area(diff2019[repeated, ])) %>% units::set_units("km^2") # 194.74km2
# reduced to 178.26km2

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

# 10224 polygons
dim(diff2019)

diff2019 <- diff2019 %>% 
  sf::st_make_valid() %>%
  dplyr::mutate(geometry = geom)
  
sf::st_geometry(diff2019) <- "geometry"

diff2019 <- diff2019 %>% 
  dplyr::select(code_biome, geometry)

rbind(biomes2019, diff2019) %>%
  dplyr::filter(code_biome != "") %>%
  dplyr::group_by(code_biome) %>%
  dplyr::summarise() %>%
  sf::st_cast() %>%
  sf::st_make_valid() %>%
  sf::write_sf("results/biomes-2019.gpkg")

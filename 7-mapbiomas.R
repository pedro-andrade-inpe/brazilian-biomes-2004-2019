require(magrittr)
require(dplyr)

units::install_unit("Mha", "1e6 ha")

########################### RECLASSIFY MAPBIOMAS ###############################

mapbiomas2022 <- terra::rast("c:/Users/pedro/Downloads/brasil_coverage_2022.tif")

mbfreq <- terra::freq(mapbiomas2022)
value <- mbfreq %>% dplyr::filter(value %in% c(1, 3, 4, 5, 6, 49, 10, 11, 12, 32, 29, 50, 13)) %>% .$count %>% sum()
value * 30 * 30 / 1e10 # 556 Mha of native vegetation

# from https://brasil.mapbiomas.org/wp-content/uploads/sites/4/2023/08/Legenda-Colecao-8-LEGEND-CODE.pdf
m1 <- cbind(c(1, 3, 4, 5, 6, 49, 10, 11, 12, 32, 29, 50, 13), 1)

linhas <- c(2)

mapbiomas2022_2values <- terra::classify(mapbiomas2022, m1, others=0)

terra::writeRaster(mapbiomas2022_2values, "c:/Users/pedro/Downloads/brasil_coverage_natveg.tif")
##############################################################################

intersecsf <- sf::read_sf("results/biomes-intersec.gpkg")

intersecsf$area <- sf::st_area(intersecsf) %>% units::set_units("Mha")
intersecsf <- intersecsf[-c(1, 4, 9, 14, 17, 20), ] # remove the intersections with the biome itself

intersec <- terra::vect("results/biomes-intersec.gpkg")
intersec <- intersec[-c(1, 4, 9, 14, 17, 20), ] # remove the intersections with the biome itself

mapbiomas2022 <- terra::rast("c:/Users/pedro/Downloads/brasil_coverage_natveg.tif")

intersec$native_veg <- 0
intersec$non_native <- 0

regions <- dim(intersec)[1]

for(i in 1:regions){
  cat(paste("Processing ", i, "/", regions, "\n"))
  v <- terra::crop(mapbiomas2022, intersec[i,]) %>%
    terra::mask(intersec[i,])
  
  mfreq <- terra::freq(v)
  
  total <- mfreq %>% dplyr::filter(value == 1) %>% .$count
  intersec$native_veg[i] <- total
  total <- mfreq %>% dplyr::filter(value == 0) %>% .$count
  intersec$non_native[i] <- total
}

intersec$native_veg2 <- units::set_units(intersec$native_veg * 30 * 30 * 1e-4, "ha") %>% 
  units::set_units("Mha")

intersec$perc = as.numeric(intersec$native_veg2) / as.numeric(intersec$total_area) * 100
intersec$total_area <- intersecsf$area

result <- intersec %>% 
  as.data.frame() %>%
  dplyr::mutate(native_veg = as.numeric(native_veg2)) %>%
  dplyr::mutate(total_area = as.numeric(total_area)) %>%
  dplyr::mutate(from = code_biome) %>%
  dplyr::mutate(to = code_biome.1) %>%
  dplyr::select(-non_native, -native_veg2, -code_biome, -code_biome.1) %>%
  dplyr::mutate(perc = native_veg / total_area * 100) %>%
  dplyr::mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
  dplyr::relocate(from, to, total_area, native_veg, perc) %>%
  dplyr::arrange(-perc) %>%
  dplyr::mutate(from = dplyr::recode(from,   
                                           "AMZ" = "Amazônia",
                                           "CAAT" = "Caatinga",
                                           "CER" = "Cerrado",
                                           "MAT" = "Mata Atlântica",
                                           "PMP" = "Pampa",
                                           "PTN" = "Pantanal")) %>%
  dplyr::mutate(to = dplyr::recode(to,   
                                     "AMZ" = "Amazônia",
                                     "CAAT" = "Caatinga",
                                     "CER" = "Cerrado",
                                     "MAT" = "Mata Atlântica",
                                     "PMP" = "Pampa",
                                     "PTN" = "Pantanal"))

result %>%
  kableExtra::kbl(format = "latex")

decrease <- result %>%
  dplyr::group_by(from) %>%
  dplyr::summarise(decrease = sum(native_veg))

increase <- result %>%
  dplyr::group_by(to) %>%
  dplyr::summarise(increase = sum(native_veg))

result <- cbind(decrease, increase) %>%
  dplyr::mutate(biome = from) %>%
  dplyr::select(-from, -to) %>%
  dplyr::mutate(change = increase - decrease) %>%
  dplyr::relocate(biome, increase, decrease, change)

result %>%
  kableExtra::kbl(format = "latex")





require(magrittr)

units::install_unit("Mha", "1e6 ha")

brazil <- geobr::read_country()

biomes2004 <- geobr::read_biomes(year = 2004) %>%
  dplyr::filter(!startsWith(.$code_biome, "00")) %>%
  sfheaders::sf_remove_holes() %>%
  dplyr::select(code_biome) 

sf::write_sf(biomes2004, "results/biomes-2004-before-intersec.gpkg")

sf::st_agr(biomes2004) <- "constant"
biomes2004 <- sf::st_intersection(biomes2004, brazil) %>% sf::st_make_valid()

sf::write_sf(biomes2004, "results/biomes2004-after-intersec.gpkg")

diff2004 <- sf::st_difference(brazil, sf::st_union(biomes2004)) %>%
  sf::st_cast("POLYGON")

dim(diff2004) # 5200  polygons in brazil but not in biomes 2004 that need to be handled
sum(units::set_units(sf::st_area(diff2004), "Mha")) # 15.23 [Mha]

sf::write_sf(diff2004, "results/diff-2004.gpkg")

# the buffer and overlap takes a while to process
relations <- sf::st_overlaps(biomes2004, diff2004 %>% sf::st_buffer(0.00001)) # 1.11m of buffer

diff2004$code_biome <- ""
for(i in 1:6){
  diff2004$code_biome[relations[[i]]] <- biomes2004$code_biome[i]
}

# ignore those polygons that have relation with more than one biome
repeated <- unlist(relations)[which(duplicated(unlist(relations)))]
sf::write_sf(diff2004[repeated, ], "results/repeated-2004.gpkg")

diff2004$code_biome[repeated] <- ""

sf::st_agr(diff2004) <- "constant"
sf::st_agr(biomes2004) <- "constant"

# compute the intersection area of those polygons with more than one biome
# the biome with greater intersection is used for the respective polygon
for(i in 1:length(repeated)){
  cat(paste0(i, "/", length(repeated), "\n"))
  biome <- sf::st_intersection(diff2004[repeated[i], ], biomes2004)
  
  if(dim(biome)[1] > 1) 
    biome <- biome %>%
    sf::st_make_valid() %>%
    dplyr::mutate(area = sf::st_area(.)) %>%
    dplyr::arrange(rev(area))
  
  biome <- biome %>%
    as.data.frame() %>%
    .[1, "code_biome.1"]
  
  diff2004$code_biome[repeated[i]] <- biome
}

diff2004$code_biome[repeated[1]] <- "" # more than one biome (see below)
diff2004$code_biome[repeated[3]] <- "" # more than one biome (see below)

# split polygons that share more than one biome
# the intersection points were extracted visually using QGIS

# 1) repeated[3] (share 2 biomes)
r2 <- diff2004[repeated[3],] %>% nngeo::st_remove_holes()

plot(r2)
mr2 <- as.matrix(sf::st_geometry(r2)[[1]])

p1 = which(abs(mr2[,1] +49.4498) < 1e-2 & abs(mr2[,2] +8.5852) < 1e-2)
p2 = which(abs(mr2[,1] +49.4426) < 1e-2 & abs(mr2[,2] +8.6370) < 1e-2)
cer1 <- mr2[c(p1:p2, p1),] 
cer1pol <- sf::st_sf(code_biome = "CER", geom = sf::st_sfc(sf::st_polygon(list(cer1)), crs = sf::st_crs(diff2004)))
plot(cer1pol)
diff2004 <- rbind(diff2004, cer1pol)

mr2 <- mr2[-((p1+1):(p2-1)),]

p1 = which(abs(mr2[,1] +48.1425) < 1e-3 & abs(mr2[,2] +5.2732) < 1e-3)
p2 = which(abs(mr2[,1] +48.1447) < 1e-2 & abs(mr2[,2] +5.2588) < 1e-2)
cer2 <- mr2[c(p1:p2, p1), ]
cer2pol <- sf::st_sf(code_biome = "CER", geom = sf::st_sfc(sf::st_polygon(list(cer2)), crs = sf::st_crs(diff2004)))
diff2004 <- rbind(diff2004, cer2pol)
plot(cer2pol)
mr2 <- mr2[-((p1+1):(p2-1)),]

amz1pol <- sf::st_sf(code_biome = "AMZ", geom = sf::st_sfc(sf::st_polygon(list(mr2)), crs = sf::st_crs(diff2004)))
diff2004 <- rbind(diff2004, amz1pol)
plot(amz1pol)

# 2) repeated[1] (share three biomes)
r2 <- diff2004[repeated[1],] %>% nngeo::st_remove_holes()
plot(r2)

mr2 <- as.matrix(sf::st_geometry(r2)[[1]])

p1 = which(abs(mr2[,1] +42.5238) < 1e-2 & abs(mr2[,2] +10.3700) < 1e-2)
p2 = which(abs(mr2[,1] +42.5396) < 1e-3 & abs(mr2[,2] +10.4312) < 1e-3)

cer1 <- mr2[c(p1:p2, p1), ] 
cer1pol <- sf::st_sf(code_biome = "CER", geom = sf::st_sfc(sf::st_polygon(list(cer1)), crs = sf::st_crs(diff2004)))
diff2004 <- rbind(diff2004, cer1pol)
plot(cer1pol)

mr2 <- mr2[-((p1+1):(p2-1)),]

p1 = which(abs(mr2[,1] +37.1289) < 1e-2 & abs(mr2[,2] +9.9210) < 1e-2)
p2 = which(abs(mr2[,1] +37.1543) < 1e-3 & abs(mr2[,2] +9.9488) < 1e-3)

caat1 <- mr2[c(p1:p2, p1), ] 
caat1pol <- sf::st_sf(code_biome = "CAAT", geom = sf::st_sfc(sf::st_polygon(list(caat1)), crs = sf::st_crs(diff2004)))
diff2004 <- rbind(diff2004, caat1pol)
plot(caat1pol)

mr2 <- mr2[-((p1+1):(p2-1)),]
mr2 <- mr2[mr2[,1] > -38,] # there are some useless points in the data
mat1pol <- sf::st_sf(code_biome = "MAT", geom = sf::st_sfc(sf::st_polygon(list(mr2)), crs = sf::st_crs(diff2004)))
diff2004 <- rbind(diff2004, mat1pol)

diff2004 <- diff2004 %>% sf::st_make_valid()

rbind(biomes2004, diff2004) %>% 
  dplyr::filter(code_biome != "") %>%
  dplyr::group_by(code_biome) %>%
  dplyr::summarise() %>%
  sf::st_cast() %>%
  sf::st_make_valid() %>%
  sf::write_sf("results/biomes-2004.gpkg")

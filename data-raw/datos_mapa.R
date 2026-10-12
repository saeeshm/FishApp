# Author: Saeesh Mangwani
# Date: 2026-10-12

# Description: Builds the spatial layers shown on the regional map page
# (mod_04_mapaReg). Run from the project root.
#
# Inputs:
#   data-raw/spatial_data/tela_comunidades.geojson
#   data-raw/spatial_data/trujillo_comunidades.geojson
#   data-raw/spatial_data/180910 zonas recuperacion pesquera Tela.shp
#   data-raw/spatial_data/APMC_HN_2019.shp
#
# Outputs: data/tela_locs.rda, data/truj_locs.rda, data/zdr.rda, data/apmc.rda

# ==== Packages ====
library(sf)

# ==== Map data ====
hpath <- 'data-raw/spatial_data'
tela_locs <- st_read(file.path(hpath, 'tela_comunidades.geojson'))
truj_locs <- st_read(file.path(hpath, 'trujillo_comunidades.geojson'))
zdr <- st_read(file.path(hpath, '180910 zonas recuperacion pesquera Tela.shp'))
apmc <- st_read(file.path(hpath, 'APMC_HN_2019.shp'))
apmc$lab <- paste0(
    '<strong>Nombre:</strong> ', apmc$NOMBRE, '<br>',
    '<strong>Tipo:</strong> ', apmc$CATEGORIA
  ) |>
  lapply(htmltools::HTML)

# ==== Saving ====
usethis::use_data(tela_locs, truj_locs, zdr, apmc, overwrite = TRUE)

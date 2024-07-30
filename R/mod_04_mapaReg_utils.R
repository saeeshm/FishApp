# Author: Saeesh Mangwani
# Date: 2024-07-30

# Description: Utility functions for the producing the region map

# Preparing input data
# hpath <- '../fisheries-analysis/data/tela-site-data'
# comunidades <- st_read(file.path(hpath, 'tela_comunidades.geojson'))
# zdr <- st_read(file.path(hpath, '180910 zonas recuperacion pesquera Tela.shp'))
# apmc <- st_read(file.path(hpath, 'APMC_HN_2019.shp'))
# save(comunidades, zdr, apmc, file='data/tela_mapa.rda')

# ==== Functions ====

# Function for drawing the map for the Tela Bay region
mapa_regional <- function(region){
  # This argument does nothing yet
  if(region == 'Bahia de Tela') invisible()
  leaflet::leaflet() |> 
    leaflet::setView(-87.56, 15.93, zoom = 10) |>
    # leaflet::setMaxBounds(-88, 14, -86, 17) |>
    # leaflet::addProviderTiles(leaflet::providers$Esri.WorldShadedRelief,
    #                           options = leaflet::providerTileOptions(opacity = 0.4)) |> 
    leaflet::addProviderTiles(leaflet::providers$CartoDB.VoyagerLabelsUnder,
                              options = leaflet::providerTileOptions(opacity = 0.8)) |>
    leaflet::addMapPane('admin', zIndex=500) |>
    leaflet::addMapPane('comunidades', zIndex=550) |>
    # Communities
    leaflet::addCircles(
      data=comunidades,
      label = comunidades$nombre,
      group = 'Comunidades',
      color = 'darkorange',
      fill = T,
      radius = 6,
      options = leaflet::pathOptions(pane = 'comunidades')
    ) |> 
    # National Administrative layers
    leaflet::addPolygons(
      data=apmc,
      label = apmc$CATEGORIA,
      group = 'APMCs',
      # color = 'orange',
      color = ~leaflet::colorFactor('plasma', domain=unique(apmc$CATEGORIA))(CATEGORIA),
      fill = T,
      fillOpacity = 0.1,
      weight = 1,
      options = leaflet::pathOptions(pane = 'admin')
    ) |> 
    # Fish recovery zones
    leaflet::addPolygons(
      data=zdr,
      label = zdr$Nombre,
      group = 'ZRPs Tela',
      color = 'darkgreen',
      fill = T,
      fillOpacity = 0.1,
      weight = 2,
      options = leaflet::pathOptions(pane = 'admin')
    ) |> 
    # Layer control set
    leaflet::addLayersControl(
      overlayGroups = c("APMCs", "ZRPs Tela", "Comunidades"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) |> 
    # Button for resetting the view
    leaflet::addEasyButton(leaflet::easyButton(
      icon="fa-globe", title="Vuelva a la región",
      onClick=leaflet::JS("function(btn, map){ map.setView([15.93, -87.56], 10); }")))
}
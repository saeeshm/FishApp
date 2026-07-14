# Author: Saeesh Mangwani
# Date: 2024-07-30

# Description: Utility functions for the producing the region map

# ==== Functions ====

# Setting map views based on database
get_map_views <- function(region){
  if(!region %in% c('Tela', 'Trujillo')){
    stop('Date region provided is not valid! Check database input options')
  }
  views <- switch(
    region,
    'Tela' = list(
      lng = -87.56, 
      lat = 15.93,
      zoom = 10
    ),
    'Trujillo' = list(
      lng = -85.92, 
      lat = 15.94,
      zoom = 10.5
     )
   )
  return(views)
}

# Function for drawing the map for the Tela Bay region
mapa_regional <- function(region){
  # Choosing community location coordinates based on the region
  if(!region %in% c('Tela', 'Trujillo')){
    stop('Date region provided is not valid! Check database input options')
  }
  curr_comunidades <- switch(
    region,
    'Tela' = tela_locs,
    'Trujillo' = truj_locs
  )
  # Getting the map view options based on the region
  viewOpts <- get_map_views(region)
  
  # Drawing leaflet map
  leaflet::leaflet() |> 
    leaflet::setView(
      lng = viewOpts$lng, 
      lat = viewOpts$lat, 
      zoom = viewOpts$zoom
    ) |>
    # leaflet::setMaxBounds(-88, 14, -86, 17) |>
    # leaflet::addProviderTiles(leaflet::providers$Esri.WorldShadedRelief,
    #                           options = leaflet::providerTileOptions(opacity = 0.4)) |> 
    leaflet::addProviderTiles(leaflet::providers$CartoDB.VoyagerLabelsUnder,
                              options = leaflet::providerTileOptions(opacity = 0.8)) |>
    leaflet::addMapPane('admin', zIndex=500) |>
    leaflet::addMapPane('comunidades', zIndex=550) |>
    # Communities
    leaflet::addCircles(
      data = curr_comunidades,
      label = curr_comunidades$nombre,
      group = 'Comunidades',
      color = 'darkorange',
      fill = T,
      radius = 10,
      options = leaflet::pathOptions(pane = 'comunidades')
    ) |> 
    # National Administrative layers
    leaflet::addPolygons(
      data=apmc,
      label = apmc$lab,
      # labelOptions = labelOptions()
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
    # Button for resetting the view - defines view of interest based on region
    # chosen using the viewOpts object defined above
    leaflet::addEasyButton(leaflet::easyButton(
      icon="fa-globe", title="Vuelva a la región",
      onClick=leaflet::JS(paste0(
        "function(btn, map){ map.setView([", 
        viewOpts$lat,
        ", ", 
        viewOpts$lng,
        "], ", 
        viewOpts$zoom,
        "); }"
      )))) |> 
    # Optional: map inset to show where we are in the world.
    leaflet::addMiniMap(position = 'bottomleft', width = 125, height = 125, toggleDisplay = T, zoomLevelFixed = 5)
}
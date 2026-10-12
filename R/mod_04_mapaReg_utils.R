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

  # Carto basemap API key. Carto issues separate keys restricted to different
  # origins, so we select the key for the current environment:
  #   - production (deployed app): CARTO_API_KEY        (restricted to app domain)
  #   - local development:         CARTO_API_KEY_LOCAL  (restricted to localhost)
  # golem::app_prod() is TRUE when launched via app.R (the deploy entrypoint)
  # and FALSE via dev/run_dev.R. Both keys live in the gitignored .Renviron
  # locally; only CARTO_API_KEY is pushed to the server (see deploy.R). Never
  # hardcode a key here, so they stay out of the git repository.
  carto_key_var <- if (golem::app_prod()) "CARTO_API_KEY" else "CARTO_API_KEY_LOCAL"
  carto_key <- Sys.getenv(carto_key_var, unset = "")
  if (carto_key == "") {
    warning(
      carto_key_var, " is not set - the Carto basemap tiles will not load. ",
      "Set both keys in a local .Renviron, and push the production key via ",
      "rsconnect::deployApp(envVars = \"CARTO_API_KEY\") when deploying.",
      call. = FALSE
    )
  }

  # Drawing leaflet map
  leaflet::leaflet() |> 
    leaflet::setView(
      lng = viewOpts$lng, 
      lat = viewOpts$lat, 
      zoom = viewOpts$zoom
    ) |>
    leaflet::addTiles(
      map = _,
      urlTemplate = paste0(
        'https://basemaps.cartocdn.com/rastertiles/voyager_labels_under/{z}/{x}/{y}.png',
        '?key=', carto_key
      ),
      attribution = paste(
        '&copy; <a href="https://carto.com/attributions">CARTO</a>,',
        '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
      ),
      options = leaflet::tileOptions(opacity = 0.8)
    ) |> 
    leaflet::addMapPane('admin', zIndex=500) |>
    leaflet::addMapPane('comunidades', zIndex=550) |>
    # Communities
    leaflet::addCircles(
      data = curr_comunidades,
      label = curr_comunidades$nombre,
      group = 'Comunidades',
      color = 'darkorange',
      fill = T,
      radius = 30,
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
#' 04_mapaReg UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bslib card
mod_04_mapaReg_ui <- function(id){
  ns <- NS(id)
  layout_sidebar(
    # Inputs and descriptions in the sidebar
    sidebar = bslib::sidebar(
      position='left',
      # Text output describing currently focused region
      tags$h4('Region Elegido:'),
      textOutput(ns('region_desc'))
    ),
    bslib::card(
      leaflet::leafletOutput(ns('mapa'), height = "100%")
    )
  )
}
    
#' 04_mapaReg Server Functions
#'
#' @noRd 
mod_04_mapaReg_server <- function(id, region){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Region description text output
    output$region_desc <- renderText(region)
      # Producing map
    output$mapa <- leaflet::renderLeaflet(mapa_regional(region))
  })
}
    
## To be copied in the UI
# mod_04_mapaReg_ui("04_mapaReg_1")
    
## To be copied in the server
# mod_04_mapaReg_server("04_mapaReg_1")

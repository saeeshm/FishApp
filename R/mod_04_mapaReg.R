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
      width = '15%',
      position='left',
      # Instantiating share sidebar UI, specifying filters for this page
      mod_06_sidebar_filters_ui(ns('shared_sidebar'), show=c('database'))
    ),
    bslib::card(
      leaflet::leafletOutput(ns('mapa'), height = "100%")
    )
  )
}
    
#' 04_mapaReg Server Functions
#'
#' @noRd 
mod_04_mapaReg_server <- function(id, lang, filters){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Filter server for this page, using the filter_state reactive object as the
    # shared source of truth
    mod_06_sidebar_filters_server(
      'shared_sidebar',
      filter_state = filters,
      lang = lang
    )
    
      # Producing map - reactive based on chosen region
    output$mapa <- leaflet::renderLeaflet(mapa_regional(filters$database))
  })
}

#' 03_datMuestro UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_03_datMuestro_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' 03_datMuestro Server Functions
#'
#' @noRd 
mod_03_datMuestro_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_03_datMuestro_ui("03_datMuestro_1")
    
## To be copied in the server
# mod_03_datMuestro_server("03_datMuestro_1")

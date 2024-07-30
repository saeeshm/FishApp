#' 04_mapaReg UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_04_mapaReg_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' 04_mapaReg Server Functions
#'
#' @noRd 
mod_04_mapaReg_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_04_mapaReg_ui("04_mapaReg_1")
    
## To be copied in the server
# mod_04_mapaReg_server("04_mapaReg_1")

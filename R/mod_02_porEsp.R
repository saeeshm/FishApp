#' 02_porEsp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_02_porEsp_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' 02_porEsp Server Functions
#'
#' @noRd 
mod_02_porEsp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_02_porEsp_ui("02_porEsp_1")
    
## To be copied in the server
# mod_02_porEsp_server("02_porEsp_1")

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @noRd
app_server <- function(input, output, session) {
  # General page server logic function
  mod_01_multiEsp_server('multiEsp', region='Bahia de Tela')
  mod_02_porEsp_server('porEsp', region='Bahia de Tela')
  mod_03_datGen_server("datGen", region = 'Bahia de Tela')
  mod_04_mapaReg_server('mapaReg', region='Bahia de Tela')
}

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @noRd
app_server <- function(input, output, session) {
  # Generate text for description nav panel.
  output$desc_title = renderText(ifelse(input$lang == 'ES', "Descripción", "Description"))
  output$desc_ui = shiny::renderUI({
    build_desc(input$lang)
  })
  lang_r = reactive(input$lang)
  mod_01_multiEsp_server('multiEsp', lang = lang_r, region='Bahia de Tela')
  mod_02_porEsp_server('porEsp', lang = input$lang, region='Bahia de Tela')
  mod_03_datGen_server("datGen", lang = input$lang, region = 'Bahia de Tela')
  mod_04_mapaReg_server('mapaReg', lang = input$lang, region='Bahia de Tela')
}

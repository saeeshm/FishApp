#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @noRd
app_server <- function(input, output, session) {
  # Generate text for nav panel titles - alas, this must be done in the server here.
  output$desc_title = renderText(ifelse(input$lang == 'ES', "Descripción", "Description"))
  output$desc_ui = shiny::renderUI({
    build_desc(input$lang)
  })
  output$map_title = renderText(ifelse(input$lang == 'ES', "Mapa", "Map"))
  output$datGen_title = renderText(ifelse(input$lang == 'ES', "Datos Generales", "General Summaries"))
  output$multiEsp_title = renderText(ifelse(input$lang == 'ES', "Indicadores Multiespecíficos", "Multispecies Indicators"))
  output$porEsp_title = renderText(ifelse(input$lang == 'ES', "Indicadores Por Especie", "Indicators by Species"))
  
  # Make reactive form of language toggle; pass this pre-resolved reactive form into modules.
  lang_r = reactive(input$lang)
  
  # Because modals get summoned into a broader scope than the module that spawned them,
  # we need to close them at this level.
  observeEvent(input$close_modal_btn, {
    shiny::removeModal()
  })
  
  mod_01_multiEsp_server('multiEsp', lang = lang_r, region='Bahia de Tela')
  mod_02_porEsp_server('porEsp', lang = lang_r, region='Bahia de Tela')
  mod_03_datGen_server("datGen", lang = lang_r, region = 'Bahia de Tela')
  mod_04_mapaReg_server('mapaReg', lang = lang_r, region='Bahia de Tela')
}

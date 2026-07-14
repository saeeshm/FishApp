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
  output$map_title = renderText(ifelse(input$lang == 'ES', "Mapa", "Map"))
  output$datGen_title = renderText(ifelse(input$lang == 'ES', "Datos Generales", "General Summaries"))
  output$multiEsp_title = renderText(ifelse(input$lang == 'ES', "Indicadores Multiespecíficos", "Multispecies Indicators"))
  output$porEsp_title = renderText(ifelse(input$lang == 'ES', "Indicadores Por Especie", "Indicators by Species"))
  
  # Make reactive form of language toggle; pass this pre-resolved reactive form into modules.
  lang_r = reactive(input$lang)
  
  # Because modals get summoned into a broader scope than the module that
  # spawned them, we need to close them at this level.
  observeEvent(input$close_modal_btn, {
    shiny::removeModal()
  })
  
  # Shared sidebar ---------- 
  
  # Only the server function of this module is called at the root of the app, to
  # instantiate a singular shared sidebar for all pages. Individual UI elements
  # from this module, using the same namespace are then instantiated inside each
  # page that requires a sidebar, so that sidebar server state is shared between
  # all pages, even if the individual UI per-page is unique
  filter_state <- reactiveValues(
    database = 'Tela',
    years = get_year_periods('Tela')[c(1, length(get_year_periods('Tela')))],
    species = 'Calale'
  )
  # sidebar_filters = mod_06_sidebar_filters_server('shared_sidebar', lang = lang_r)
  
  # Using the utility function to render a pre-set description onto the
  # description page
  output$desc_ui = shiny::renderUI({build_desc(input$lang)})
  # Calling modules for remaining pages
  mod_01_multiEsp_server('multiEsp', lang = lang_r, filters = filter_state)
  mod_02_porEsp_server('porEsp', lang = lang_r, filters = filter_state)
  mod_03_datGen_server("datGen", lang = lang_r, filters = filter_state)
  mod_04_mapaReg_server('mapaReg', lang = lang_r, filters = filter_state)
}

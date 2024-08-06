#' 01_multiEsp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import ggplot2
#' @import data.table
mod_01_multiEsp_ui <- function(id){
  ns <- NS(id)
  layout_sidebar(
    # Inputs and descriptions in the sidebar
    sidebar = sidebar(
      position='left',
      # Text output describing currently focused region
      tags$h4('Region Elegido:'),
      textOutput(ns('region_desc')),
      
      # Date-range input
      tags$h4('Año de Muestreo'),
      shinyWidgets::sliderTextInput(
        ns('sampleyear'), 
        label=NULL,
        # Using only Month-year combos as valid input choices
        choices=unique(year(na.omit(fishdbase$fecha))),
        selected=range(year(na.omit(fishdbase$fecha)))
      )
    ),
    layout_column_wrap(
      width = 1/2,
      # Column-left - full sized plots with tabs
      navset_card_tab(
        full_screen = T,
        nav_panel(
          'LFQ',
          bslib::card_header("Frecuencias de longitud por arte"),
          bslib::card_body(plotOutput(ns('plot_lfq')))
        ),
        nav_panel(
          'Especies',
          bslib::card_header("Las 10 especies más capturadas"),
          bslib::card_body(plotOutput(ns('plot_esp')))
        ),
      ),
      # Column-right - half sized plots split into rows (this is nested inside
      # the above call to layout column wrap)
      layout_column_wrap(
        width = 1,
        plot_card('Espectro de densidad de biomasa normalizada', plotOutput(ns('plot_nbds'))),
        plot_card('Capturas por unidad de esfuerzo',plotOutput(ns('plot_cpue')))
      )
    )
  )
}
    
#' 01_multiEsp Server Functions
#'
#' @noRd 
mod_01_multiEsp_server <- function(id, region){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Adding the selected region description
    output$region_desc <- renderText(region)
    # LFQ by trophic level plot
    output$plot_lfq <- renderPlot(multiesp_lfq_plot(
      lfqdbase,
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2]
    ))
    # Top-captured species plot
    output$plot_esp <- renderPlot(multiesp_top10_plot(
      lfqdbase,
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2]
    ))
    # Normalized biomass density plot
    output$plot_nbds <- renderPlot(multiesp_bspect_plot(
      fishdbase,
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2]
    ))
    # CPUE plot
    output$plot_cpue <- renderPlot(multiesp_cpue_plot(
      lfqdbase,
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2]
    ))
  })
}
    
## To be copied in the UI
# mod_01_multiEsp_ui("01_multiEsp_1")
    
## To be copied in the server
# mod_01_multiEsp_server("01_multiEsp_1")

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
      tags$h4(textOutput(ns("region_title"))),
      textOutput(ns('region_desc')),
      # Date-range input
      tags$h4(textOutput(ns("sampleyear_title"))),
      uiOutput(ns("sampleyear_ui"))
    ),
    layout_column_wrap(
      width = 1/2,
      # Column-left - full sized plots with tabs
      uiOutput(ns("left_col_plots_ui")),
      # Column-right - half sized plots split into rows (this is nested inside
      # the above call to layout column wrap)
      layout_column_wrap(
        width = 1,
        uiOutput(ns('right_col_plot_top')),
        uiOutput(ns('right_col_plot_bottom'))
      )
    )
  )
}
    
#' 01_multiEsp Server Functions
#'
#' @noRd 
mod_01_multiEsp_server <- function(id, lang, region){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Adding the selected region description
    output$region_title <- renderText(ifelse(lang() == "ES","Región","Region"))
    output$region_desc <- renderText(region)
    # Slider input for year filter
    output$sampleyear_title <- renderText(ifelse(lang() == "ES","Año de Muestreo","Sampling Year(s)"))
    output$sampleyear_ui <- renderUI({
      shinyWidgets::sliderTextInput(
        ns('sampleyear'),
        label=NULL,
        # Using only Month-year combos as valid input choices
        choices=unique(year(na.omit(fishdbase$fecha))),
        selected=range(year(na.omit(fishdbase$fecha)))
      )
    })
    # LFQ by trophic level plot
    output$plot_lfq <- renderPlot(multiesp_lfq_plot(
      lfqdbase,
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2],
      lang()
    ))
    # Top-captured species plot
    output$plot_esp <- renderPlot(multiesp_top10_plot(
      lfqdbase,
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2],
      lang()
    ))
    # Normalized biomass density plot
    output$plot_nbds <- renderPlot(multiesp_bspect_plot(
      fishdbase,
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2],
      lang()
    ))
    # CPUE plot
    output$plot_cpue <- renderPlot(multiesp_cpue_plot(
      lfqdbase,
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2],
      lang()
    ))
    
    # Render UI for left column plots
    output$left_col_plots_ui = renderUI({
      if(lang() == "ES"){
        navset_card_tab(
          full_screen = T,
          nav_panel(
            'LFQ',
            bslib::card_header("Frecuencias de tallas por arte"),
            bslib::card_body(plotOutput(ns('plot_lfq')))
          ),
          nav_panel(
            'Especies',
            bslib::card_header("Las 10 especies más capturadas"),
            bslib::card_body(plotOutput(ns('plot_esp')))
          ),
        )
      } else {
        navset_card_tab(
          full_screen = T,
          nav_panel(
            'LFQ',
            bslib::card_header("Size Class Frequencies by Technique"),
            bslib::card_body(plotOutput(ns('plot_lfq')))
          ),
          nav_panel(
            'Species',
            bslib::card_header("The 10 Most Captured Species"),
            bslib::card_body(plotOutput(ns('plot_esp')))
          ),
        )
      }
    })
    
    # Render UI for right column plots
    output$right_col_plot_top = renderUI({
      plot_card(
        ifelse(
          lang() == 'ES',
          'Espectro de densidad de biomasa normalizada', 
          'Density Plot of Normalized Biomass'
        ),
        plotOutput(ns('plot_nbds')))
    })
    output$right_col_plot_bottom = renderUI({
      plot_card(
        ifelse(
          lang() == 'ES',
          'Capturas por unidad de esfuerzo', 
          'Captures per Unit of Effort'
        ),
        plotOutput(ns('plot_cpue')))
    })
  })
}
    
## To be copied in the UI
# mod_01_multiEsp_ui("01_multiEsp_1")
    
## To be copied in the server
# mod_01_multiEsp_server("01_multiEsp_1")

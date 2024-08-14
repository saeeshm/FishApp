#' 02_porEsp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import data.table
#' @import ggplot2
mod_02_porEsp_ui <- function(id){
  ns <- NS(id)
  layout_sidebar(
    # Inputs and descriptions in the sidebar
    sidebar = sidebar(
      position='left',
      # Text output describing currently focused region
      tags$h4('Region'),
      textOutput(ns('region_desc')),
      # Date-range input
      tags$h4('Año de Muestreo'),
      shinyWidgets::sliderTextInput(
        ns('sampleyear'), 
        label=NULL,
        # Using only Month-year combos as valid input choices
        choices=unique(year(na.omit(fishdbase$fecha))),
        selected=range(year(na.omit(fishdbase$fecha)))
      ),
      # Fish species input
      tags$h4('Especie'),
      shinyWidgets::pickerInput(
        ns('species'), 
        label = NULL, 
        choices = stringr::str_to_title(spectab$nombre_comun_cln),
        selected = 'Calale',
        choicesOpt = list(
          subtext = spectab$nombre_cientifico,
          sep = '     '
        ),
        options = list(
          `live-search` = TRUE)
      )
    ),
    # Column-left: Distributions by gear
    layout_column_wrap(
      width = 1/2,
      # LFQ and CPUE plots by gear as a card tab
      navset_card_tab(
        full_screen = T,
        nav_panel(
          'LFQ',
          bslib::card_header("Frecuencias de tallas por arte"),
          bslib::card_body(plotOutput(ns('plot_lfq')))
        ),
        nav_panel(
          'CPUE',
          bslib::card_header("Capturas por unidad de esfuerzo por arte"),
          bslib::card_body(plotOutput(ns('plot_cpue_arte')))
        ),
      ),
      # Column-right - half sized plots split into rows
      layout_column_wrap(
        width = 1,
        plot_card('Estacionalidad de capturas', plotOutput(ns('plot_est'))),
        navset_card_tab(
          full_screen = T,
          nav_panel(
            'CPUE (Total)',
            bslib::card_header("Capturas por unidad de esfuerzo (total)"),
            bslib::card_body(plotOutput(ns('plot_cpue_total')))
          ),
          nav_panel(
            'CPUE (Longitud)',
            bslib::card_header("Capturas por unidad de esfuerzo (por clase de longitud)"),
            bslib::card_body(plotOutput(ns('plot_cpue_pl')))
          )
        )
      )
    )
  )
}
    
#' 02_porEsp Server Functions
#'
#' @noRd 
mod_02_porEsp_server <- function(id, region){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Adding the selected region description
    output$region_desc <- renderText(region)
    # Length-frequencies by gear plot
    output$plot_lfq <- renderPlot(ss_lfq_plot(
      lfqdbase,
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2],
      currspec = input$species
    ))
    # CPUE total plot
    output$plot_cpue_total <- renderPlot(ss_cpue_plot(
      lfqdbase,
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2],
      currspec = input$species,
      bygear = F
    ))
    # CPUE by gear plot
    output$plot_cpue_arte <- renderPlot(ss_cpue_plot(
      lfqdbase,
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2],
      currspec = input$species,
      bygear = T
    ))
    # CPUE per length-class plot
    output$plot_cpue_pl <- renderPlot(ss_plot_cpue_pl(
      lfqdbase,
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2],
      currspec = input$species
    ))
    # Catch-seasonality plot
    output$plot_est <- renderPlot(ss_plot_ssn(
      lfqdbase,
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2],
      currspec = input$species
    ))
  })
}
    
## To be copied in the UI
# mod_02_porEsp_ui("02_porEsp_1")
    
## To be copied in the server
# mod_02_porEsp_server("02_porEsp_1")

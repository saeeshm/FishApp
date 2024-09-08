#' 03_datMuestro UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_03_datGen_ui <- function(id){
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
      )
    ),
    layout_column_wrap(
      width = 1/2,
      plot_card('Peso muestreado',plotOutput(ns('plot_muest'))),
      plot_card('Estacionalidad de capturas',plotOutput(ns('plot_ssn')))
    )
    
    # Column-left: Distributions by gear
    # plot_card('Peso muestreado',plotOutput(ns('plot_muest'))),
    # plot_card('Estacionalidad de capturas',plotOutput(ns('plot_ssn')))
  )
}
    
#' 03_datMuestro Server Functions
#'
#' @noRd 
mod_03_datGen_server <- function(id, region){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Adding the selected region description
    output$region_desc <- renderText(region)
    # LFQ by trophic level plot
    output$plot_muest <- renderPlot(dg_plot_peso(
      fishdbase, 
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2]
    ))
    # Top-captured species plot
    output$plot_ssn <- renderPlot(dg_plot_ssn(
      fishdbase,
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2]
    ))
  })
}
    
## To be copied in the UI
# mod_03_datGen_ui("03_datMuestro_1")
    
## To be copied in the server
# mod_03_datGen_server("03_datMuestro_1")

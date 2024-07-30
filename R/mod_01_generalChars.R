 #' 01_generalChars UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bslib card
mod_01_generalChars_ui <- function(id){
  ns <- NS(id)
  layout_sidebar(
    # Inputs and descriptions in the sidebar
    sidebar = sidebar(
      position='left',
      # Text output describing currently focused region
      tags$h4('Region Elegido:'),
      textOutput(ns('region_desc')),
      # Text output describing currently focused region
      tags$h4('Comunidad Elegido:'),
      textOutput(ns('comunidad_desc')),
      tags$br(),
      
      # Unit-selection input
      tags$h4('Controles gráficos'),
      tags$h5('Unidad de muestreo'),
      shinyWidgets::switchInput(
        ns('yIsPeso'), 
        value=TRUE,
        onLabel='Peso',
        offLabel = 'Número'
      ),
      # Date-range input
      tags$h5('Año de Muestreo'),
      shinyWidgets::sliderTextInput(
        ns('sampleyear'), 
        label=NULL,
        # Using only Month-year combos as valid input choices
        choices=unique(year(na.omit(fishdbase$fecha)))
      )
    ),
    # Top-row 
    layout_column_wrap(
      width = 1/2,
      # 1 full-width card: species plot
      plot_card('Especies mas capturadas', plotOutput(ns('plot_espc'))),
      plot_card('Tallas promedios', plotOutput(ns('plot_long')))
    ),
    # Bottom row (2, 1/2 width cards: gear and seasonality plots)
    layout_column_wrap(
      width = 1/2,
      plot_card('Estacionalid de pesca',plotOutput(ns('plot_ssnl'))),
      plot_card('Capturas por arte de pesca',plotOutput(ns('plot_arte')))
    )
  )
}

#' 01_generalChars Server Functions
#'
#' @noRd 
mod_01_generalChars_server <- function(id, comunidad, region){
  moduleServer(id, function(input, output, session){
    # ns <- session$ns
    # Region description text output
    output$region_desc <- renderText(region)
    # Community description text output
    output$comunidad_desc <- renderText(comunidad)
    
    # Top-10 species plot (top-left)
    output$plot_espc <- renderPlot(make_plot(
      fishdbase, 
      plot_type='species', 
      yIsPeso=input$yIsPeso, 
      comunidad=comunidad, 
      sample_year = input$sampleyear
    ))
    
    # Mean length of top-10 species plot (top-right)
    output$plot_long <- renderPlot(make_plot(
      fishdbase, 
      plot_type='length', 
      yIsPeso=input$yIsPeso, 
      comunidad=comunidad, 
      sample_year = input$sampleyear
    ))
    
    # Catch-seasonality plot (bottom-left)
    output$plot_ssnl <- renderPlot(make_plot(
      fishdbase, 
      plot_type='seasonality', 
      yIsPeso=input$yIsPeso, 
      comunidad=comunidad, 
      sample_year = input$sampleyear
    ))
    
    # Catches by gear plot (bottom-right)
    output$plot_arte <- renderPlot(make_plot(
      fishdbase, 
      plot_type='gear', 
      yIsPeso=input$yIsPeso, 
      comunidad=comunidad, 
      sample_year = input$sampleyear
    ))
  })
}
    
## To be copied in the UI
# mod_01_generalChars_ui("01_generalChars_1")
    
## To be copied in the server
# mod_01_generalChars_server("01_generalChars_1")

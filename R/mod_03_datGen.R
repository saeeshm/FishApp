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
  
  # Preparing UI Objects ----------
  
  # Creating a list of value boxes for summary statistics
  vbs <- list(
    bslib::value_box(
      # fill=F,
      min_width = "300px",
      # min_height = '150px',
      theme='text-indigo',
      title = 'Numero de muestreos',
      value = textOutput(ns('num_muestreos')),
      showcase = fontawesome::fa_i("pen-to-square")
    ),
    bslib::value_box(
      # fill=F,
      min_width = "300px",
      # min_height = '150px',
      theme = 'text-orange',
      title = 'Dias muestreado',
      value = textOutput(ns('num_dias_muestreo')),
      showcase = fontawesome::fa_i("calendar-days")
    ),
    bslib::value_box(
      # fill=F,
      min_width = "300px",
      # min_height = '150px',
      theme = 'text-blue',
      title = 'Comunidades muestreadas',
      value = textOutput(ns('num_comunidades')),
      showcase = fontawesome::fa_i("map-location-dot")
      # p(textOutput(ns('nombres_comunidades')))
    ),
    bslib::value_box(
      # fill=F,
      min_width = "300px",
      theme = 'text-teal',
      # min_height = '150px',
      title = 'Peso total muestreado',
      value = textOutput(ns('peso_muestreado')),
      showcase = fontawesome::fa_i("scale-unbalanced")
    ),
    bslib::value_box(
      # fill=F,
      min_width = "300px",
      # min_height = '150px',
      theme = 'text-pink',
      title = 'Especies muestreadas',
      value = textOutput(ns('num_especies')),
      showcase = fontawesome::fa_i("fish-fins")
    ),
    bslib::value_box(
      # fill=F,
      min_width = "300px",
      theme = 'text-purple',
      # min_height = '150px',
      title = 'Individuos muestreados',
      value = textOutput(ns('num_individuos')),
      showcase = fontawesome::fa_i("ruler")
    )
  )
  
  # UI Layout ----------
  
  # Sidebar
  bslib::layout_sidebar(
    # Inputs and descriptions in the sidebar
    sidebar = bslib::sidebar(
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
    # Main body
    bslib::layout_column_wrap(
      width = 1,
      fill = F,
      height = NULL,
      heights_equal = 'row',
      # Row 1: 3 value boxes
      bslib::layout_column_wrap(
        fill=F,
        !!!vbs[1:3]
      ),
      # Row 2: 3 value boxes
      layout_column_wrap(
        fill=F,
        !!!vbs[4:6]
      ),
      # Row 3: Short plots
      layout_column_wrap(
        width = '400px',
        fill = F,
        # Plot 1 - Top 10-species
        bslib::card(
          fill = T,
          full_screen = TRUE,
          min_height = '600px',
          bslib::card_header('Especies más capturadas'),
          # PLACEHOLDER PLOT - UPDATE THIS
          bslib::card_body(plotOutput(ns('plot_esp_comunes'))),
        ),
        # Plot 2 - Gears
        bslib::card(
          fill = T,
          full_screen = TRUE,
          min_height = '600px',
          bslib::card_header('Artes de pesca utilizada'),
          # PLACEHOLDER PLOT - UPDATE THIS
          bslib::card_body(plotOutput(ns('plot_artes')))
        )
      ),
     # Row 4: Long plots
      layout_column_wrap(
        width = '400px',
        # Plot 1 - Sampled weight by community and gear
        bslib::card(
          fill = T, 
          min_height = '600px',
          full_screen = TRUE,
          bslib::card_header('Peso muestreado por comunidad'),
          bslib::card_body(plotOutput(ns('plot_muest')))
        ),
        # Plot 2 - Catch seasonality by community
        bslib::card(
          fill = T,
          min_height = '600px',
          full_screen = TRUE,
          bslib::card_header('Estacionalidad de capturas'),
          bslib::card_body(plotOutput(ns('plot_ssn')))
        )
      )
    )
  )
}

#' 03_datMuestro Server Functions
#'
#' @noRd 
mod_03_datGen_server <- function(id, region){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Sidebar elements ----------
    
    # Adding the selected region description
    output$region_desc <- renderText(region)
    
    # Main body elements ----------
    
    # Summary statistics
    ss <- reactive(dg_summ_stats(
      fishdbase, 
      input$sampleyear[1], 
      input$sampleyear[2]
    ))
    output$num_muestreos <- renderText(ss()$num_muestreos)
    output$num_dias_muestreo <- renderText(ss()$num_dias_muestreo)
    output$num_comunidades <- renderText(ss()$num_comunidades)
    output$nombres_comunidades <- renderText(ss()$nombres_comunidades)
    output$peso_muestreado <- renderText(ss()$peso_muestreado)
    output$num_especies <- renderText(ss()$num_especies)
    output$num_individuos <- renderText(ss()$num_individuos)
    
    # Top 10 captured species data
    esp_comunies <- reactive(dg_plot_esp_comunes(
      fishdbase, 
      input$sampleyear[1], 
      input$sampleyear[2]
    ))
    output$plot_esp_comunes <- renderPlot(esp_comunies()$top10_plot)
    output$prop_peso_esp_comunes <- renderText(esp_comunies()$prop_capt)
    
    # Gear usage
    output$plot_artes <- renderPlot(dg_plot_artes(
      fishdbase, 
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2]
    ))
    
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

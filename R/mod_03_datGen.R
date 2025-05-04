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
      title = textOutput(ns('num_muestreos_title')),
      value = textOutput(ns('num_muestreos')),
      showcase = fontawesome::fa_i("pen-to-square")
    ),
    bslib::value_box(
      # fill=F,
      min_width = "300px",
      # min_height = '150px',
      theme = 'text-orange',
      title = textOutput(ns('num_dias_muestreo_title')),
      value = textOutput(ns('num_dias_muestreo')),
      showcase = fontawesome::fa_i("calendar-days")
    ),
    bslib::value_box(
      # fill=F,
      min_width = "300px",
      # min_height = '150px',
      theme = 'text-blue',
      title = textOutput(ns('num_comunidades_title')),
      value = textOutput(ns('num_comunidades')),
      showcase = fontawesome::fa_i("map-location-dot")
      # p(textOutput(ns('nombres_comunidades')))
    ),
    bslib::value_box(
      # fill=F,
      min_width = "300px",
      theme = 'text-teal',
      # min_height = '150px',
      title = textOutput(ns('peso_muestreado_title')),
      value = textOutput(ns('peso_muestreado')),
      showcase = fontawesome::fa_i("scale-unbalanced")
    ),
    bslib::value_box(
      # fill=F,
      min_width = "300px",
      # min_height = '150px',
      theme = 'text-pink',
      # title = 'Especies muestreadas',
      title = textOutput(ns('num_especies_title')),
      value = textOutput(ns('num_especies')),
      showcase = fontawesome::fa_i("fish-fins")
    ),
    bslib::value_box(
      # fill=F,
      min_width = "300px",
      theme = 'text-purple',
      # min_height = '150px',
      title = textOutput(ns('num_individuos_title')),
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
      # tags$h4('Región'),
      tags$h4(textOutput(ns('region_title'))),
      textOutput(ns('region_desc')),
      # Date-range input
      # tags$h4('Año de Muestreo'),
      tags$h4(textOutput(ns('sampleyear_title'))),
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
      bslib::layout_column_wrap(
        fill=F,
        !!!vbs[4:6]
      ),
      # Row 3: Short plots
      bslib::layout_column_wrap(
        width = '400px',
        fill = F,
        # Plot 1 - Top 10-species
        bslib::card(
          fill = T,
          full_screen = TRUE,
          min_height = '600px',
          # bslib::card_header('Especies más capturadas'),
          bslib::card_header(textOutput(ns('plot_esp_comunes_title'))),
          # PLACEHOLDER PLOT - UPDATE THIS
          bslib::card_body(plotOutput(ns('plot_esp_comunes'))),
        ),
        # Plot 2 - Gears
        bslib::card(
          fill = T,
          full_screen = TRUE,
          min_height = '600px',
          # bslib::card_header('Artes de pesca utilizada'),
          bslib::card_header(textOutput(ns('plot_artes_title'))),
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
          # bslib::card_header('Peso muestreado por comunidad'),
          bslib::card_header(textOutput(ns('plot_muest_title'))),
          bslib::card_body(plotOutput(ns('plot_muest')))
        ),
        # Plot 2 - Catch seasonality by community
        bslib::card(
          fill = T,
          min_height = '600px',
          full_screen = TRUE,
          # bslib::card_header('Estacionalidad de capturas'),
          bslib::card_header(textOutput(ns('plot_ssn_title'))),
          bslib::card_body(plotOutput(ns('plot_ssn')))
        )
      )
    )
  )
}

#' 03_datMuestro Server Functions
#'
#' @noRd 
mod_03_datGen_server <- function(id, lang, region){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Sidebar elements ----------
    
    # Adding the selected region description
    output$region_title <- renderText(ifelse(lang() == "ES","Región","Region"))
    output$region_desc <- renderText(region)
    
    output$sampleyear_title <- renderText(ifelse(lang() == "ES","Año de Muestreo","Sampling Year(s)"))
    # Main body elements ----------
    
    # Summary statistics
    ss <- reactive(dg_summ_stats(
      fishdbase, 
      input$sampleyear[1], 
      input$sampleyear[2]
    ))
    
    # Render up them titles!
    output$num_muestreos_title <- renderText(ifelse(lang() == 'ES','Número de muestreos','Number of Samples'))
    output$num_dias_muestreo_title <- renderText(ifelse(lang() == 'ES','Dias muestreado','Days Sampled'))
    output$num_comunidades_title <- renderText(ifelse(lang() == 'ES','Comunidades muestreadas','Communities Sampled'))
    output$peso_muestreado_title <- renderText(ifelse(lang() == 'ES','Peso total muestreado','Total Weight Sampled'))
    output$num_especies_title <- renderText(ifelse(lang() == 'ES','Especies muestreadas','Species Sampled'))
    output$num_individuos_title <- renderText(ifelse(lang() == 'ES','Individuos muestreados','Individuals Sampled'))
    
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
      input$sampleyear[2],
      lang
    ))
    output$plot_esp_comunes_title = renderText(ifelse(lang() == "ES","Especies más capturadas","Most Commonly Caught Species"))
    output$plot_esp_comunes <- renderPlot(esp_comunies()$top10_plot)
    output$prop_peso_esp_comunes <- renderText(esp_comunies()$prop_capt)
    
    # Gear usage
    output$plot_artes_title <- renderText(ifelse(lang() == "ES","Artes de pesca utilizada","Fishing Technique Used"))
    output$plot_artes <- renderPlot(dg_plot_artes(
      fishdbase, 
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2],
      lang
    ))
    
    # LFQ by trophic level plot
    output$plot_muest_title <- renderText(ifelse(lang() == "ES","Peso muestreado por comunidad","Weight Sampled by Community"))
    output$plot_muest <- renderPlot(dg_plot_peso(
      fishdbase, 
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2],
      lang
    ))
    # Top-captured species plot
    output$plot_ssn_title <- renderText(ifelse(lang() == "ES","Estacionalidad de capturas","Seasonality of Catches"))
    output$plot_ssn <- renderPlot(dg_plot_ssn(
      fishdbase,
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2],
      lang
    ))
  })
}
    
## To be copied in the UI
# mod_03_datGen_ui("03_datMuestro_1")
    
## To be copied in the server
# mod_03_datGen_server("03_datMuestro_1")

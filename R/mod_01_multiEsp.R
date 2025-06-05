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
  
  # Preparing UI objects ----------
  
  # Creating a list of value boxes for summary statistics
  vbs <- list(
    # Species richness
    bslib::value_box(
      # fill=F,
      # min_width = "300px",
      # min_height = '150px',
      theme='text-indigo',
      title = textOutput(ns('spec_rich_title')),
      value = textOutput(ns('spec_rich')),
      showcase = fontawesome::fa_i("fish-fins")
    ),
    # Species diversity (Shannon Index)
    bslib::value_box(
      # fill=F,
      # min_width = "300px",
      theme = 'text-teal',
      # min_height = '150px',
      title = textOutput(ns('spec_div_title')),
      value = textOutput(ns('spec_div')),
      showcase = fontawesome::fa_i("heading")
    ),
    # Species Equitability Index
    bslib::value_box(
      # fill=F,
      # min_width = "300px",
      # min_height = '150px',
      theme = 'text-blue',
      title = textOutput(ns('spec_eq_title')),
      value = textOutput(ns('spec_eq')),
      showcase = fontawesome::fa_i("scale-balanced")
      # p(textOutput(ns('nombres_comunidades')))
    )
  )
  
  # UI Layout ----------
  
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
    bslib::layout_column_wrap(
      width = 1,
      fill=F,
      heights_equal = 'row',
      # Row 1 (Plot cards and value boxes)
      bslib::layout_column_wrap(
        width = '300px',
        fixed_width = F,
        # Column 1 (Total CPUE trend)
        bslib::layout_column_wrap(
          min_height='400px',
          max_height = '400px',
          fill=T,
          bslib::card(
            fill = T,
            full_screen = TRUE,
            bslib::card_header(textOutput(ns('plot_cpue_total_title'))),
            bslib::card_body(plotOutput(ns('plot_cpue_total'))),
            bslib::card_footer(textOutput(ns('plot_cpue_total_desc')))
          )
        ),
        # Column 2 (Indicator value boxes)
        bslib::layout_column_wrap(
          min_height='400px',
          max_height = '400px',
          !!!vbs
        ),
        # Column 3 (Biomass density plot)
        bslib::layout_column_wrap(
          min_height='400px',
          max_height = '400px',
          fill=T,
          bslib::card(
            fill = T,
            full_screen = TRUE,
            bslib::card_header(textOutput(ns('plot_bdens_title'))),
            bslib::card_body(plotOutput(ns('plot_bdens'))),
            bslib::card_footer(textOutput(ns('plot_bdens_desc')))
          )
        ),
      ),
      # Row 2 (Ancilliary large-size plots)
      bslib::layout_column_wrap(
        width = '450px',
        fixed_width=F,
        fill=T,
        # Column-left: LFQ-Distributions by gear and trophic level
        bslib::layout_column_wrap(
          width = '450px',
          fixed_width=F,
          max_height = '600px',
          bslib::card(
            fill = T, 
            full_screen = TRUE,
            bslib::card_header(textOutput(ns('plot_lfq_title'))),
            bslib::card_body(plotOutput(ns('plot_lfq')))
          )
        ),
        # Column-right deviance plot of captured weight
        bslib::layout_column_wrap(
          width = '450px',
          fixed_width=F,
          max_height = '600px',
          bslib::card(
            fill = T,
            full_screen = TRUE,
            # bslib::card_header('Estacionalidad de capturas'),
            bslib::card_header(textOutput(ns('plot_top10_dev_title'))),
            bslib::card_body(plotOutput(ns('plot_top10_dev')))
          )
        )
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
    
    # Input objects ----------
    
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
    
    # Output objects ----------
    
    # Multispecies CPUE trend
    cpue <- reactive(ms_cpue_plot(
      lfqdbase=lfqdbase,
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2],
      lang = lang()
    ))
    output$plot_cpue_total_title <- renderText(ifelse(
      lang() == 'ES',
      'Tendencia de capturas por unidad de esfuerzo',
      'Trend of catch per unit effort CPUE Trend'
    ))
    output$plot_cpue_total_desc <- renderText(cpue()$cpue_desc)
    output$plot_cpue_total <- renderPlot(cpue()$cpue_plot)
    
    # Value boxes (getting relevant diversity indices)
    di <- reactive(ms_div_indices(
      lfqdbase = lfqdbase,
      input$sampleyear[1], 
      input$sampleyear[2]
    ))
    # Species richness
    output$spec_rich_title <- renderText(ifelse(
      lang() == 'ES',
      'Riqueza de especies',
      'Species Richness'
    ))
    output$spec_rich <- renderText(di()$rich)
    # Species diversity
    output$spec_div_title <- renderText(ifelse(
      lang() == 'ES',
      'Diversidad de especies (Índice de Shannon)',
      'Species Diversity (Shannon Index)'
    ))
    output$spec_div <- renderText(di()$shannon)
    # Species equitability
    output$spec_eq_title <- renderText(ifelse(
      lang() == 'ES',
      'Equitabilidad de especies',
      'Species Equitability'
    ))
    output$spec_eq <- renderText(di()$eq)
    
    # Biomass density spectrum plot
    bd <- reactive(ms_bdens_plot(
      fishdbase = fishdbase,
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2],
      lang = lang()
    ))
    output$plot_bdens_title <- renderText(ifelse(
      lang() == 'ES',
      'Espectro de densidad de biomasa normalizada',
      'Normalized biomass density spectrum'
    ))
    output$plot_bdens_desc <- renderText(bd()$bd_desc)
    output$plot_bdens <- renderPlot(bd()$bd_plot)
    
    # Length-frequency per gear and trophic level plot
    output$plot_lfq_title <- renderText(ifelse(
      lang() == 'ES',
      'Frecuencias de tallas por arte y nivel trófico',
      'Size Class Frequencies by Technique and Trophic Level'
    ))
    output$plot_lfq <- renderPlot(ms_lfq_plot(
      lfqdbase,
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2],
      lang()
    ))
    
    # Top-10 species captured as deviances plot 
    output$plot_top10_dev_title <- renderText(ifelse(
      lang() == 'ES',
      'Peso capturado por especie con respecto a la media (las 10 especies más capturadas)',
      'Captured weight by species relative to mean (10-most caught species)'
    ))
    output$plot_top10_dev <- renderPlot(ms_top10_plot(
      lfqdbase,
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2],
      lang()
    ))
  })
}

## To be copied in the UI
# mod_01_multiEsp_ui("01_multiEsp_1")

## To be copied in the server
# mod_01_multiEsp_server("01_multiEsp_1")

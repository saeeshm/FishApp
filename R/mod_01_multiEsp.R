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
        width = '450px',
        fixed_width = F,
        # Column 1 (Total CPUE trend)
        bslib::layout_column_wrap(
          min_height='400px',
          max_height = '450px',
          fill=T,
          plot_card_labelled(
            plot = plotOutput(ns('cpue_plot')),
            bottom_label = shiny::uiOutput(ns('cpue_mean_desc')),
            card_title = modal_UI(ns('cpue_plot_title'), 'test'),
            top_label = shiny::uiOutput(ns('cpue_trend_desc'))
          )
        ),
        # Column 2 (Diversity indicator timeseries plots)
        bslib::layout_column_wrap(
          min_height='400px',
          max_height = '450px',
          fill=T,
          # Creating all plots and descriptions as a navset panel
          bslib::navset_card_underline(
            full_screen = TRUE,
            # Species richness
            bslib::nav_panel(
              title = modal_UI(ns('div_rich_title'), 'test'),
              plot_card_labelled(
                plot = plotOutput(ns('div_rich_plot')),
                bottom_label = shiny::uiOutput(ns('div_rich_ov_desc')),
                card_title = NULL,
                top_label = shiny::uiOutput(ns('div_rich_trend_desc'))
              )
            ),
            # Species diversity
            bslib::nav_panel(
              title = modal_UI(ns('div_shannon_title'), 'test'),
              plot_card_labelled(
                plot = plotOutput(ns('div_shannon_plot')),
                bottom_label = shiny::uiOutput(ns('div_shannon_ov_desc')),
                card_title = NULL,
                top_label = shiny::uiOutput(ns('div_shannon_trend_desc'))
              )
            ),
            # Species equitability
            bslib::nav_panel(
              title = modal_UI(ns('div_eq_title'), 'test'),
              plot_card_labelled(
                plot = plotOutput(ns('div_eq_plot')),
                bottom_label = shiny::uiOutput(ns('div_eq_ov_desc')),
                card_title = NULL,
                top_label = shiny::uiOutput(ns('div_eq_trend_desc'))
              )
            )
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
          min_height = '700px',
          bslib::card(
            fill = T, 
            full_screen = TRUE,
            # bslib::card_header(textOutput(ns('plot_lfq_title'))),
            bslib::card_header(modal_UI(ns('plot_lfq_title'),'Frecuencias de tallas por arte y nivel trófico')),
            bslib::card_body(plotOutput(ns('plot_lfq')))
          )
        ),
        # Column-right deviance plot and biomass density plot
        bslib::layout_column_wrap(
          width = '450px',
          fixed_width=F,
          # Biomass density
          min_height = '700px',
          bslib::card(
            fill = T,
            full_screen = TRUE,
            bslib::card_header(modal_UI(ns('plot_bdens_title'),'Espectro de densidad de biomasa normalizada')),
            bslib::card_body(plotOutput(ns('plot_bdens')))
            # bslib::card_footer(textOutput(ns('plot_bdens_desc')))
          ),
          # Top-10 species captured as deviances plot
          bslib::card(
            fill = T,
            full_screen = TRUE,
            bslib::card_header(modal_UI(ns('plot_top10_dev_title'),'Peso capturado por especie con respecto a la media (las 10 especies más capturadas)')),
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
      lfqdb=lfqdbase,
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2],
      lang = lang()
    ))
    modal_Server(
      'cpue_plot_title', lang,
      plot_title=c('Tendencia de capturas por unidad de esfuerzo','Trend of catch per unit effort CPUE Trend'),
      plot_info=c("Información sobre el gráfico","Information about the plot")
    )
    output$cpue_trend_desc <- renderUI(cpue()$cpue_trend_desc)
    output$cpue_plot <- renderPlot(cpue()$cpue_plot)
    output$cpue_mean_desc <- renderUI(cpue()$cpue_mean_desc)
    
    # Calling helper function to generate all diversity index UI objects
    div <- reactive(ms_div_ts_plots(
      lfqdb = lfqdbase,
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2],
      lang = lang()
    ))
    # Species richness
    modal_Server('div_rich_title',lang,
                 plot_title=c('Riqueza','Richness'),
                 plot_info=c("Información sobre el gráfico","Information about the plot"))
    output$div_rich_trend_desc <- renderUI(div()$rich$trend_desc)
    output$div_rich_plot <- renderPlot(div()$rich$plot)
    output$div_rich_ov_desc <- renderUI(div()$rich$ov_desc)
    
    # Species diversity
    modal_Server('div_shannon_title',lang,
                 plot_title=c('Diversidad (Shannon)','Diversity (Shannon)'),
                 plot_info=c("Información sobre el gráfico","Information about the plot"))
    output$div_shannon_trend_desc <- renderUI(div()$shannon$trend_desc)
    output$div_shannon_plot <- renderPlot(div()$shannon$plot)
    output$div_shannon_ov_desc <- renderUI(div()$shannon$ov_desc)
    
    # Species equitability
    modal_Server('div_eq_title',lang,
                 plot_title=c('Equitatividad','Equitability'),
                 plot_info=c("Información sobre el gráfico","Information about the plot"))
    output$div_eq_trend_desc <- renderUI(div()$eq$trend_desc)
    output$div_eq_plot <- renderPlot(div()$eq$plot)
    output$div_eq_ov_desc <- renderUI(div()$eq$ov_desc)
    
    # Biomass density spectrum plot
    bd <- reactive(ms_bdens_plot(
      fdb = fishdbase,
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2],
      lang = lang()
    ))
    modal_Server('plot_bdens_title',lang,
                 plot_title=c('Espectro de densidad de biomasa normalizada','Normalized biomass density spectrum'),
                 plot_info=c("Información sobre el gráfico","Information about the plot"))
    output$plot_bdens_desc <- renderText(bd()$bd_desc)
    output$plot_bdens <- renderPlot(bd()$bd_plot)
    
    # Length-frequency per gear and trophic level plot
    modal_Server('plot_lfq_title',lang,
                 plot_title=c('Frecuencias de tallas por arte y nivel trófico','Size Class Frequencies by Technique and Trophic Level'),
                 plot_info=c("Información sobre el gráfico","Information about the plot"))
    output$plot_lfq <- renderPlot(ms_lfq_plot(
      lfqdb = lfqdbase,
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2],
      lang()
    ))
    
    # Top-10 species captured as deviances plot 
    modal_Server('plot_top10_dev_title',lang,
                 plot_title=c('Peso capturado por especie con respecto a la media (las 10 especies más capturadas)',
                              'Captured weight by species relative to mean (10-most caught species)'),
                 plot_info=c("Información sobre el gráfico","Information about the plot"))
    output$plot_top10_dev <- renderPlot(ms_top10_plot(
      lfqdb = lfqdbase,
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

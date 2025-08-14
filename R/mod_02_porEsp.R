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
  
  # Preparing UI objects ----------
  
  # Creating a list of value boxes for summary statistics
  vbs <- list(
    # Total number captured
    bslib::value_box(
      theme='text-indigo',
      class = 'vb-no-pad',
      title = modal_UI(ns('num_capturado_title'),'Número de pescas capturadas'),
      value = textOutput(ns('num_capturado')),
      showcase = fontawesome::fa_i("pen-to-square")
    ),
    # Total weight captured
    bslib::value_box(
      theme = 'text-teal',
      class = 'vb-no-pad',
      title = modal_UI(ns('peso_capturado_title'),'Peso capturado (kg)'),
      value = textOutput(ns('peso_capturado')),
      showcase = fontawesome::fa_i("scale-unbalanced")
    ),
    # Proportion of captures that are close to the length at maturity
    bslib::value_box(
      theme = 'text-orange',
      class = 'vb-no-pad',
      title = modal_UI(ns('prop_capturas_matr_title'),'Proporción de capturas cerca de la talla de madurez'),
      value = textOutput(ns('prop_capturas_matr')),
      showcase = fontawesome::fa_i("ruler")
    ),
    # Proportion of captures that were immature
    bslib::value_box(
      theme = 'text-pink',
      class = 'vb-no-pad',
      title = modal_UI(ns('prop_capturas_immat_title'),'Proporción de capturas inmaduras'),
      value = textOutput(ns('prop_capturas_immat')),
      showcase = fontawesome::fa_i("ruler")
    ),
    
    # Proportion of captures that are mega-spawners
    bslib::value_box(
      theme = 'text-blue',
      class = 'vb-no-pad',
      title = modal_UI(ns('prop_capturas_mspawn_title'),'Proporción de capturas que fueron mega-reproductadores'),
      value = textOutput(ns('prop_capturas_mspawn')),
      showcase = fontawesome::fa_i("fish-fins")
    )
  )
  
  # UI Layout ----------
  
  layout_sidebar(
    # Inputs and descriptions in the sidebar
    sidebar = sidebar(
      position='left',
      # Text output describing currently focused region
      tags$h4(textOutput(ns('region_title'))),
      textOutput(ns('region_desc')),
      # Date-range input
      tags$h4(textOutput(ns('sampleyear_title'))),
      shinyWidgets::sliderTextInput(
        ns('sampleyear'), 
        label=NULL,
        # Using only Month-year combos as valid input choices
        choices=unique(year(na.omit(fishdbase$fecha))),
        selected=range(year(na.omit(fishdbase$fecha)))
      ),
      # Fish species input
      tags$h4(textOutput(ns('species_title'))),
      shinyWidgets::pickerInput(
        ns('species'), 
        label = NULL, 
        choices = stringr::str_to_title(spectab$nombre_comun_cln),
        selected = 'Calale',
        choicesOpt = list(
          subtext = spectab$nombre_cientifico,
          sep = ' '
        ),
        options = list(
          `live-search` = TRUE,
          `container` = 'body')
      )
    ),
    # Main body
    bslib::layout_column_wrap(
      width=1,
      fill=F,
      heights_equal = 'row',
      # Row 1 - 2-column box layout
      bslib::layout_column_wrap(
        width = '450px',
        # width = NULL,
        # style = css(grid_template_columns = "2.5fr 1.75fr 1.75fr"),
        # style = css(grid_template_columns = "minmax(450px, 3fr) minmax(300px, 2fr)"),
        # style = css(grid_template_columns = "repeat(auto-fit, minmax(500px, 1fr))"),
        fixed_width=F,
        # Column 1 - CPUE plot card
        bslib::layout_column_wrap(
          width = '450px',
          # style = css(grid_column = "span 3"),
          min_height='400px',
          max_height = '450px',
          fill=T,
          plot_card_labelled(
            plot = plotOutput(ns('cpue_plot')),
            bottom_label = shiny::uiOutput(ns('cpue_mean_desc')),
            card_title = modal_UI(ns('cpue_plot_title'),'default'),
            top_label = shiny::uiOutput(ns('cpue_trend_desc'))
          )
        ),
        # Column 2 - LFQ Froese Indicators
        bslib::layout_column_wrap(
          width = '300px',
          # style = css(grid_column = "span 1"),
          min_height='400px',
          max_height = '450px',
          bslib::card(
            fill = T,
            full_screen = TRUE,
            # min_width = '300px',
            # max_width = '350px',
            bslib::card_header(textOutput(ns('froese_indics_title'))),
            bslib::card_body(
              padding = '0.1rem',
              !!!vbs[3:5]
            )
          )
        )
      ),
      # Row 2 - Weight and number cards
      bslib::layout_column_wrap(
        width = '450px',
        fixed_width = F,
        fill = T,
          min_height='150px',
          max_height = '200px',
        !!!vbs[1:2]
      ),
      # Row 3 - supplementary plots
      bslib::layout_column_wrap(
        width = '450px',
        fixed_width=F,
        fill=T,
        # Column-left: LFQ-Distributions by gear
        bslib::layout_column_wrap(
          width = '450px',
          fixed_width=F,
          min_height = '700px',
          bslib::card(
            fill = T,
            full_screen = TRUE,
            # bslib::card_header(textOutput(ns('plot_lfq_title'))),
            bslib::card_header(modal_UI(ns('plot_lfq_title'),'Frecuencias de tallas por arte')),
            bslib::card_body(plotOutput(ns('plot_lfq'))),
          )
        ),
        # Column-right Seasonality and CPUE per length class
        bslib::layout_column_wrap(
          width = '450px',
          fixed_width=F,
          min_height = '700px',
          # CPUE per length class
          bslib::card(
            fill = T,
            full_screen = TRUE,
            # bslib::card_header(textOutput(ns('plot_cpue_pl_title'))),
            bslib::card_header(modal_UI(ns('plot_cpue_pl_title'),'Capturas por unidad de esfuerzo (por talla)')),
            bslib::card_body(plotOutput(ns('plot_cpue_pl')))
          ),
          # Catch seasonality
          bslib::card(
            fill = T,
            full_screen = TRUE,
            # bslib::card_header(textOutput(ns('plot_est_title'))),
            bslib::card_header(modal_UI(ns('plot_est_title'),'Estacionalidad de capturas')),
            bslib::card_body(plotOutput(ns('plot_est'))),
          )
        )
      )
    )
  )
}
    
#' 02_porEsp Server Functions
#'
#' @noRd 
mod_02_porEsp_server <- function(id, lang, region){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # Sidebar metadata
    output$region_title <- renderText(ifelse(lang() == "ES","Región","Region"))
    output$region_desc <- renderText(region)
    output$sampleyear_title <- renderText(ifelse(lang() == "ES","Año de Muestreo","Sampling Year(s)"))
    output$species_title <- renderText(ifelse(lang() == "ES","Especie","Species"))
    
    # Value boxes (getting relevant summary statistics)
    ss <- reactive(ss_summ_stats(
      fishdbase = fishdbase, 
      lfqdbase = lfqdbase,
      input$sampleyear[1], 
      input$sampleyear[2],
      currspec = input$species
    ))
    
    # Filling in value box titles
    # output$num_capturado_title <- renderText(ifelse(lang() == "ES","Número de pescas capturadas","Number of fish captured"))
    # output$peso_capturado_title <- renderText(ifelse(lang() == "ES","Peso capturado (kg)","Weight captured (kg)"))
    # output$prop_capturas_matr_title <- renderText(ifelse(lang() == "ES","Proporción de capturas cerca de la talla de madurez","Proportion of captures near the length at maturity"))
    # output$prop_capturas_mspawn_title <- renderText(ifelse(lang() == "ES","Proporción de capturas que fueron mega-reproductadores","Proportion of captures that were mega-spawners"))
    output$froese_indics_title <- renderText(ifelse(
      lang() == 'ES', 
      'Indicadores de la Poblacion de Peces',
      'Fishery Stock Indicators'
    ))
    modal_Server('num_capturado_title',lang,
                 plot_title = c("Número de pescas capturadas","Number of fish captured"),
                 plot_info = c("Información sobre el gráfico","Information about the plot"))
    modal_Server('peso_capturado_title',lang,
                 plot_title = c("Peso capturado (kg)","Weight captured (kg)"),
                 plot_info = c("Información sobre el gráfico","Information about the plot"))
    modal_Server('prop_capturas_matr_title',lang,
                 plot_title = c("Proporción de capturas cerca de la talla de madurez","Proportion of captures near the length at maturity"),
                 plot_info = c("Información sobre el gráfico","Information about the plot"))
    modal_Server('prop_capturas_immat_title',lang,
                 plot_title = c("Proporción de capturas inmaduras","Proportion of captures that were immature"),
                 plot_info = c("Información sobre el gráfico","Information about the plot"))
    modal_Server('prop_capturas_mspawn_title',lang,
                 plot_title = c("Proporción de capturas que fueron mega-reproductadores","Proportion of captures that were mega-spawners"),
                 plot_info = c("Información sobre el gráfico","Information about the plot"))
    # And text
    output$num_capturado <- renderText(ss()$num_capturado)
    output$peso_capturado <- renderText(ss()$peso_capturado)
    output$prop_capturas_matr <- renderText(ss()$prop_capturas_matr)
    output$prop_capturas_immat <- renderText(ss()$prop_capturas_immat)
    output$prop_capturas_mspawn <- renderText(ss()$prop_capturas_mspawn)

    # CPUE Trend card
    cpue <- reactive(ss_cpue_trend_total(
      lfqdbase=lfqdbase,
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2],
      currspec = input$species,
      lang = lang()
    ))
    # output$plot_cpue_total_title <- renderText(ifelse(lang() == "ES","Tendencia de capturas por unidad de esfuerzo", "Trend of catch per unit effort"))
    modal_Server('cpue_plot_title',lang,
                 plot_title = c("Tendencia de capturas por unidad de esfuerzo", "Trend of catch per unit effort"),
                 plot_info = c("Información sobre el gráfico","Information about the plot"))
    output$cpue_trend_desc <- renderUI(cpue()$cpue_trend_desc)
    output$cpue_plot <- renderPlot(cpue()$cpue_plot)
    output$cpue_mean_desc <- renderUI(cpue()$cpue_mean_desc)
    
    # Length-frequencies by gear plot
    output$plot_lfq_title <- renderText(ifelse(lang() == "ES","Frecuencias de tallas por arte","Frequencies by Size Class"))
    modal_Server(
      'plot_lfq_title',
      lang,
      plot_title = c("Frecuencias de tallas por arte","Frequencies by Size Class"),
      plot_info = c("Información sobre el gráfico","Information about the plot")
    )
    output$plot_lfq <- renderPlot(ss_lfq_plot(
      lfqdbase,
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2],
      currspec = input$species,
      froese = ss()$froese,
      lang = lang()
    ))
    
    # CPUE by gear plot
    # output$plot_cpue_arte_title <- renderText(
    #   ifelse(lang() == "ES",
    #          "Capturas por unidad de esfuerzo por arte",
    #          "Captures by unit of effort")
    # )
    # output$ss_cpue_por_arte <- renderPlot(ss_cpue_plot(
    #   lfqdbase,
    #   year_min = input$sampleyear[1],
    #   year_max = input$sampleyear[2],
    #   currspec = input$species,
    #   bygear = T,
    #   lang=lang()
    # ))
    
    # Catch-seasonality plot
    modal_Server('plot_est_title',lang,
                 plot_title = c("Estacionalidad de capturas","Catch seasonality"),
                 plot_info = c("Información sobre el gráfico","Information about the plot"))
    output$plot_est <- renderPlot(ss_plot_ssn(
      lfqdbase,
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2],
      currspec = input$species,
      lang = lang()
    ))
    
    # CPUE per length-class plot
    output$plot_cpue_pl_title <- renderText(ifelse(
      lang() == "ES",
      "Capturas por unidad de esfuerzo (por talla)",
      "Catch per unit effort (by size)")
    )
    modal_Server('plot_cpue_pl_title',lang,
                 plot_title = c("Capturas por unidad de esfuerzo (por talla)","Catch per unit effort (by size)"),
                 plot_info = c("Información sobre el gráfico","Information about the plot"))
    output$plot_cpue_pl <- renderPlot(ss_plot_cpue_pl(
      lfqdbase,
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2],
      currspec = input$species,
      lang = lang()
    ))
  })
}
    
## To be copied in the UI
# mod_02_porEsp_ui("02_porEsp_1")
    
## To be copied in the server
# mod_02_porEsp_server("02_porEsp_1")

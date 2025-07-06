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
  
  # Value boxes
  num_capt_v_box = bslib::value_box(
    min_width = "300px",
    theme='text-indigo',
    title = modal_UI(ns('num_capturado_title'),'Número de pescas capturadas',inline=FALSE),
    value = textOutput(ns('num_capturado')),
    showcase = fontawesome::fa_i("pen-to-square")
  )
  
  # Total weight captured
  peso_cap_v_box = bslib::value_box(
    min_width = "300px",
    theme = 'text-teal',
    title = modal_UI(ns('peso_capturado_title'),'Peso capturado (kg)',inline=FALSE),
    value = textOutput(ns('peso_capturado')),
    showcase = fontawesome::fa_i("scale-unbalanced")
  )
  
  # Proportion of captures that are close to the length at maturity
  prop_capt_matr_v_box = bslib::value_box(
    min_width = "300px",
    # min_height = '150px',
    theme = 'text-orange',
    # title = textOutput(ns('prop_capturas_matr_title')),
    title = modal_UI(ns('prop_capturas_matr_title'),'Proporción de capturas cerca de la talla de madurez',inline=FALSE),
    value = textOutput(ns('prop_capturas_matr')),
    showcase = fontawesome::fa_i("ruler")
  )
  
  # Proportion of captures that are mega-spawners
  prop_capt_mspawn_v_box = bslib::value_box(
    min_width = "300px",
    theme = 'text-blue',
    title = modal_UI(ns('prop_capturas_mspawn_title'),'Proporción de capturas que fueron mega-reproductadores',inline=FALSE),
    value = textOutput(ns('prop_capturas_mspawn')),
    showcase = fontawesome::fa_i("fish-fins")
  )
  
  # UI Layout ----------
  
  the_sidebar = sidebar(
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
  )
  
  bslib::layout_sidebar(
    fillable = T,
    # Inputs and descriptions in the sidebar
    sidebar = the_sidebar,
    # Main body
    bslib::layout_column_wrap(
      width = 1,
      fill=F,
      heights_equal = 'row',
      # Row 1 - 3-column box layout
      bslib::layout_column_wrap(
        # width = 1/3,
        width = '300px',
        fixed_width=F,
        # Column 1 - CPUE plot card
        bslib::layout_column_wrap(
          min_height='300px',
          max_height = '300px',
          fill=T,
          bslib::card(
            fill = T,
            full_screen = TRUE,
            bslib::card_header(modal_UI(ns('plot_cpue_total_title'),'Tendencia de capturas por unidad de esfuerzo')),
            bslib::card_body(plotOutput(ns('plot_cpue_total'))),
            bslib::card_footer(textOutput(ns('plot_cpue_total_desc')))
          )
        ),
        # Column 2 - Weight and number boxes
        bslib::card(
          bslib::card_header("Indicadores de monitoreo"),
          bslib::layout_column_wrap(
            min_height='300px',
            max_height = '300px',
            prop_capt_matr_v_box,
            prop_capt_mspawn_v_box
          )
        )
      ),
      # Row 2 - supplementary plots
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
            bslib::card_header(modal_UI(ns('plot_cpue_pl_title'),'Capturas por unidad de esfuerzo (por talla)')),
            bslib::card_body(plotOutput(ns('plot_cpue_pl')))
          ),
          # Catch seasonality
          bslib::card(
            fill = T,
            full_screen = TRUE,
            bslib::card_header(modal_UI(ns('plot_est_title'),'Estacionalidad de capturas')),
            bslib::card_body(plotOutput(ns('plot_est'))),
          )
        )
      ),
      bslib::layout_column_wrap(
        min_height='300px',
        max_height = '300px',
        num_capt_v_box,
        peso_cap_v_box
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
    modal_Server('num_capturado_title',lang,
                 plot_title = c("Número de pescas capturadas","Number of fish captured"),
                 plot_info = c("Información sobre el gráfico","Information about the plot"))
    modal_Server('peso_capturado_title',lang,
                 plot_title = c("Peso capturado (kg)","Weight captured (kg)"),
                 plot_info = c("Información sobre el gráfico","Information about the plot"))
    modal_Server('prop_capturas_matr_title',lang,
                 plot_title = c("Proporción de capturas cerca de la talla de madurez","Proportion of captures near the length at maturity"),
                 plot_info = c("Información sobre el gráfico","Information about the plot"))
    modal_Server('prop_capturas_mspawn_title',lang,
                 plot_title = c("Proporción de capturas que fueron mega-reproductadores","Proportion of captures that were mega-spawners"),
                 plot_info = c("Información sobre el gráfico","Information about the plot"))
    # And text
    output$num_capturado <- renderText(ss()$num_capturado)
    output$peso_capturado <- renderText(ss()$peso_capturado)
    output$prop_capturas_matr <- renderText(ss()$prop_capturas_matr)
    output$prop_capturas_mspawn <- renderText(ss()$prop_capturas_mspawn)

    # CPUE Trend card
    cpue_total <- reactive(ss_cpue_trend_total(
      lfqdbase=lfqdbase,
      year_min = input$sampleyear[1],
      year_max = input$sampleyear[2],
      currspec = input$species,
      lang = lang()
    ))
    # output$plot_cpue_total_title <- renderText(ifelse(lang() == "ES","Tendencia de capturas por unidad de esfuerzo", "Trend of catch per unit effort"))
    modal_Server('plot_cpue_total_title',lang,
                 plot_title = c("Tendencia de capturas por unidad de esfuerzo", "Trend of catch per unit effort"),
                 plot_info = c("Información sobre el gráfico","Information about the plot"))
    output$plot_cpue_total_desc <- renderText(cpue_total()$cpue_desc)
    output$plot_cpue_total <- renderPlot(cpue_total()$plot)
    
    # Length-frequencies by gear plot
    output$plot_lfq_title <- renderText(ifelse(lang() == "ES","Frecuencias de tallas por arte","Frequencies by Size Class"))
    modal_Server('plot_lfq_title',lang,
                 plot_title = c("Frecuencias de tallas por arte","Frequencies by Size Class"),
                 plot_info = c("Información sobre el gráfico","Information about the plot"))
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

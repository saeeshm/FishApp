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
      # Instantiating share sidebar UI, specifying filters for this page
      mod_06_sidebar_filters_ui(ns('shared_sidebar'), show=c('database', 'years', 'species'))
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
            card_title = modal_UI(ns('cpue_plot_title')),
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
            bslib::card_header(modal_UI(ns('plot_lfq_title'))),
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
            bslib::card_header(modal_UI(ns('plot_cpue_pl_title'))),
            bslib::card_body(plotOutput(ns('plot_cpue_pl')))
          ),
          # Catch seasonality
          bslib::card(
            fill = T,
            full_screen = TRUE,
            bslib::card_header(modal_UI(ns('plot_est_title'))),
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
mod_02_porEsp_server <- function(id, lang, filters){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Sidebar/Input elements ----------
    
    # Filter server for this page, using the filter_state reactive object as the
    # shared source of truth
    mod_06_sidebar_filters_server(
      'shared_sidebar',
      filter_state = filters,
      lang = lang
    )
    
    # Picking the appropriate databases based on the provided region
    curr_lfqdb <- reactive({pick_lfq_database(dbname = filters$database)}) |> 
      bindCache(filters$database)
    curr_fdb <- reactive({pick_fish_database(dbname = filters$database)}) |> 
      bindCache(filters$database)
    
    # Modals and descriptions ----------
    
    # List of title/description keywords - to use for generating modals and
    # descriptions
    idx_list <- c(
      # Plots
      'cpue_plot',
      'plot_lfq',
      'plot_est',
      'plot_cpue_pl',
      # 'plot_cpue_arte'
      # Value boxes
      'num_capturado',
      'peso_capturado',
      'prop_capturas_matr',
      'prop_capturas_immat',
      'prop_capturas_mspawn'
    )
    
    # TEST - Checking that all present
    # idx_list %in% descriptions$idx
    
    # Creating modals
    purrr::walk(idx_list, \(curr_idx){
      desc <- reactive(get_desc(curr_idx, 'Single Species Indicators', lang))
      modal_Server(
        # Assigning an object name to this modal which is the idx + '_title"
        paste0(curr_idx,'_title'),
        # Pass lang as a reactive
        lang = lang,
        plot_title = reactive(desc()$title),
        plot_info = reactive(desc()$desc)
      )
    })
    
    # Sectional title for the fishery indicators (no modal associated with this
    # ATM)
    output$froese_indics_title <- renderText(ifelse(
      lang() == 'ES', 
      'Indicadores de la Poblacion de Peces',
      'Fishery Stock Indicators'
    ))
    
    # Main body elements ----------
    
    # Summary statistics object (used to populate value boxes)
    ss <- reactive(ss_summ_stats(
      fishdbase = curr_fdb(), 
      lfqdbase = curr_lfqdb(),
      year_min = filters$years[1], 
      year_max = filters$years[2],
      currspec = filters$species
    ))
    
    # Populating value boxes
    output$num_capturado <- renderText(ss()$num_capturado)
    output$peso_capturado <- renderText(ss()$peso_capturado)
    output$prop_capturas_matr <- renderText(ss()$prop_capturas_matr)
    output$prop_capturas_immat <- renderText(ss()$prop_capturas_immat)
    output$prop_capturas_mspawn <- renderText(ss()$prop_capturas_mspawn)

    # CPUE Trend card
    cpue <- reactive(ss_cpue_trend_total(
      lfqdbase = curr_lfqdb(),
      year_min = filters$years[1], 
      year_max = filters$years[2],
      currspec = filters$species,
      lang = lang()
    ))
    # Outputs for the labelled plot
    output$cpue_trend_desc <- renderUI(cpue()$cpue_trend_desc)
    output$cpue_plot <- renderPlot(cpue()$cpue_plot)
    output$cpue_mean_desc <- renderUI(cpue()$cpue_mean_desc)
    
    # Length-frequencies by gear plot
    output$plot_lfq <- renderPlot(ss_lfq_plot(
      lfqdbase = curr_lfqdb(),
      year_min = filters$years[1], 
      year_max = filters$years[2],
      currspec = filters$species,
      froese = ss()$froese,
      lang = lang()
    ))
    
    # CPUE by gear plot
    # output$ss_cpue_por_arte <- renderPlot(ss_cpue_plot(
    #   lfqdbase,
    #   year_min = input$years[1],
    #   year_max = input$years[2],
    #   currspec = input$species,
    #   bygear = T,
    #   lang=lang()
    # ))
    
    # Catch-seasonality plot
    output$plot_est <- renderPlot(ss_plot_ssn(
      lfqdbase = curr_lfqdb(),
      year_min = filters$years[1], 
      year_max = filters$years[2],
      currspec = filters$species,
      lang = lang()
    ))
    
    # CPUE per length-class plot
    output$plot_cpue_pl <- renderPlot(ss_plot_cpue_pl(
      lfqdbase = curr_lfqdb(),
      year_min = filters$years[1], 
      year_max = filters$years[2],
      currspec = filters$species,
      lang = lang()
    ))
  })
}
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
      # title = textOutput(ns('num_muestreos_title')),
      title = modal_UI(ns('num_muestreos_title'),'Número de muestreos'),
      value = textOutput(ns('num_muestreos')),
      showcase = fontawesome::fa_i("pen-to-square")
    ),
    bslib::value_box(
      # fill=F,
      min_width = "300px",
      # min_height = '150px',
      theme = 'text-orange',
      title = modal_UI(ns('num_dias_muestreo_title'),'Dias muestreado'),
      value = textOutput(ns('num_dias_muestreo')),
      showcase = fontawesome::fa_i("calendar-days")
    ),
    bslib::value_box(
      # fill=F,
      min_width = "300px",
      # min_height = '150px',
      theme = 'text-blue',
      title = modal_UI(ns('num_comunidades_title')),
      value = textOutput(ns('num_comunidades')),
      showcase = fontawesome::fa_i("map-location-dot")
      # p(textOutput(ns('nombres_comunidades')))
    ),
    bslib::value_box(
      # fill=F,
      min_width = "300px",
      theme = 'text-teal',
      # min_height = '150px',
      title = modal_UI(ns('peso_muestreado_title')),
      value = textOutput(ns('peso_muestreado')),
      showcase = fontawesome::fa_i("scale-unbalanced")
    ),
    bslib::value_box(
      # fill=F,
      min_width = "300px",
      # min_height = '150px',
      theme = 'text-pink',
      title = modal_UI(ns('num_especies_title')),
      value = textOutput(ns('num_especies')),
      showcase = fontawesome::fa_i("fish-fins")
    ),
    bslib::value_box(
      # fill=F,
      min_width = "300px",
      theme = 'text-purple',
      # min_height = '150px',
      title = modal_UI(ns('num_individuos_title')),
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
      # Instantiating share sidebar UI, specifying filters for this page
      mod_06_sidebar_filters_ui(ns('shared_sidebar'), show=c('database', 'years'))
    ),
    # Main body
    bslib::layout_column_wrap(
      width = 1,
      fill = F,
      height = NULL,
      heights_equal = 'row',
      # Row 0: Management milestones timeline (full width, short)
      bslib::card(
        fill = TRUE,
        # bslib::card_header(textOutput(ns('timeline_title'))),
        bslib::card_body(
          padding = 0,
          shiny::uiOutput(ns('timeline'))
        )
      ),
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
        min_height = '600px',
        fill = F,
        # Plot 1 - Top 10-species
        plot_card_labelled(
          plot = plotOutput(ns('plot_esp_comunes')),
          # top_label = shiny::uiOutput(ns('prop_capt_desc')),
          card_title = modal_UI(ns('plot_esp_comunes_title'))
        ),
        # Plot 2 - Gears
        bslib::card(
          fill = T,
          full_screen = TRUE,
          min_height = '600px',
          bslib::card_header(modal_UI(ns('plot_artes_title'))),
          bslib::card_body(plotOutput(ns('plot_artes')))
        )
      ),
     # Row 4: Long plots
      layout_column_wrap(
        width = '400px',
        # Plot 1 - Sampled weight by community and gear
        bslib::card(
          fill = T, 
          min_height = '700px',
          full_screen = TRUE,
          bslib::card_header(modal_UI(ns('plot_muest_title'))),
          bslib::card_body(plotOutput(ns('plot_muest')))
        ),
        # Plot 2 - Catch seasonality by community
        bslib::card(
          fill = T,
          min_height = '700px',
          full_screen = TRUE,
          bslib::card_header(modal_UI(ns('plot_ssn_title'))),
          bslib::card_body(plotOutput(ns('plot_ssn')))
        )
      )
    )
  )
}

#' 03_datMuestro Server Functions
#'
#' @noRd 
mod_03_datGen_server <- function(id, lang, filters){
  moduleServer( id, function(input, output, session){
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
      'plot_esp_comunes',
      'plot_artes',
      'plot_muest',
      'plot_ssn',
      # Value boxes
      'num_muestreos',
      'num_dias_muestreo',
      'num_comunidades',
      'peso_muestreado',
      'num_especies',
      'num_individuos'
    )
    
    # TEST - Checking that all present
    # idx_list %in% descriptions$idx
    
    # Creating modals
    purrr::walk(idx_list, \(curr_idx){
      desc <- reactive(get_desc(curr_idx, 'Datos Generales', lang))
      modal_Server(
        # Assigning an object name to this modal which is the idx + '_title"
        paste0(curr_idx,'_title'),
        # We pass lang as a reactive object
        lang = lang,
        plot_title = reactive(desc()$title),
        plot_info = reactive(desc()$desc)
      )
    })
    
    # Main body elements ----------

    # Management milestones timeline (varies by database and language)
    output$timeline_title <- renderText(
      if (lang() == 'ES') 'Cronología' else 'Timeline'
    )
    output$timeline <- renderUI(dg_timeline(filters$database, lang())) |>
      bindCache(filters$database, lang())

    # Summary statistics object (used to populate value boxes)
    ss <- reactive(dg_summ_stats(
      dbase = curr_fdb(), 
      year_min = filters$years[1],
      year_max = filters$years[2]
    )) |> bindCache(curr_fdb(), filters$years)
    
    # Text outputs for value box UIs
    output$num_muestreos <- renderText(ss()$num_muestreos)
    output$num_dias_muestreo <- renderText(ss()$num_dias_muestreo)
    output$num_comunidades <- renderText(ss()$num_comunidades)
    output$nombres_comunidades <- renderText(ss()$nombres_comunidades)
    output$peso_muestreado <- renderText(ss()$peso_muestreado)
    output$num_especies <- renderText(ss()$num_especies)
    output$num_individuos <- renderText(ss()$num_individuos)
    
    # Top 10 captured species data
    esp_comunes <- reactive(dg_plot_esp_comunes(
      dbase = curr_fdb(), 
      year_min = filters$years[1],
      year_max = filters$years[2],
      lang = lang
    )) |> bindCache(curr_fdb(), filters$years, lang())
    
    output$plot_esp_comunes <- renderPlot(esp_comunes()$top10_plot)
    output$prop_capt_desc <- renderUI(esp_comunes()$prop_capt)
    
    # Gear usage plot
    output$plot_artes <- renderPlot(dg_plot_artes(
      dbase = curr_fdb(), 
      year_min = filters$years[1],
      year_max = filters$years[2],
      lang = lang
    )) |> bindCache(curr_fdb(), filters$years, lang())
    
    # LFQ by trophic level plot
    output$plot_muest <- renderPlot(dg_plot_peso(
      dbase = curr_fdb(), 
      year_min = filters$years[1],
      year_max = filters$years[2],
      lang = lang
    )) |> bindCache(curr_fdb(), filters$years, lang())
    
    # Top-captured species plot
    output$plot_ssn <- renderPlot(dg_plot_ssn(
      dbase = curr_fdb(), 
      year_min = filters$years[1],
      year_max = filters$years[2],
      lang = lang
    )) |> bindCache(curr_fdb(), filters$years, lang())
  })
}

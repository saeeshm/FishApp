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
      # Instantiating share sidebar UI, specifying filters for this page
      mod_06_sidebar_filters_ui(ns('shared_sidebar'), show=c('database', 'years'))
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
            card_title = modal_UI(ns('cpue_plot_title')),
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
              title = modal_UI(ns('div_rich_title')),
              plot_card_labelled(
                plot = plotOutput(ns('div_rich_plot')),
                bottom_label = shiny::uiOutput(ns('div_rich_ov_desc')),
                card_title = NULL,
                top_label = shiny::uiOutput(ns('div_rich_trend_desc'))
              )
            ),
            # Species diversity
            bslib::nav_panel(
              title = modal_UI(ns('div_shannon_title')),
              plot_card_labelled(
                plot = plotOutput(ns('div_shannon_plot')),
                bottom_label = shiny::uiOutput(ns('div_shannon_ov_desc')),
                card_title = NULL,
                top_label = shiny::uiOutput(ns('div_shannon_trend_desc'))
              )
            ),
            # Species equitability
            bslib::nav_panel(
              title = modal_UI(ns('div_eq_title')),
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
            bslib::card_header(modal_UI(ns('plot_lfq_title'))),
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
            plot_card_labelled(
              plot = plotOutput(ns('plot_bdens')),
              card_title = modal_UI(ns('plot_bdens_title')),
              top_label = shiny::uiOutput(ns('plot_bdens_desc'))
            )
          ),
          # Top-10 species captured as deviances plot
          bslib::card(
            fill = T,
            full_screen = TRUE,
            bslib::card_header(modal_UI(ns('plot_top10_dev_title'))),
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
mod_01_multiEsp_server <- function(id, lang, filters){
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
      'cpue_plot',
      'div_rich',
      'div_shannon',
      'div_eq',
      'plot_bdens',
      'plot_lfq',
      'plot_top10_dev'
    )
    
    # TEST - Checking that all present
    # idx_list %in% card_descriptions$idx
    
    # Creating modals
    purrr::walk(idx_list, \(curr_idx){
      desc <- reactive(get_desc(curr_idx, 'Multispecies Indicators', lang))
      modal_Server(
        # Assigning an object name to this modal which is the idx + '_title"
        paste0(curr_idx,'_title'),
        # Pass lang as a reactive
        lang = lang,
        plot_title = reactive(desc()$title),
        plot_info = reactive(desc()$desc)
      )
    })
    
    # Output objects ----------
    
    # Multispecies CPUE trend
    cpue <- reactive(ms_cpue_plot(
      lfqdb = curr_lfqdb(),
      year_min = filters$years[1],
      year_max = filters$years[2],
      lang = lang()
    )) |> bindCache(curr_lfqdb(), filters$years)
    output$cpue_trend_desc <- renderUI(cpue()$cpue_trend_desc)
    output$cpue_plot <- renderPlot(cpue()$cpue_plot)
    output$cpue_mean_desc <- renderUI(cpue()$cpue_mean_desc)
    
    # Diversity plots and UI object list
    div <- reactive(ms_div_ts_plots(
      lfqdb = curr_lfqdb(),
      year_min = filters$years[1],
      year_max = filters$years[2],
      lang = lang()
    )) |> bindCache(curr_lfqdb(), filters$years)
    # Species richness
    output$div_rich_trend_desc <- renderUI(div()$rich$trend_desc)
    output$div_rich_plot <- renderPlot(div()$rich$plot)
    output$div_rich_ov_desc <- renderUI(div()$rich$ov_desc)
    # Species diversity
    output$div_shannon_trend_desc <- renderUI(div()$shannon$trend_desc)
    output$div_shannon_plot <- renderPlot(div()$shannon$plot)
    output$div_shannon_ov_desc <- renderUI(div()$shannon$ov_desc)
    # Species equitability
    output$div_eq_trend_desc <- renderUI(div()$eq$trend_desc)
    output$div_eq_plot <- renderPlot(div()$eq$plot)
    output$div_eq_ov_desc <- renderUI(div()$eq$ov_desc)
    
    # Biomass density spectrum plot
    bd <- reactive(ms_bdens_plot(
      fdb = curr_fdb(),
      year_min = filters$years[1],
      year_max = filters$years[2],
      lang = lang()
    )) |> bindCache(curr_fdb(), filters$years)
    output$plot_bdens_desc <- renderUI(bd()$bd_desc)
    output$plot_bdens <- renderPlot(bd()$bd_plot)
    
    # Length-frequency per gear and trophic level plot
    output$plot_lfq <- renderPlot(ms_lfq_plot(
      lfqdb = curr_lfqdb(),
      year_min = filters$years[1],
      year_max = filters$years[2],
      lang = lang()
    )) |> bindCache(curr_lfqdb(), filters$years, lang())
    
    # Top-10 species captured as deviances plot 
    output$plot_top10_dev <- renderPlot(ms_top10_plot(
      lfqdb = curr_lfqdb(),
      year_min = filters$years[1],
      year_max = filters$years[2],
      lang()
    )) |> bindCache(curr_lfqdb(), filters$years)
  })
}

## To be copied in the UI
# mod_01_multiEsp_ui("01_multiEsp_1")

## To be copied in the server
# mod_01_multiEsp_server("01_multiEsp_1")

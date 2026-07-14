# ==== Helper objects used for indexing ====

#' 06_sidebar_filters.R UI Function
#'
#' @description A shiny Module which implements a set of sidebar filters for
#'   analytics pages. A singular filtering module allows the preservation of
#'   filter options between pages - most importantly the choice of regional
#'   database.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib card
mod_06_sidebar_filters_ui <- function(id, show=c('database','years','species')){
  ns <- NS(id)
  tagList(
    # Database input
    if('database' %in% show){
      tagList(
        tags$h4(textOutput(ns('database_title'))),
        shinyWidgets::pickerInput(
          ns('database'), 
          label = NULL, 
          choices = c('Tela', 'Trujillo'),
          selected = 'Tela',
          options = list(`live-search` = TRUE)
        )
      )
    },
    # Date-range input
    if('years' %in% show){
      tagList(
        tags$h4(textOutput(ns('years_title'))),
        shinyWidgets::sliderTextInput(
          ns('years'), 
          label=NULL,
          choices=get_year_periods('Tela'),
          selected=c(get_year_periods('Tela')[1], tail(get_year_periods('Tela'), 1))
        )
      )
    },
    # Fish species input
    if('species' %in% show){
      tagList(
        tags$h4(textOutput(ns('species_title'))),
        shinyWidgets::pickerInput(
          ns('species'), 
          label = NULL, 
          choices = get_spec_cmn_names('Tela'),
          selected = 'Calale',
          choicesOpt = list(
            subtext = get_spec_sci_names('Tela'),
            sep = ' '
          ),
          options = list(`live-search` = TRUE)
        )
      )
    }
  )
}

#' 06_sidebar_filters Server Functions
#'
#' @noRd 
mod_06_sidebar_filters_server <- function(id, filter_state, lang){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # ==== Inbound - Updating UIs based on changes to database object ====
    
    # Adjusting inputs choices for ALL inputs if the database was updated
    observeEvent(filter_state$database, {
      # Update all instances of the database selector
      shinyWidgets::updatePickerInput(
        session,
        inputId  = "database",
        selected = filter_state$database
      )
      
      # Updating all instances of the year-period selector with new year-period
      # range appropriate to this database, when database is updated
      req(filter_state$database)
      db_year_period <- get_year_periods(filter_state$database)
      shinyWidgets::updateSliderTextInput(
        session,
        inputId  = 'years',
        choices = db_year_period,
        selected = c(db_year_period[1], tail(db_year_period, 1))
      )
      
      # Updating all instances of the species selector with new species list
      # appropriate to this database, when database is updated
      spec_cmn_names <- get_spec_cmn_names(filter_state$database)
      shinyWidgets::updatePickerInput(
        session,
        inputId  = "species",
        choices = spec_cmn_names,
        # Defaulting to Calale, which for now we now is captured in all
        # fisheries
        selected = 'Calale',
        choicesOpt = list(
          subtext = get_spec_sci_names(filter_state$database),
          sep = ' '
        )
      )
    }, ignoreInit = FALSE)
    
    # ==== Outbound - Updating filter_state based on selections made via UI ====
    
    # Updating database selection
    observeEvent(input$database, {
      # Guard prevents circular update loop
      if (identical(input$database, filter_state$database)) return()
      # Updating values of filter_state object
      filter_state$database <- input$database
    }, ignoreInit = TRUE)
    
    # Updating years selection
    observeEvent(input$years, {
      # Next, checking for identical and updating if required
      req(input$years)
      if (identical(input$years, filter_state$years)) return()
      filter_state$years <- input$years
    }, ignoreInit = TRUE)
    
    # Updating species selection
    observeEvent(input$species, {
      # Checking for identical and updating if required
      req(input$species)
      if (identical(input$species, filter_state$species)) return()
      filter_state$species <- input$species
    }, ignoreInit = TRUE)
    
    # Adding title/headers for each input type
    output$database_title <- renderText(ifelse(lang() == 'ES','Región Seleccionada','Selected Region'))
    output$years_title <- renderText(ifelse(lang() == 'ES','Año de Muestreo','Sampling Year(s)'))
    output$species_title <- renderText(ifelse(lang() == 'ES','Especies','Species'))
    
    # Returning list of filtered options (these can then be used by pages that
    # call this module)
    list(
      database = reactive(req(input$database)),
      years = reactive(req(input$years)),
      species = reactive(req(input$species))
    )
  })
}

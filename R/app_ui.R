#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Golem external resources
    golem_add_external_resources(),
    page_navbar(
      # THEME
      theme = bslib::bs_theme(bootswatch = "sandstone"),
      title='PescApp',
      id='main-page',
      # Per-species indicators
      nav_panel(
        title="Descripcion", 
        h1("Page still under development!")
      ),
      # Zone map
      nav_panel(
        title="Mapa", 
        # h1("Page still under development!")
        mod_04_mapaReg_ui(id='mapaReg')
      ),
      # Multispecies indicators
      nav_panel(
        title="Datos Multiespecificos", 
        # General characteristics module UI
        # h1("Page still under development!")
        mod_01_multiEsp_ui(id='multiEsp')
      ),
      # Per-species indicators
      nav_panel(
        title="Datos Por Especies", 
        # h1("Page still under development!")
        mod_01_generalChars_ui(id='genPage')
      ),
      # Sampling/Fishing effort indicators
      nav_panel(
        title="Datos de Muestro", 
        h1("Page still under development!")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "PescApp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

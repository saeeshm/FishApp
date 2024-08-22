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
      theme = bslib::bs_theme(bootswatch = "cerulean"),
      title='PescApp',
      id='main-page',
      # Per-species indicators
      nav_panel(
        title="Descripción", 
        build_desc()
      ),
      # Zone map
      nav_panel(
        title="Mapa", 
        mod_04_mapaReg_ui(id='mapaReg')
      ),
      # General sampling effort information
      nav_panel(
        title="Datos Generales", 
        mod_03_datGen_ui("datGen")
      ),
      # Multispecies indicators
      nav_panel(
        title="Indicadores Multiespecíficos", 
        mod_01_multiEsp_ui(id='multiEsp')
      ),
      # Per-species indicators
      nav_panel(
        title="Indicadores Por Especie", 
        mod_02_porEsp_ui(id='porEsp')
      ),
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

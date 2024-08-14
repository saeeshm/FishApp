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
        title="Descripcion", 
        h1("The Coral Reef Alliance"),
        p("CORAL is an NGO committed to saving the world’s coral reefs through \
          cutting-edge science and community engagement, through collaboration \
          with local partners, to reduce direct threats to reefs and to promote \
          scalable and effective solutions for their protection. CORAL’s goal is\
          to create a network large, diverse connected, and well-managed reefs. \
          To achieve this goal, CORAL has Programs in two critically important \
          reef regions of the world: Hawaii and the Western Caribbean.")
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
        title="Datos Multiespecificos", 
        mod_01_multiEsp_ui(id='multiEsp')
      ),
      # Per-species indicators
      nav_panel(
        title="Datos Por Especies", 
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

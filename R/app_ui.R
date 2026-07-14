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
      shiny::includeScript("inst/app/www/code.js"),
      # Theming
      theme = bslib::bs_theme(
        brand = 'inst/app/www/brand.yml',
        bootswatch = "cerulean",
        bg = "#fff",
        fg = "#000000",
        primary = "#3ebbed",
        secondary = "#eddfca",
        info = '#90d6e5',
        success = "#00afa4",
        warning = '#f69a94',
        danger = '#a8426e',
        base_font = font_google("Inter"),
        code_font = font_google("JetBrains Mono"),
        heading_font = font_google("Poppins"),
        # Further customizations
        # 'body-font-family' = font_google('Inter'),
        # Navbar background hill
        navbar_bg = "#3ebbed",
        # NAvbar text colour
        navbar_light_contrast="#fff",
        "bslib_spacer" = "0.75rem" # Squeeze out a bit of space between cards
      ),
      
      # ==== App header ====
      title=tags$div(
        style='display:inline-flex;',
        tags$div(
          tags$img(
            class='app-logo app-logo-fish',
            src='www/CORAL_Logo_WhiteSquare_notext.png'
          ),
          tags$img(
            class='app-logo app-logo-text',
            src='www/CORAL_Logo_WhiteSquare_justtext.png'
          ),
          class = 'app-logo-container'
        ),
        tags$div(class='app-title','PescApp'),
        tags$div(class = 'waves-container')
      ),
      id='main-page',
      
      # ==== App pages ====
      
      # About page ----------
      nav_panel(
        title = shiny::uiOutput('desc_title'),
        shiny::uiOutput('desc_ui')
      ),
      
      # Map page ----------
      nav_panel(
        title = shiny::uiOutput('map_title'),
        mod_04_mapaReg_ui(id='mapaReg')
      ),
      
      # General sampling summary (stakeholder report) page ----------\
      nav_panel(
        title = shiny::uiOutput('datGen_title'),
        mod_03_datGen_ui("datGen")
      ),
      
      # Per-species fishery indicators ----------
      nav_panel(
        title = shiny::uiOutput('porEsp_title'),
        mod_02_porEsp_ui(id='porEsp')
      ),
      
      # Multi-species fishery indicators ----------
      nav_panel(
        title = shiny::uiOutput('multiEsp_title'),
        mod_01_multiEsp_ui(id='multiEsp')
      ),
      
      # A generalized language picker (this generates a warning, because bslib
      # doesn't like this floating loosely inside nav_panel!)
      tags$div(
        class='lang-picker',
        # shiny::icon('language'),
        shinyWidgets::pickerInput(
          inputId = "lang",
          # icon = fontawesome::fa('language'),
          choices=c('ES','EN'),
          selected='ES',
          width='10%',
          options=list(
            container='body'
          )
        )
      ) |> 
        bslib::tooltip("Cambiar idioma / Change language")
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
    favicon(ico='CORAL_Logo_ColorSquare_notext', ext='png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "PescApp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

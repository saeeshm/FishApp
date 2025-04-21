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
      theme = bslib::bs_theme(
        brand = 'inst/app/www/brand.yml',
        bootswatch = "cerulean",
        bg = "#fff",
        fg = "#000000",
        primary = "#0093d7",
        secondary = "#f0f0f0",
        info = '#0b639a',
        success = "#bfdc5d",
        warning = '#ffb43c',
        danger = '#fc7a64',
        base_font = font_google("Inter"),
        code_font = font_google("JetBrains Mono"),
        heading_font = font_google("Poppins"),
        # Further customizations
        # 'body-font-family' = font_google('Inter'),
        # Navbar background hill
        navbar_bg = "#0093d7",
        # NAvbar text colour
        navbar_light_contrast="#fff",
        "bslib_spacer" = "0.75rem" # Squeeze out a bit of space between cards
      ),
      title=tags$div(
        # style='{display:inline-block}',
        tags$img(
          class='app-logo',
          src='www/CORAL_Logo_WhiteSquare.png'
        ),
        tags$div(
          class='app-title',
          'PescApp'
        )
        # tags$div(
        #   class='lang-picker',
        #   shinyWidgets::pickerInput(
        #     inputId = "Id018",
        #     # icon = fontawesome::fa('language'),
        #     choices=c('ES', 'EN'),
        #     selected='ES',
        #     width='5%',
        #     options=list(
        #       container='body'
        #     )
        #   )
        # )
      ),
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
      tags$div(
        class='lang-picker',
        shinyWidgets::pickerInput(
          inputId = "Id018",
          # icon = fontawesome::fa('language'),
          choices=c('ES', 'EN'),
          selected='ES',
          width='10%',
          options=list(
            container='body'
          )
        )
      )
      # nav_spacer(),
      # nav_item(
      #   id='lang-picker',
      #   shinyWidgets::pickerInput(
      #     inputId = "Id018",
      #     # icon = fontawesome::fa('language'),
      #     choices=c('ES', 'EN'),
      #     selected='ES',
      #     # inline=T,
      #     options=list(
      #       container='body'
      #     )
      #   )
      # )
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

#' 05_modal UI Function
#'
#' @description A shiny Module to create modal windows.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
modal_UI <- function(id, default_title='Sin Título') {
  ns <- NS(id)
  tagList(
    div(
      div(shiny::textOutput(ns('plot_label')), class = "shiny-text-output shiny-bound-output"),
      actionButton(ns('modal_hook'), label = shiny::icon("info"), class = 'modal-hook'),
      style = 'display:ruby;'
    )
  )
}

#' 05_modal Server Functions
#'
#' @param id 
#' @param lang language toggle for app (must be a reactive object, character string)
#' @param plot_title A title for modal window (must be a reactive object, character string)
#' @param plot_info additional info for modal window (must be a reactive object, character string)
#'
#' @noRd
modal_Server <- function(id, lang, plot_title, plot_info) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      # Setting title and description reactively, so that they update with each
      # new change
      p_title = reactive(plot_title()[1])
      p_info = reactive(plot_info()[1])
      
      # Title for card header.
      output$plot_label = shiny::renderText(p_title())
      
      # Create and show modal dialogue window 
      observeEvent(input$modal_hook, {
        # print("Triggered new modal")
        # If there's a modal already visible on the page, remove it.
        shiny::removeModal()
        # Create and render shiny Modal window.
        showModal(
          modalDialog(
            title = p_title(),
            easyClose = TRUE,
            p_info(),
            footer = tagList(
              actionButton("close_modal_btn", ifelse(lang() == "ES", "Cerrar","Close"))
            )
          )
        )
      })
    }
  )
}
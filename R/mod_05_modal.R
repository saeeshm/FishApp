#' 05_modal UI Function
#'
#' @description A shiny Module to create modal windows.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
modal_UI <- function(id,default_title) {
  ns <- NS(id)
  tagList(
  div(
    div(shiny::textOutput(ns('plot_label')), class = "shiny-text-output shiny-bound-output"),
    actionButton(ns('modal_hook'),label = '?', class = 'modal-hook'),
    style = 'display:ruby;'
  ),
  tags$script(
    shiny::HTML(paste0("
          /// Find the element in the DOM
          the_el = document.querySelector(`[id*=\"",id,"\"]`);
          /// Hard-code the initial value of the element with the Spanish option.
          the_el.innerText = '",default_title,"';"
    ))
  )
  )
}

#' 05_modal Server Functions
#'
#' @param id 
#' @param lang language toggle for app
#' @param plot_title title for modal window
#' @param plot_info additional info for modal window
#'
#' @noRd
modal_Server <- function(id, lang, plot_title, plot_info) {
  moduleServer(
    id,
    function(input, output, session) {
      
      p_title = reactive({
        ifelse(
          lang() == 'ES',plot_title[1],plot_title[2]
        )
      })
      
      p_info = reactive({
        ifelse(
          lang() == 'ES',plot_info[1],plot_info[2]
        )
      })
      
      # Title for card header.
      output$plot_label = shiny::renderText(p_title())
      
      # Create and show modal dialogue window 
      observeEvent(input$modal_hook, {
        showModal(
          modalDialog(
            title = p_title(), 
            easyClose = TRUE,
            p_info()
          )
        )
      })
    }
  )
}
#' Create a standard bslic card with a plot output
#'
#' @param list An R list
#' @param class a class for the list
#'
#' @return an HTML list
#' @noRd
#'
#' @importFrom bslib card
plot_card <- function(header, ...) {
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(header),
    bslib::card_body(...)
  )
}

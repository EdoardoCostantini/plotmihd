#' Start Shiny app
#'
#' This function starts a Shiny app used to display the results from Anonymized et. al. (2022).
#'
#' @return Starts a shiny app to explore the results.
#' @author Anonymized, 2023
#' @references
#'
#' Anonymized
#'
#' @export
start_app <- function() {
    shiny::shinyApp(
        ui = ui_call(),
        server = server
    )
}

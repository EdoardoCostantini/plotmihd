#' Start Shiny app
#'
#' This function starts a Shiny app used to display the results from Costantini et. al. (2022).
#'
#' @return Starts a shiny app to explore the results.
#' @author Edoardo Costantini, 2023
#' @references
#'
#' Costantini, E., Lang, K. M., Reeskens, T., & Sijtsma, K. (2022). High-dimensional imputation for the social sciences: a comparison of state-of-the-art methods. arXiv preprint arXiv:2208.13656.
#'
#' @export
start_app <- function() {
    shiny::shinyApp(
        ui = ui_call(),
        server = server
    )
}

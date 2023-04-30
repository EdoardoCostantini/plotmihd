#' User interface call
#'
#' Calls the definition of the user interface and returns it as an output
#'
#' @return UI object that can be passed directly to shiny::shinyApp()
#' @author Edoardo Costantini, 2023
#' @export
ui_call <- function() {
    # Define UI
    ui <- shiny::fluidPage(

        # App title
        shiny::titlePanel(
            shiny::h1("High-dimensional imputation for the social sciences", align = "center")
        ),
        shiny::column(
            width = 10,
            offset = 1,
            # Create tabs for different plotting aspects
            shiny::tabsetPanel(
                type = "tabs",
                selected = "About this Shiny app",
                shiny::tabPanel(
                    title = "About this Shiny app",
                    shiny::column(
                        width = 8,
                        offset = 2,
                        shiny::HTML(
                            "<br>
                            This Shiny app accompanies the article:
                            <br>
                            <br>
                            <a href='https://doi.org/10.48550/arXiv.2208.13656'>Costantini, E., Lang, K. M., Reeskens, T., & Sijtsma, K. (2022). High-dimensional imputation for the social sciences: a comparison of state-of-the-art methods. <i>arXiv preprint arXiv:2208.13656.</i></a>
                            <br>
                            <br>
                            The app contains five modules that allow the user to interact with different results related to this article:
                            <ul>
                                <li><b>Module 1</b>: Interact with the main simulation study results <i>coming soon</i>.</li>
                                <li><b>Module 2</b>: Interact with the collinearity simulation study results <i>coming soon</i>.</li>
                                <li><b>Module 3</b>: Interact with a tool exploring the performance of MI-PCA for varying levels of collinearity.</li>
                                <li><b>Module 4</b>: Interact with the resampling study results <i>coming soon</i>.</li>
                                <li><b>Module 5</b>: Interact with the convergence plots for the studies <i>coming soon</i>.</li>
                            </ul>
                            For questions and feedback, please <a href = 'mailto:e.costantini@tilburguniversity.edu'>send me an email</a>.
                            "
                        )
                    )
                ),
                shiny::tabPanel(
                    title = "Module 1: Simulation study",
                    shiny::column(
                        width = 2,
                        offset = 5,
                        shiny::HTML(
                            "<br>
                            <p style='text-align:center'>Coming soon</p>
                            <br>
                            <br>"
                        )
                    )
                ),
                shiny::tabPanel(
                    title = "Module 2: Collinearity study",
                    shiny::column(
                        width = 2,
                        offset = 5,
                        shiny::HTML(
                            "<br>
                            <p style='text-align:center'>Coming soon</p>
                            <br>
                            <br>"
                        )
                    )
                ),
                shiny::tabPanel(
                    title = "Module 3: MI-PCA deep-dive",
                    shiny::fluidRow(
                        shiny::column(
                            width = 4,
                            shiny::HTML(
                                "<br>
                            <br>"
                            ),
                            shiny::tabsetPanel(
                                type = "tabs",
                                shiny::tabPanel(
                                    title = "Introduction",
                                    shiny::htmlOutput("introduction")
                                ),
                                shiny::tabPanel(
                                    title = "1. Setup",
                                    shiny::htmlOutput("setup")
                                ),
                                shiny::tabPanel(
                                    title = "2. Correlation matrix",
                                    shiny::htmlOutput("heatmap_cor_int")
                                ),
                                shiny::tabPanel(
                                    title = "3. PC Loadings",
                                    shiny::htmlOutput("heatmap_load_int")
                                ),
                                shiny::tabPanel(
                                    title = "4. Non-graphical decision rules",
                                    shiny::htmlOutput("hist_int")
                                ),
                                shiny::tabPanel(
                                    title = "5. CPVE",
                                    shiny::htmlOutput("scatter_int")
                                ),
                                shiny::tabPanel(
                                    title = "6. Conclusions",
                                    shiny::htmlOutput("conclusions")
                                )
                            )
                        ),
                        shiny::column(
                            width = 8,
                            shiny::HTML(
                                "<br>
                            <br>"
                            ),
                            shiny::column(
                                width = 8,
                                offset = 2,
                                shiny::sliderInput(
                                    inputId = "colli",
                                    label = "Collinearity",
                                    min = 0,
                                    max = .9,
                                    value = .1,
                                    step = .1,
                                    width = "100%"
                                )
                            ),
                            shiny::column(
                                width = 4,
                                offset = 2,
                                shiny::plotOutput(
                                    outputId = "heatmap_cor"
                                ),
                                shiny::plotOutput(
                                    outputId = "hist"
                                ),
                                style = "height: 525px"
                            ),
                            shiny::column(
                                width = 4,
                                shiny::plotOutput(
                                    outputId = "heatmap_load"
                                ),
                                shiny::plotOutput(
                                    outputId = "scatter"
                                ),
                                style = "height: 525px"
                            ),
                            style = "border-left: 1px solid; border-left-color: #DDDDDD"
                        )
                    )
                ),
                shiny::tabPanel(
                    title = "Module 4: Resampling study",
                    shiny::column(
                        width = 2,
                        offset = 5,
                        shiny::HTML(
                            "<br>
                            <p style='text-align:center'>Coming soon</p>
                            <br>
                            <br>"
                        )
                    )
                ),
                shiny::tabPanel(
                    title = "Module 5: Convergence checks",
                    shiny::column(
                        width = 2,
                        offset = 5,
                        shiny::HTML(
                            "<br>
                            <p style='text-align:center'>Coming soon</p>
                            <br>
                            <br>"
                        )
                    )
                )
            )
        )
    )
    # Return ui
    return(ui)
}
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
                selected = "Collinearity study",
                shiny::tabPanel(
                    title = "Simulation study",
                    "Coming soon"
                ),
                shiny::tabPanel(
                    title = "Collinearity study",
                    shiny::HTML("<br>"),
                    shiny::column(
                        width = 3,
                        shiny::selectInput(
                            inputId = "tab3_dims",
                            label = "Number of columns in the data (p)",
                            choices = c(50, 500),
                            selected = 500
                        ),
                        shiny::selectInput(
                            inputId = "tab3_outcome",
                            label = "Performance Measure",
                            choices = c("PRB", "CIC", "CIW"),
                            selected = "PRB"
                        ),
                        shiny::checkboxGroupInput(
                            inputId = "tab3_rho",
                            label = "Strenght of the correlation",
                            choices = unique(res_exp_1_2$collinearity),
                            selected = range(res_exp_1_2$collinearity)
                        ),
                        shiny::checkboxGroupInput(
                            inputId = "tab3_methods",
                            label = "Missing data treatments",
                            choices = levels(res_exp_1_2$methods),
                            selected = levels(res_exp_1_2$methods)[1:12]
                        ),
                        shinyWidgets::sliderTextInput(
                            inputId = "tab3_xlim",
                            label = "X-axis range",
                            hide_min_max = TRUE,
                            choices = 0:100,
                            selected = c(0, 50),
                            grid = FALSE
                        )
                    ),
                    shiny::column(
                        width = 7,
                        offset = 1,
                        shiny::plotOutput(
                            outputId = "tab3_plot"
                        )
                    )
                ),
                shiny::tabPanel(
                    title = "MI-PCA deep-dive",
                    shiny::fluidRow(
                        shiny::column(
                            width = 4,
                            shiny::titlePanel(
                                shiny::h3("Understanding the MI-PCA behaviour", align = "center")
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
                            shiny::fluidRow(
                                shiny::titlePanel(
                                    shiny::h3("Input", align = "center")
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
                            ),
                            shiny::fluidRow(
                                shiny::titlePanel(
                                    shiny::h3("Plots", align = "center")
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
                                )
                            ),
                            style = "border-left: 1px solid; border-left-color: #DDDDDD"
                        )
                    )
                ),
                shiny::tabPanel(
                    title = "Resampling study",
                    "Coming soon"
                )
            )
        )
    )
    # Return ui
    return(ui)
}
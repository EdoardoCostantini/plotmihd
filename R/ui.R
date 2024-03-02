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
                            Costantini, E., Lang, K. M., Reeskens, T., & Sijtsma, K. (2023). High-Dimensional Imputation for the Social Sciences: A Comparison of State-of-The-Art Methods. <i>Sociological Methods & Research.</i> <a href='https://doi.org/10.1177/00491241231200194'>https://doi.org/10.1177/00491241231200194</a>
                            <br>
                            <br>
                            The app contains four modules that allow the user to interact with different results related to this article:
                            <ul>
                                <li><b>Module 1</b>: Interact with the main simulation study results.</li>
                                <li><b>Module 2</b>: Interact with the collinearity simulation study results.</li>
                                <li><b>Module 3</b>: Interact with a tool exploring the performance of MI-PCA for varying levels of collinearity.</li>
                                <li><b>Module 4</b>: Interact with the resampling study results.</li>
                            </ul>
                            For questions and feedback, just <a href = 'mailto:e.costantini@tilburguniversity.edu'>send me an email</a>.
                            "
                        )
                    )
                ),
                shiny::tabPanel(
                    title = "Module 1: Simulation study",
                    shiny::HTML("<br>"),
                    shiny::tabsetPanel(
                        shiny::tabPanel(
                            title = "Main results",
                            shiny::column(
                                width = 3,
                                shiny::HTML(
                                    "<br>
                            This tab allows you to plot the results of the main simulation study reported in the article. You change the values of the experimental factors to plot the results you are most interested in.
                            <br>
                            <br>
                            "
                                ),
                                shiny::selectInput(
                                    inputId = "tab2_dims",
                                    label = "Number of columns in the data",
                                    choices = c(50, 500),
                                    selected = 500
                                ),
                                shiny::selectInput(
                                    inputId = "tab2_outcome",
                                    label = "Performance Measure",
                                    choices = c("PRB", "CIC", "CIW"),
                                    selected = "PRB"
                                ),
                                shiny::checkboxGroupInput(
                                    inputId = "tab2_pm",
                                    label = "Proportion of missing cases",
                                    inline = TRUE,
                                    choices = unique(plotmihd::res_exp_1$pm),
                                    selected = unique(plotmihd::res_exp_1$pm)
                                ),
                                shiny::checkboxGroupInput(
                                    inputId = "tab2_methods",
                                    label = "Missing data treatments",
                                    choices = levels(plotmihd::res_exp_1$methods),
                                    selected = levels(plotmihd::res_exp_1$methods)[c(1:8, 10:12)]
                                ),
                                shinyWidgets::sliderTextInput(
                                    inputId = "tab2_xlim",
                                    label = "X-axis range",
                                    hide_min_max = TRUE,
                                    choices = 0:100,
                                    selected = c(0, 50),
                                    grid = FALSE
                                )
                            ),
                            shiny::column(
                                width = 9,
                                shiny::plotOutput(
                                    outputId = "tab2_plot"
                                ),
                                style = "border-left: 1px solid; border-left-color: #DDDDDD; height: 725px"
                            )
                        ),
                        shiny::tabPanel(
                            title = "Imputation time",
                            shiny::HTML("<br>"),
                            shiny::column(
                                width = 3,
                                shiny::selectInput(
                                    inputId = "tab2_time_dims",
                                    label = "Number of columns in the data",
                                    choices = c(50, 500),
                                    selected = 500
                                ),
                                shiny::selectInput(
                                    inputId = "tab2_time_pm",
                                    label = "Proportion of missing cases",
                                    choices = unique(plotmihd::res_exp_1_time$pm),
                                    selected = unique(plotmihd::res_exp_1_time$pm)[2]
                                ),
                                shiny::checkboxGroupInput(
                                    inputId = "tab2_time_methods",
                                    label = "Missing data treatments",
                                    choices = levels(plotmihd::res_exp_1_time$variable),
                                    selected = levels(plotmihd::res_exp_1_time$variable)
                                )
                            ),
                            shiny::column(
                                width = 9,
                                offset = 0,
                                shiny::plotOutput(
                                    outputId = "tab2_plot_time_main_sim"
                                )
                            )
                        ),
                        shiny::tabPanel(
                            title = "Convergence checks",
                            shiny::column(
                                width = 3,
                                shiny::HTML(
                                    "<br>
                                This tab allows you to interact with the trace plots for the imputation methods used in the simulation study.
                                <br>
                                <br>
                                "
                                ),
                                shiny::selectInput("tab_2_conv_method",
                                    "Imputation method:",
                                    choices = names(plotmihd::res_exp_1_mids[[1]]),
                                    selected = names(plotmihd::res_exp_1_mids[[1]])[1]
                                ),
                                shiny::selectInput("tab_2_conv_rep",
                                    "Repetition:",
                                    choices = 1:10,
                                    selected = 1
                                ),
                                shinyWidgets::sliderTextInput(
                                    inputId = "tab_2_conv_iters",
                                    label = "Iteration range",
                                    hide_min_max = TRUE,
                                    choices = 0:250,
                                    selected = c(0, 25),
                                    grid = FALSE
                                )
                            ),
                            shiny::column(
                                width = 9,
                                offset = 0,
                                shiny::plotOutput(
                                    outputId = "tab2_trace_plots"
                                )
                            )
                        )
                    )
                ),
                shiny::tabPanel(
                    title = "Module 2: Collinearity study",
                    shiny::HTML("<br>"),
                    shiny::tabsetPanel(
                        shiny::tabPanel(
                            title = "Main results",
                            shiny::column(
                                width = 3,
                                shiny::selectInput(
                                    inputId = "tab3_dims",
                                    label = "Number of columns in the data",
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
                                    label = "Collinearity",
                                    inline = TRUE,
                                    choices = unique(plotmihd::res_exp_1_2$collinearity),
                                    selected = range(plotmihd::res_exp_1_2$collinearity)
                                ),
                                shiny::checkboxGroupInput(
                                    inputId = "tab3_methods",
                                    label = "Missing data treatments",
                                    choices = levels(plotmihd::res_exp_1_2$methods)[c(1:9, 11:14)],
                                    selected = levels(plotmihd::res_exp_1_2$methods)[c(1:9, 11:14)]
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
                                width = 9,
                                shiny::plotOutput(
                                    outputId = "tab3_plot"
                                ),
                                style = "border-left: 1px solid; border-left-color: #DDDDDD; height: 725px"
                            )
                        ),
                        shiny::tabPanel(
                            title = "Imputation time",
                            shiny::HTML("<br>"),
                            shiny::column(
                                width = 3,
                                shiny::selectInput(
                                    inputId = "tab3_time_dims",
                                    label = "Number of columns in the data",
                                    choices = c(50, 500),
                                    selected = 500
                                ),
                                shiny::selectInput(
                                    inputId = "tab3_time_rho",
                                    label = "Collinearity",
                                    choices = unique(plotmihd::res_exp_1_2_time$collinearity),
                                    selected = unique(plotmihd::res_exp_1_2_time$collinearity)[4]
                                ),
                                shiny::checkboxGroupInput(
                                    inputId = "tab3_time_methods",
                                    label = "Missing data treatments",
                                    choices = levels(plotmihd::res_exp_1_2_time$variable),
                                    selected = levels(plotmihd::res_exp_1_2_time$variable)
                                )
                            ),
                            shiny::column(
                                width = 9,
                                offset = 0,
                                shiny::plotOutput(
                                    outputId = "tab3_plot_time"
                                )
                            )
                        ),
                        shiny::tabPanel(
                            title = "Convergence checks",
                            shiny::column(
                                width = 3,
                                shiny::HTML(
                                    "<br>
                                This tab allows you to interact with the trace plots for the imputation methods used in the simulation study.
                                <br>
                                <br>
                                "
                                ),
                                shiny::selectInput("tab_3_conv_method",
                                    "Imputation method:",
                                    choices = names(plotmihd::res_exp_1_2_mids[[1]]),
                                    selected = names(plotmihd::res_exp_1_2_mids[[1]])[1]
                                ),
                                shiny::selectInput("tab_3_conv_rep",
                                    "Repetition:",
                                    choices = 1:5,
                                    selected = 1
                                ),
                                shinyWidgets::sliderTextInput(
                                    inputId = "tab_3_conv_iters",
                                    label = "Iteration range",
                                    hide_min_max = TRUE,
                                    choices = 0:100,
                                    selected = c(0, 25),
                                    grid = FALSE
                                )
                            ),
                            shiny::column(
                                width = 9,
                                offset = 0,
                                shiny::plotOutput(
                                    outputId = "tab3_trace_plots"
                                )
                            )
                        )
                    )
                ),
                shiny::tabPanel(
                    title = "Module 3: MI-PCA deep-dive",
                    shiny::fluidRow(
                        shiny::column(
                            width = 5,
                            shiny::HTML(
                                "<br>"
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
                            width = 7,
                            shiny::HTML(
                                "<br>"
                            ),
                            shiny::column(
                                width = 10,
                                offset = 1,
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
                                width = 5,
                                offset = 1,
                                shiny::plotOutput(
                                    outputId = "heatmap_cor"
                                ),
                                shiny::plotOutput(
                                    outputId = "hist"
                                )
                            ),
                            shiny::column(
                                width = 5,
                                shiny::plotOutput(
                                    outputId = "heatmap_load"
                                ),
                                shiny::plotOutput(
                                    outputId = "scatter"
                                )
                            ),
                            style = "border-left: 1px solid; border-left-color: #DDDDDD; height: 725px"
                        )
                    )
                ),
                shiny::tabPanel(
                    title = "Module 4: Resampling study",
                    shiny::HTML("<br>"),
                    shiny::tabsetPanel(
                        shiny::tabPanel(
                            title = "Main results",
                            shiny::column(
                                width = 3,
                                shiny::HTML(
                                    "<br>
                            This tab allows you to plot the results of the resampling study reported in the article. You change the values of the experimental factors to plot the results you are most interested in.
                            <br>
                            <br>
                            "
                                ),
                                shiny::checkboxGroupInput(
                                    inputId = "tab4_n",
                                    label = "Sample size",
                                    inline = TRUE,
                                    choices = c(1000, 300),
                                    selected = c(1000, 300)
                                ),
                                shiny::selectInput(
                                    inputId = "tab4_outcome",
                                    label = "Performance Measure",
                                    choices = c("PRB", "CIC", "CIW"),
                                    selected = "bias_per"
                                ),
                                shiny::selectInput(
                                    inputId = "tab4_model",
                                    label = "Model",
                                    choices = c("m1", "m2"),
                                    selected = "m1"
                                )
                            ),
                            shiny::column(
                                width = 9,
                                shiny::plotOutput(
                                    outputId = "tab4_plot_res"
                                ),
                                style = "border-left: 1px solid; border-left-color: #DDDDDD; height: 725px"
                            )
                        ),
                        shiny::tabPanel(
                            title = "Imputation time",
                            shiny::HTML("<br>"),
                            shiny::column(
                                width = 3,
                                shiny::selectInput(
                                    inputId = "tab4_time_sample_size",
                                    label = "Sample size",
                                    choices = c(1000, 300),
                                    selected = 1000
                                ),
                                shiny::checkboxGroupInput(
                                    inputId = "tab4_time_methods",
                                    label = "Missing data treatments",
                                    choices = levels(plotmihd::res_exp_4_time$variable),
                                    selected = levels(plotmihd::res_exp_4_time$variable)
                                )
                            ),
                            shiny::column(
                                width = 9,
                                offset = 0,
                                shiny::plotOutput(
                                    outputId = "tab4_plot_time"
                                )
                            )
                        ),
                        shiny::tabPanel(
                            title = "Convergence checks",
                            shiny::column(
                                width = 3,
                                shiny::HTML(
                                    "<br>
                                This tab allows you to interact with the trace plots for the imputation methods used in the simulation study.
                                <br>
                                <br>
                                "
                                ),
                                shiny::selectInput("tab_5_conv_method",
                                    "Imputation method:",
                                    choices = names(plotmihd::res_exp_4_mids[[1]]),
                                    selected = names(plotmihd::res_exp_4_mids[[1]])[1]
                                ),
                                shiny::selectInput("tab_5_conv_rep",
                                    "Repetition:",
                                    choices = 1:10,
                                    selected = 1
                                ),
                                shinyWidgets::sliderTextInput(
                                    inputId = "tab_5_conv_iters",
                                    label = "Iteration range",
                                    hide_min_max = TRUE,
                                    choices = 0:250,
                                    selected = c(0, 25),
                                    grid = FALSE
                                )
                            ),
                            shiny::column(
                                width = 9,
                                offset = 0,
                                shiny::plotOutput(
                                    outputId = "tab5_trace_plots"
                                ),
                            )
                        )
                    )
                )
            )
        )
    )
    # Return ui
    return(ui)
}
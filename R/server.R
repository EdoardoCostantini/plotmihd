#' server
#'
#' server function for the shiny app
#'
#' @param input set of inputs coming from ui
#' @param output set of outputs
#' @param session session info and status
#' @author Edoardo Costantini, 2023
#' @export
server <- function(input, output, session) {

        # Tab 2: Simulation study ----------------------------------------------

        # Update X limits default input based on outcome (performance) measure
        observe({
            # Define subset of data in use
            if (input$tab2_outcome == "PRB") {
                tab2_xlim_choices <- 0:100
                tab2_xlim_selected <- c(0, 50)
            }
            if (input$tab2_outcome == "CIC") {
                tab2_xlim_choices <- 0:100
                tab2_xlim_selected <- c(70, 100)
            }
            if (input$tab2_outcome == "CIW") {
                tab2_xlim_choices <- 0:10
                tab2_xlim_selected <- c(0, 5)
            }
            shinyWidgets::updateSliderTextInput(
                session,
                inputId = "tab2_xlim",
                choices = tab2_xlim_choices,
                selected = tab2_xlim_selected
            )
        })

        # Main plot
        output$tab2_plot <- shiny::renderPlot(
            res = 96,
            height = 725,
            {
                plot_simulation(
                    res = res_exp_1,
                    dims = input$tab2_dims,
                    outcome = input$tab2_outcome,
                    meths = input$tab2_methods,
                    prop_NA = input$tab2_pm,
                    rho = unique(res_exp_1$collinearity),
                    reps = 1e3,
                    x_lims = input$tab2_xlim
                )
            }
        )

        # Tab 3: Collineairty study --------------------------------------------

        # Update X limits default input based on outcome (performance) measure
        observe({
            # Define subset of data in use
            if (input$tab3_outcome == "PRB") {
                tab3_xlim_choices <- 0:100
                tab3_xlim_selected <- c(0, 50)
            }
            if(input$tab3_outcome == "CIC"){
                tab3_xlim_choices <- 0:100
                tab3_xlim_selected <- c(70, 100)
            }
            if (input$tab3_outcome == "CIW") {
                tab3_xlim_choices <- 0:10
                tab3_xlim_selected <- c(0, 5)
            }
            shinyWidgets::updateSliderTextInput(
                session,
                inputId = "tab3_xlim",
                choices = tab3_xlim_choices,
                selected = tab3_xlim_selected
            )
        })

        # Main plot
        output$tab3_plot <- shiny::renderPlot(
            res = 96,
            height = 725,
            {
                plot_simulation(
                    res = res_exp_1_2,
                    dims = input$tab3_dims,
                    outcome = input$tab3_outcome,
                    meths = input$tab3_methods,
                    rho = input$tab3_rho,
                    prop_NA = unique(res_exp_1_2$pm),
                    reps = 500,
                    x_lims = input$tab3_xlim
                )
            }
        )

        # Tab 3: MI-PCA deep dive ----------------------------------------------

        # > Simulate data ------------------------------------------------------

        app_data <- reactive({
            # Simulate Data
            X <- gen_data(
                n = 5e3,
                block_sizes = c(5, 5, 40),
                block_rhos = c(.6, .3, .01),
                inflated_rhos = list(c(4, 5), c(9, 10), c(11:sum(c(5, 5, 40)))),
                inflated_values = as.numeric(input$colli)
            )

            # Prepare correlation matrix
            cor_mat <- round(cor(X)[c(1:12, 49:50), c(1:12, 49:50)], 1)

            # Subset X to desired active set for PCA
            X_ma <- X[, -c(1:3, 6:8)]

            # Perform PCA
            svd_X <- svd(X_ma)

            # Store the loadings
            load_mat <- svd_X$v

            # Give meaningful names to the loading matrix
            colnames(load_mat) <- paste0("PC", 1:ncol(svd_X$v))
            rownames(load_mat) <- colnames(X_ma)

            # Compute cumulative proportion of explained variance
            cpve <- cumsum(prop.table(svd_X$d^2))

            # Apply the decision rule used in the simulation study
            if (cpve[1] >= 0.5) {
                npcs_kpet <- 1
            } else {
                npcs_kpet <- sum(cpve <= 0.5)
            }

            # Non-graphical decision rules
            storenScree <- nFactors::nScree(as.data.frame(X_ma))$Components

            # Return the list of outputs
            list(
                X = X,
                X_ma = X_ma,
                cor_mat = cor_mat,
                load_mat = load_mat,
                cpve = cpve,
                npcs_kpet = npcs_kpet,
                storenScree = storenScree
            )
        })

        # > Correlation matrix -------------------------------------------------

        output$heatmap_cor <- shiny::renderPlot(
            res = 96,
            height = 400,
            {
                heatmap_correlation(
                    cor_mat = app_data()$cor_mat,
                    var_range = 1:12,
                    absolute = TRUE
                )
            }
        )

        # > Loading matrix -----------------------------------------------------

        output$heatmap_load <- shiny::renderPlot(
            res = 96,
            height = 400,
            {
            heatmap_loadings(
                load_mat = app_data()$load_mat,
                absolute = TRUE,
                var_range = c(1:6, 43:44),
                PCs_range = 1:10
            )
        })

        # > CPVE plot ----------------------------------------------------------

        output$scatter <- shiny::renderPlot(
            res = 96,
            height = 165,
            {
            scatter_cpve(
                cpve = app_data()$cpve,
                PCs_range = 1:10
            )
        })

        # > NPCS plot ----------------------------------------------------------

        output$hist <- shiny::renderPlot(
            res = 96,
            height = 150,
            {
            histogram_npcs(
                npcs_nscree = app_data()$storenScree,
                npcs_50rule = app_data()$npcs_kpet
            )
        })

        # > Text ---------------------------------------------------------------

        output <- tab_mi_pca_text(output)

        # Imputation time ------------------------------------------------------

        # Simulation study
        output$tab2_plot_time_main_sim <- shiny::renderPlot(
            res = 96,
            height = 725,
            {
                plot_time_simulation(
                    res = res_exp_1_time,
                    dims = input$tab2_time_dims,
                    meths = input$tab2_time_methods,
                    prop_NA = input$tab2_time_pm,
                    rho = 0,
                    x_lims = c(0, 90)
                )
            }
        )

        # Simulation study
        output$tab3_plot_time <- shiny::renderPlot(
            res = 96,
            height = 725,
            {
                plot_time_simulation(
                    res = res_exp_1_2_time,
                    dims = input$tab3_time_dims,
                    meths = input$tab3_time_methods,
                    prop_NA = 0.3,
                    rho = input$tab3_time_rho,
                    x_lims = c(0, 90)
                )
            }
        )

        # Tab 4: Resampling study ----------------------------------------------

        # Simulation study
        output$tab4_plot_res <- shiny::renderPlot(
            res = 96,
            height = 725,
            {
                plot_resampling(
                    res = res_exp_4,
                    outcome = c("bias_per", "ci_cov", "CIW")[1],
                    model = c("m1", "m2")[1],
                    type = "bias",
                    dt_reps = 500,
                    ci_lvl = .95,
                    meth_compare = c("DURR_la", "IURR_la", "blasso", "bridge", "MI_PCA", "MI_CART", "MI_RF", "stepFor", "CC")
                )
            }
        )

    }
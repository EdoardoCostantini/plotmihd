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

        # Tab 2: Collineairty study --------------------------------------------

        # Main plot
        output$tab3_plot <- shiny::renderPlot(
            res = 96,
            height = 725,
            {
                plot_simulation_colli(
                    res = res_exp_1_2,
                    dims = 500,
                    outcome = "PRB",
                    meths = levels(res$methods)[1:11],
                    rho = c(0, .9)
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

    }
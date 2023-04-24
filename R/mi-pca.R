#' Shiny app to understand MI-PCA results
#'
#' This function allows you to generate data according to different conditions explored in the reference study, and explore how PCA behaves on the data.
#'
#' @param argument_1 A description of the first argument
#' @param argument_2 A description of the second argument
#' @details
#' This function does such and such.
#' @return Starts a shiny app to explore the results.
#' @author Edoardo Costantini, 2023
#' @references
#'
#' Costantini, E., Lang, K. M., Reeskens, T., & Sijtsma, K. (2022). High-dimensional imputation for the social sciences: a comparison of state-of-the-art methods. arXiv preprint arXiv:2208.13656.
#'
#' @export
plotResults <- function() {
    # UI -----------------------------------------------------------------------
    ui <- shiny::fluidPage(

        # App title
        shiny::titlePanel("Understanding the MI-PCA behaviour"),

        # App UI
        shiny::fluidRow(
            shiny::column(
                width = 8, 
                "",
                shiny::sliderInput(
                    inputId = "colli",
                    label = "Collinearity",
                    min = 0,
                    max = .9,
                    value = .1,
                    step = .1,
                    width = "100%"
                )
            )
        ),
        shiny::fluidRow(
            shiny::column(
                width = 4, 
                "Data correlation structure", 
                shiny::plotOutput(outputId = "heatmap_cor")
                ),
            shiny::column(
                width = 4, 
                "Loadings", 
                shiny::plotOutput(outputId = "heatmap_load")
                )
        ),
        shiny::fluidRow(
            shiny::column(
                width = 4, 
                "Number of principal components kept", 
                shiny::plotOutput(outputId = "hist")
                ),
            shiny::column(
                width = 4, 
                "Cumulative proportion of explained variance", 
                shiny::plotOutput(outputId = "scatter")
                )
        )
    )

    # Server -------------------------------------------------------------------

    server <- function(input, output) {
        # Simulate data
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
            cor_mat <- round(cor(X)[c(1:13, 48:50), c(1:13, 48:50)], 1)

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
        output$heatmap_cor <- shiny::renderPlot({
            # Subset correlation matrix
            cor_mat_melt <- reshape2::melt(app_data()$cor_mat)

            # Add ellipsis as an empty level
            cor_mat_melt[, "Var1"] <- factor(
                x = cor_mat_melt[, "Var1"],
                levels = c(levels(cor_mat_melt[, "Var1"])[1:13], "...", levels(cor_mat_melt[, "Var1"])[-c(1:13)])
            )
            cor_mat_melt[, "Var2"] <- factor(
                x = cor_mat_melt[, "Var2"],
                levels = c(levels(cor_mat_melt[, "Var2"])[1:13], "...", levels(cor_mat_melt[, "Var2"])[-c(1:13)])
            )

            # Make heatmap
            ggplot2::ggplot(
                data = cor_mat_melt,
                ggplot2::aes(x = Var1, y = Var2, fill = value)
            ) +
                ggplot2::geom_tile(color = "white") +
                ggplot2::scale_fill_gradient2(
                    low = "blue",
                    high = "darkgray",
                    mid = "white",
                    midpoint = 0,
                    limit = c(0, 1),
                    space = "Lab",
                    name = ""
                ) +
                ggplot2::theme_bw() +
                ggplot2::theme(
                    axis.title.y = ggplot2::element_blank(),
                    axis.title.x = ggplot2::element_blank(),
                ) +
                ggplot2::coord_fixed() +
                ggplot2::scale_y_discrete(
                    drop = FALSE, # avoid dropping empty elements
                    limits = rev # reverse the y order
                ) +
                ggplot2::scale_x_discrete(
                    drop = FALSE, # avoid dropping empty elements
                    position = "top"
                )
        })

        # > Loading matrix -----------------------------------------------------

        output$heatmap_load <- shiny::renderPlot({
            # Subset correlation matrix
            load_mat_melt <- reshape2::melt(app_data()$load_mat[c(1:6, 43:44), 1:4])

            # Round values
            load_mat_melt$value <- abs(round(load_mat_melt$value, 3))

            # Add ellipsis as an empty level
            load_mat_melt[, "Var1"] <- factor(
                x = load_mat_melt[, "Var1"],
                levels = c(levels(load_mat_melt[, "Var1"])[1:6], "...", levels(load_mat_melt[, "Var1"])[-c(1:6)])
            )
            load_mat_melt[, "Var2"] <- factor(
                x = load_mat_melt[, "Var2"],
                levels = c(levels(load_mat_melt[, "Var2"])[1:6], "...", levels(load_mat_melt[, "Var2"])[-c(1:6)])
            )

            # Make heatmap
            ggplot2::ggplot(
                data = load_mat_melt,
                ggplot2::aes(x = Var1, y = Var2, fill = value)
            ) +
                ggplot2::geom_tile(color = "white") +
                ggplot2::scale_fill_gradient2(
                    low = "white",
                    high = "darkgray",
                    limit = c(min(load_mat_melt$value), max(load_mat_melt$value)),
                    space = "Lab",
                    name = ""
                ) +
                ggplot2::theme_bw() +
                ggplot2::theme(
                    axis.title.y = ggplot2::element_blank(),
                    axis.title.x = ggplot2::element_blank(),
                ) +
                ggplot2::coord_fixed() +
                ggplot2::scale_y_discrete(
                    drop = FALSE, # avoid dropping empty elements
                    limits = rev # reverse the y order
                ) +
                ggplot2::scale_x_discrete(
                    drop = FALSE, # avoid dropping empty elements
                    position = "top"
                )
        })

        # > CPVE plot ----------------------------------------------------------

        output$scatter <- shiny::renderPlot({
            # Round CPVE
            cpve_round <- app_data()$cpve # round(cpve, 2) * 100

            # Give PC number as name
            names(cpve_round) <- 1:length(cpve_round)

            # Attach collinearity as column
            cpve_data <- data.frame(
                npcs = factor(1:length(cpve_round)),
                value = cpve_round
            )

            # ggplot
            cpve_data %>%
                ggplot2::ggplot(
                    ggplot2::aes(
                        x = npcs,
                        y = value
                    )
                ) +
                ggplot2::geom_point() +
                ggplot2::ylab("") +
                ggplot2::xlab("Number of components") +
                ggplot2::theme_bw() +
                ggplot2::theme(
                    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
                )
        })

        # > NPCS plot ----------------------------------------------------------

        output$hist <- shiny::renderPlot({
            # Number of factors underlying data
            npcs_kept <- cbind(
                collinearity = as.numeric(input$colli),
                app_data()$storenScree,
                rule50 = app_data()$npcs_kpet
            )

            # Plot npcs
            npcs_kept <- reshape2::melt(npcs_kept, id.vars = "collinearity")

            npcs_kept %>%
                ggplot2::ggplot(
                    ggplot2::aes(
                        x = variable,
                        y = value,
                        label = value
                    )
                ) +
                ggplot2::geom_bar(stat = "identity", fill = "gray") +
                ggplot2::geom_text(colour = "black") +
                ggplot2::ylab("") +
                ggplot2::theme_bw() +
                ggplot2::theme(
                    axis.title.x = ggplot2::element_blank(),
                    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
                )
        })
    }

    # Run app ------------------------------------------------------------------

    shiny::shinyApp(ui, server)
}

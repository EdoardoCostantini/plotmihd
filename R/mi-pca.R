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

        # App title ----
        shiny::titlePanel("Understanding the MI-PCA behaviour"),

        # Sidebar layout with input and output definitions ----
        shiny::sidebarLayout(

            # Sidebar panel for inputs ----
            shiny::sidebarPanel(

                # Input: Slider for the number of bins ----
                shiny::sliderInput(
                    inputId = "bins",
                    label = "Number of bins:",
                    min = 1,
                    max = 50,
                    value = 30
                )
            ),

            # Main panel for displaying outputs ----
            shiny::mainPanel(
                # Output: Histogram ----
                shiny::plotOutput(outputId = "plot")
            )
        )
    )

    # Server -------------------------------------------------------------------

    server <- function(input, output) {
        # Histogram of the Old Faithful Geyser Data ----
        # with requested number of bins
        # This expression that generates a histogram is wrapped in a call
        # to renderPlot to indicate that:
        #
        # 1. It is "reactive" and therefore should be automatically
        #    re-executed when inputs (input$bins) change
        # 2. Its output type is a plot
        output$plot <- shiny::renderPlot({
            
            # Define experimental factor levels
            collinearity <- c(0.01, seq(0.1, .9, by = .1))

            # Crate a place to store factor structures
            storenScree <- list()
            cpve <- NULL
            pc1.loadings <- NULL
            npcs_kpet <- NULL
            cor_mats <- NULL

            i <- 9

            # Generate data
            X <- gen_data(
                n = 5e3,
                block_sizes = c(5, 5, 40),
                block_rhos = c(.6, .3, .01),
                inflated_rhos = list(c(4, 5), c(9, 10), c(11:sum(c(5, 5, 40)))),
                inflated_values = collinearity[i]
            )

            # Check correlation matrix
            cor_mats <- round(cor(X)[c(1:13, 48:50), c(1:13, 48:50)], 1)

            # Select possible auxiliary data
            X_ma <- X[, -c(1:3, 6:8)]

            # 3-factor structure
            storenScree <- nFactors::nScree(as.data.frame(X_ma))$Components

            # PCA
            svd_X <- svd(X_ma)

            # Store the loadings
            load_mat <- svd_X$v

            # Give meaningful names to the loading matrix
            colnames(load_mat) <- paste0("PC", 1:ncol(svd_X$v))
            rownames(load_mat) <- colnames(X_ma)
            
            # Compute CPVE for this run
            cpve_i <- cumsum(prop.table(svd_X$d^2))

            # Compute cumulative proportion of explained variance
            cpve <- cumsum(prop.table(svd_X$d^2))

            # Apply the decision rule used in the simulation study
            if (cpve[1] >= 0.5) {
                npcs_kpet <- c(npcs_kpet, 1)
            } else {
                npcs_kpet <- sum(cpve <= 0.5)
            }

            # > Correlation matrix ---------------------------------------------

            # Subset correlation matrix
            cor_mat_melt <- reshape2::melt(cor_mats)

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
            heatmap_cor <- ggplot2::ggplot(
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
                ) +
                ggplot2::labs(title = "Heatmap of the Pearson correlation matrix in the data")

            # > Loading matrix -------------------------------------------------

            # Subset correlation matrix
            load_mat_melt <- reshape2::melt(load_mat[c(1:6, 43:44), 1:4])

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
            heatmap_load <- ggplot2::ggplot(
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
                ) +
                ggplot2::labs(title = "Heatmap of the absolute value of the loadings on the Principal componnets")

            # Number of factors underlying data
            npcs_kept <- cbind(
                collinearity = collinearity[i],
                storenScree,
                rule50 = npcs_kpet
            )

            # Plot npcs
            npcs_kept <- reshape2::melt(npcs_kept, id.vars = "collinearity")

            hist_npcs <- npcs_kept %>%
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
                ) +
                ggplot2::labs(title = "Number of principal components kep according to 5 decision rules")

            # Round CPVE
            cpve_round <- cpve#round(cpve, 2) * 100

            # Give PC number as name
            names(cpve_round) <- 1:length(cpve_round)

            # Attach collinearity as column
            cpve_data <- data.frame(
                npcs = factor(1:length(cpve_round)),
                value = cpve_round
            )
            
            # ggplot
            scatter_cpve <- cpve_data %>%
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
                ) + 
                ggplot2::labs(title = "Cumulative proportion of explained variance by the PCs in the data matrix.")

                # Collect graphs
                    heatmap_cor + heatmap_load + hist_npcs + scatter_cpve +
                        patchwork::plot_layout(ncol = 2, widths = 1)

        })
    }

    # Run app ------------------------------------------------------------------

    shiny::shinyApp(ui, server)
}

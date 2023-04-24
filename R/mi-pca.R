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
                fluidRow(
                    splitLayout(
                        cellWidths = c("50%", "50%"), 
                        plotOutput("plotgraph1"), 
                        plotOutput("plotgraph2")
                        )
                ),

                # Output: Histogram ----
                shiny::plotOutput(outputId = "distPlot")
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
        output$distPlot <- shiny::renderPlot({
            
            # Define experimental factor levels
            collinearity <- c(0.01, seq(0.1, .9, by = .1))

            # Crate a place to store factor structures
            storenScree <- list()
            cpve <- NULL
            pc1.loadings <- NULL
            npcs_kpet <- NULL
            cor_mats <- NULL

            # Loop over the conditions
            for (i in 1:length(collinearity)) {
                
                # Generate data
                X <- gen_data(
                    n = 5e3,
                    block_sizes = c(5, 5, 40),
                    block_rhos = c(.6, .3, .01),
                    inflated_rhos = list(c(4, 5), c(9, 10), c(11:sum(c(5, 5, 40)))),
                    inflated_values = collinearity[i]
                )

                # Check correlation matrix
                cor_mats[[i]] <- round(cor(X)[c(1:13, 48:50), c(1:13, 48:50)], 1)

                # Select possible auxiliary data
                X_ma <- X[, -c(1:3, 6:8)]

                # 3-factor structure
                storenScree <- rbind(storenScree, nFactors::nScree(as.data.frame(X_ma))$Components)

                # PCA
                svd_X <- svd(X_ma)

                # Store the loadings
                pc1.loadings <- rbind(pc1.loadings, svd_X$v[, 1])

                # Compute CPVE for this run
                cpve_i <- cumsum(prop.table(svd_X$d^2))

                # Compute cumulative proportion of explained variance
                cpve <- rbind(cpve, cond = cpve_i)

                # Apply the decision rule used in the simulation study
                if (cpve_i[1] >= 0.5) {
                    npcs_kpet <- c(npcs_kpet, 1)
                } else {
                    npcs_kpet <- c(npcs_kpet, sum(cpve_i <= 0.5))
                }
            }

            # Subset correlation matrix
            cor_mat_melt <- reshape2::melt(cor_mats[[8]])

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
                    high = "red",
                    mid = "white",
                    midpoint = 0,
                    limit = c(0, 1),
                    space = "Lab",
                    name = "Pearson\nCorrelation"
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

            # Number of factors underlying data
            npcs_kept <- cbind(
                collinearity = collinearity,
                storenScree,
                rule50 = npcs_kpet
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
                ggplot2::geom_bar(stat="identity", fill = "gray") +
                ggplot2::facet_grid(cols = ggplot2::vars(collinearity)) + 
                ggplot2::geom_text(colour = "black") + 
                ggplot2::ylab("Number of principal components kept") + 
                ggplot2::theme_bw() + 
                ggplot2::theme(
                    axis.title.x = ggplot2::element_blank(),
                    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
                )

            # Round CPVE
            cpve_mat <- round(cpve, 2) * 100

            # Give PC number as name
            colnames(cpve_mat) <- 1:ncol(cpve_mat)

            # Attach collinearity as column
            cpve_data <- cbind(
                collinearity = collinearity,
                cpve_mat
            )
            
            # Melt for ggplot
            cpve_data <- reshape2::melt(
                as.data.frame(cpve_data),
                id.vars = "collinearity"
            )

            # Make number of pcs a numeric vector
            cpve_data$variable <- as.numeric(as.character(cpve_data$variable))

            # ggplot
            cpve_data %>%
                ggplot2::ggplot(
                    ggplot2::aes(
                        x = variable,
                        y = value,
                        label = value
                    )
                ) +
                ggplot2::geom_point() +
                ggplot2::facet_grid(cols = ggplot2::vars(collinearity)) +
                ggplot2::ylab("Cumulative proportion of explained variance") +
                ggplot2::xlab("Number of components") +
                ggplot2::theme_bw() +
                ggplot2::theme(
                    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
                )

            # Round CPVE
            pc_loading <- round(abs(pc1.loadings), 1) * 10

            # Give PC number as name
            colnames(pc_loading) <- colnames(X_ma)

            # Attach collinearity as column
            pc_loading_data <- cbind(
                collinearity = collinearity,
                pc_loading[, 1:9]
            )

            # Melt for ggplot
            pc_loading_data <- reshape2::melt(
                as.data.frame(pc_loading_data),
                id.vars = "collinearity"
            )

            pc_loading_data %>%
                ggplot2::ggplot(
                    ggplot2::aes(
                        x = variable,
                        y = value,
                        label = value
                    )
                ) +
                ggplot2::geom_bar(stat = "identity", fill = "gray") +
                ggplot2::facet_grid(cols = ggplot2::vars(collinearity)) +
                ggplot2::geom_text(colour = "black") +
                ggplot2::ylab("Number of principal components kept") +
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

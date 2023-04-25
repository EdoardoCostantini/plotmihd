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
        shiny::titlePanel(
            shiny::h1("Understanding the MI-PCA behaviour", align = "center")
        ),
        fluidRow(
            column(
                width = 4,
                offset = 1,
                shiny::titlePanel(
                    shiny::h3("Interpretation", align = "center")
                ),
                shiny::tabsetPanel(
                    type = "tabs",
                    shiny::tabPanel(
                        title = "1. Setup",
                        shiny::HTML(
                            "<br>
                            This shiny app generates data according to the same mechanism defined in study ---.
                            The data generating model requires sampling 200 observations from a multivariate normal distribution.
                            These observations are stored in a data matrix X.
                            The data is generated based on a known covariance matrix with a 3-block structure:
                            <ul>
                                <li>block 1: variables correlated by 0.6</li>
                                <li>block 2: variables correlated by 0.3</li>
                                <li>block 3: noise variables (extra variables not related to the ones in blocks 1 and 2.)</li>
                            </ul>
                            In the simulation study presented in ---, missing values were imposed on items --- ---, the target variables, based on variables --- ---, the MAR predictors.
                            The imputation model for the first variable uses as predictors the remaining target variables and the number of components that explains at least 50% of the variance in the set of variables composed of the MAR predicotrs and the noise variables.
                            We refer to the set of variables on which PCA is performed as Z.
                            <br>
                            <br>"
                        )
                    ),
                    shiny::tabPanel(
                        title = "2. Correlation matrix",
                        shiny::HTML(
                            "<br>
                            The heatmap in panel A shows the correlation matrix computed on X.
                            The gradient represents the strength of association, with darker colors representing higher correlation values.
                            <br>
                            <br>
                            As you change the values of the <code>Collinearity</code> input, you will notice changes in the correlation between variables <b>v4</b> and <b>v5</b>, <b>v9</b> and <b>v10</b>., and variables <b>v11</b> to <b>v15</b>.
                            <br>
                            Note the following:
                            <ul>
                                <li>The correlation between variables <b>v1</b> to <b>v10</b> and the variables <b>v11</b> to <b>v50</b> is fixed to approximately 0, irrespective of the value of the input <code>Collinearity</code>.
                                This preserves the understanding of variables <b>v11</b> to <b>v50</b> as noise variables in the task of imputing variables <b>v1</b> to <b>v10</b>.</li>
                                <li>Expect for variables <b>v4</b>, <b>v5</b>, <b>v9</b>, and <b>v10</b> The correlation between variables <b>v1</b> to <b>v10</b> also stays the same. This preserves the same strength of the MAR mechanism produced in the simulation study. Because <b>v4</b>, <b>v5</b>, <b>v9</b>, and <b>v10</b> are the MAR predictors used to impose missing values on <b>v1</b> to <b>v3</b> and <b>v5</b> to <b>v7</b>, these correlations need to stay constant, otherwise, the strength of the MAR mechanism would change together with the collinearity.</li>
                            </ul>
                            <br>
                            <br>"
                        )
                    ),
                    shiny::tabPanel(
                        title = "3. PC Loadings",
                        shiny::HTML(
                            "<br>
                            When using the MI-PCA method, PCA is performed on Z.
                            The heatmap in panel B shows the principal component loadings for the first 4 PCs.
                            The color gradient represents the weight of a column of Z in the linear combination defining the PCs.
                            Darker colors representing higher loadings (or wieghts) compared to other items.
                            <br>
                            <br>
                            When <code>Collinearity</code> is set to 0, the PC loadings for items 4, 5, 9, and 10 are many orders of magnitudes larger than the ones for all other items.
                            Although no loading is ever equal to 0, this configuration can be understood as generating a first principal component that is a linear combination of items 4, 5, 9, and 10.
                            As you increase the <code>Collinearity</code> input value, you will notice that the first PC becomes a linear combination of the noise variables, while items 4, 5, 9, and 10 are more important (higher weights) in the computation of the second and third PCs.
                            <br>
                            <br>"
                        )
                    ),
                    shiny::tabPanel(
                        title = "4. Non-graphical decision rules",
                        shiny::HTML(
                            "<br>
                            The histogram in Panel C shows the number of PCs kept when performing PCA on the correlation matrix of Z.
                            Five non-graphical decision rules are reported:
                            <ul>
                                <li>Optimal coordinates index (noc)</li>
                                <li>Acceleration factor (naf)</li>
                                <li>Parallel analysis (nparallel)</li>
                                <li>Kaiser criterion (nkaiser)</li>
                                <li>50% rule (rule50)</li>
                            </ul>
                            When <code>Collinearity</code> is set to 0, the number of components selected by the 50% rule used in this study is quite high (around 19). As you increase the value of <code>Collinearity</code>, the number selected by this rule decreases and reaches 1 for values greater than 0.5.
                            For the same values, the Kaiser rule and the parallel analysis always identify 2 or more PCs.
                            <br>
                            <br>"
                        )
                    ),
                    shiny::tabPanel(
                        title = "5. CPVE",
                        shiny::HTML(
                            "<br>
                            The scatter plot in Panel D reports the Cumulative proportion of variance explained by subsequent components obtained from the PCA of Z.
                            <br>
                            <br>
                            When <code>Collinearity</code> is set to 0, the first PC explains very little of the total variance in Z (CPVE < 0.01).
                            For larger values of the <code>Collinearity</code> input, the first PC explains more and more of the total variance.
                            As the strength of the associaiont between the variables from v11 to v50 increases, the more of the total variance in the data set is explained by the one component representing these variables.
                            For the highest values of <code>Collinearity</code>, an elbow shape appears in Panel D: the first 3 PCs collectively explain the majority of the variance in Z, and additional PCs only add a trivial amount of explained variance.
                            <br>
                            <br>"
                        )
                    ),
                    shiny::tabPanel(
                        title = "6. Conclusions",
                        shiny::HTML(
                            "<br>
                            For a <code>Collinearity</code> of 0, the important predictors contribute strongly to the computation of the first PC (high-loadings). 
                            In this scenario, it would probably be sufficient to use this single PC as a predictor in the imputation models, but based on the 50% rule we actually use the first PC together with the next 18 PCs.
                            The MAR predictors are present in the imputaiton model through the first PC, which is a linear combination of all of the variables in Z where the MAR predictors have high weights and the noise variables have weights close to 0.
                            <br>
                            <br>
                            For a correlation of .9, the 50% rule retains a single PC that explains more than 70% of the variance in X.
                            However, this single component is a linear combination where the noise variables are weighted much more than the important predictors.
                            The only additional predictor MI-PCA is using comapred to MI-AM is a linear combiantion of noise variables.
                            As a result, MI-PCA will perform similarly to MI-AM in conditions with high correlations among the zoise variables.
                            Had we used the Kaiser criterion or parallel analysis, we would have kept the first three PCs, which would have included a first PC that is useless for imputation (a combination of noise variables) and the two following important PCs (a combination of the MAR predictors.)
                            <br>
                            <br>"
                        )
                    )
                )
            ),
            column(
                width = 6,
                fluidRow(
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
                    ),
                    shinyBS::bsPopover(
                        id = "colli",
                        title = "Collinearity",
                        content = shiny::HTML("The value of the correlation between variables <b>v4</b> and <b>v5</b>, <b>v9</b> and <b>v10</b>, and between the block on noise variables (<b>v11</b> and above.)"),
                        trigger = "hover",
                        placement = "left",
                        options = list(container = "body")
                    ),
                    )
                ),
                fluidRow(
                    shiny::titlePanel(
                        shiny::h3("Plots", align = "center")
                    ),
                    fluidRow(
                    shiny::column(
                        width = 6,
                        shiny::titlePanel(
                            shiny::h5("Panel A", align = "center")
                        ),
                        shiny::plotOutput(outputId = "heatmap_cor")
                    ),
                    shiny::column(
                        width = 6,
                        shiny::titlePanel(
                            shiny::h5("Panel B", align = "center")
                        ),
                        shiny::plotOutput(outputId = "heatmap_load")
                    )
                    ),
                    fluidRow(
                    shiny::column(
                        width = 6,
                        shiny::titlePanel(
                            shiny::h5("Panel C", align = "center")
                        ),
                        shiny::plotOutput(outputId = "hist")
                    ),
                    shiny::column(
                        width = 6,
                        shiny::titlePanel(
                            shiny::h5("Panel D", align = "center")
                        ),
                        shiny::plotOutput(outputId = "scatter")
                    )
                    )
                )
            )
        )
    )

    # Server -------------------------------------------------------------------

    server <- function(input, output, session) {
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
                    legend.position = "bottom",
                    aspect.ratio = 1
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
                    legend.position = "bottom",
                    aspect.ratio = 1
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
            )[1:10, ]

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
                ggplot2::xlab("") +
                ggplot2::theme_bw() +
                ggplot2::theme(
                    aspect.ratio = 1
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
                    aspect.ratio = 1
                )
        })

        # > Tooltips -----------------------------------------------------------

        # Correlation matrix
        shinyBS::addPopover(
            session,
            id = "heatmap_cor",
            title = "Correlation matrix",
            content = shiny::HTML("This heatmap shows darker colors for higher values of bivariate correlations. Bivariate correlations for variables <b>v14</b> to <b>v47</b> are omitted as they are always close to the others in this block (<b>v11</b> to <b>v50</b>.).
            <br>
            <br>
            As you change the values of the <code>Collinearity</code> input, you will notice changes in the correlation between variables <b>v4</b> and <b>v5</b>, <b>v9</b> and <b>v10</b>., and variables <b>v11</b> to <b>v15</b>"),
            trigger = "click",
            placement = "bottom",
            options = list(container = "body")
        )

        # Loadings
        shinyBS::addPopover(
            session,
            id = "heatmap_load",
            title = "Principal component loadings",
            content = shiny::HTML("This heatmap shows darker colors for higher absolute values of the principal component loadings.
            For a given item, the higher the loading, the higher its influence in the linear combination generating the PC scores compared to the other items.
            <br>
            <br>
            As you change the values of the <code>Collinearity</code> input, you will notice that the first component goes from being a combination of items <b>v4</b>, <b>v5</b>, <b>v9</b>, and <b>v10</b>, to being a combination of the noise items (<b>v11</b> and above.)"),
            trigger = "click",
            placement = "bottom",
            options = list(container = "body")
        )

        # Number of principal components kept
        shinyBS::addPopover(
            session,
            id = "hist",
            title = "Number of principal components",
            content = shiny::HTML("none"),
            trigger = "click",
            placement = "bottom",
            options = list(container = "body")
        )

        # Cumulative proportion of explained variance
        shinyBS::addPopover(
            session,
            id = "scatter",
            title = "Cumulative proportion of variance explained (CPVE)",
            content = shiny::HTML(
                "This scatter plot reports the CPVE by subsequent components obtained by performing PCA on the correlation matrix."
            ),
            trigger = "click",
            placement = "bottom",
            options = list(container = "body")
        )
    }

    # Run app ------------------------------------------------------------------

    shiny::shinyApp(ui, server)
}

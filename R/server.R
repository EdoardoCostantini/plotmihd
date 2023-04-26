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
        output$heatmap_cor <- shiny::renderPlot({
            heatmap_correlation(
                cor_mat = app_data()$cor_mat,
                var_range = 1:12,
                absolute = TRUE
            )
        })

        # > Loading matrix -----------------------------------------------------

        output$heatmap_load <- shiny::renderPlot({
            heatmap_loadings(
                load_mat = app_data()$load_mat,
                absolute = TRUE,
                var_range = c(1:6, 43:44),
                PCs_range = 1:10
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
                ggplot2::ylab("CPVE") +
                ggplot2::xlab("Number of PCs") +
                ggplot2::theme_bw() +
                ggplot2::theme(
                    aspect.ratio = 1
                )
        })

        # > NPCS plot ----------------------------------------------------------

        output$hist <- shiny::renderPlot({
            histogram_npcs(
                npcs_nscree = app_data()$storenScree,
                npcs_50rule = app_data()$npcs_kpet
            )
        })

        # > Interpretation Panel -----------------------------------------------

        # Introduction ---------------------------------------------------------
        output$introduction <- renderText({
            shiny::HTML(
                "<br>
                The simulation study developed by <a href='https://arxiv.org/abs/2208.13656'>Costantini et. al. (2022)</a> to compare different data-driven imputation model-building strategies showed that, for higher degrees of collinearity, MI-PCA performed similarly to MI-AM.
                The numbered tabs in this section will help to understand the MI-PCA performance observed in the study.
                <br>
                <br>"
            )
        })


        # 1. Set up ------------------------------------------------------------
        output$setup <- renderText({
            shiny::HTML(
                "<br>
                The shiny app in this tab is helpful to understand the behavior observed in the study.
                <br>
                <br>
                This Shiny app generates data according to the same data-generating procedure defined in Costantini et. al. (2022).
                The original study sampled 200 observations from a multivariate normal distribution using a known covariance matrix with a 3-block structure:
                <ul>
                    <li><b>Block 1</b>: variables v1 to v5 were correlated by 0.6 among themselves.</li>
                    <li><b>Block 2</b>: variables v6 to v10 were correlated by 0.3 among themselves and with the variables in block 1.</li>
                    <li><b>Block 3</b>: variables v11 to v50 were uncorrelated among themselves and with the variables in blocks 2 and 3,</li>
                </ul>
                In the original simulation study, missing values were imposed on items v1 to v3 and v5 to v7 based on variables v4, v5, v9, and v10 (MAR predictors).
                <b>MI-PCA</b> defined the <b>imputation model</b> for each of the variables with missing values to include as predictors:
                <ul>
                    <li> The other variables with missing values (same as MI-AM.)</li>
                    <li> The first principal components explaining at least 50% of the variance in the set of possible auxiliary predictors (composed of the MAR predictors and the variables in block 3, namely v4, v5, v9, v10, and v11 to v50.)</li>
                </ul>
                <br>
                <br>"
            )
        })

        # 2. Correlation matrix ------------------------------------------------------------
        output$heatmap_cor_int <- renderText({
            shiny::HTML(
                "<br>
                During the review process, one referee highlighted the need to explore how the strategies compared in the original study performed for varying degrees of collinearity.
                We introduced collinearity as an experimental factor in the simulation study by allowing the correlation between certain variables to vary.
                <br>
                <br>
                The <b>heatmap in Panel A</b> shows the correlation matrix of the data for different levels of the collinearity factor.
                The <b>color gradient</b> represents the strength of the bivariate associations, with darker colors representing higher correlation values.
                <br>
                <br>
                As you change the values of the <code>Collinearity</code> input, you will notice changes in the correlation between variables v4 and v5, v9 and v10, and variables v11 to v15.
                <br>
                Note the following:
                <ul>
                    <li>The correlation between variables v1 to v10 and the variables v11 to v50 is fixed to approximately 0, irrespective of the value of the input <code>Collinearity</code>.
                        This <b>preserved</b> the understanding of variables v11 to v50 as <b>noise variables</b> in the task of imputing variables v1 to v10.
                    <li>Expect for variables v4, v5, v9, and v10 The correlation between variables v1 to v10 also stays the same. This preserved the same strength of the MAR mechanism produced in the original simulation study. Because v4, v5, v9, and v10 are the MAR predictors used to impose missing values on v1 to v3 and v5 to v7, these correlations needed to stay constant, otherwise, the <b>strength of the MAR mechanism</b> would have changed together with the collinearity.</li>
                </ul>
                <br>
                <br>"
            )
        })

        # 3. Loading matrix ------------------------------------------------------------
        output$heatmap_load_int <- renderText({
            shiny::HTML(
                "<br>
                 The <b>heatmap in Panel B</b> shows the <b>principal component loadings</b> for the first 10 principal components (PCs).
                 The color gradient represents the weight of a variable in the linear combination defining the PCs.
                 Darker colors indicate higher loadings (or weights) compared to other items.
                 Although no loading is ever exactly equal to 0, white and light gray indicate that a variable is only a weak contribution to the computation of a given PC.
                 <br>
                 <br>
                 When <code>Collinearity</code> is set to 0, the PC loadings for v4, v5, v9, and v10 are many orders of magnitudes larger than the ones for all other variables.
                 This configuration can be understood as generating a first principal component that is a good representation of the <b>MAR predictors</b>.
                 As you increase the <code>Collinearity</code> input value, you will notice that the first PC becomes a linear combination of the <b>noise variables</b>, while v4, v5, v9, and v10 become important only in the computation of the second and third PCs.
                 <br>
                 <br>"
            )
        })

        # 4. NPCS --------------------------------------------------------------
        output$hist_int <- renderText({
            shiny::HTML(
                "<br>
                The <b>histogram in Panel C</b> shows the number of PCs kept when performing PCA on the potential auxiliary variables.
                Five <b>non-graphical decision rules</b> are reported:
                <ul>
                    <li>Optimal coordinates index (noc)</li>
                    <li>Acceleration factor (naf)</li>
                    <li>Parallel analysis (nparallel)</li>
                    <li>Kaiser criterion (nkaiser)</li>
                    <li>50% rule (rule50) - This is the rule used in <a href='https://arxiv.org/abs/2208.13656'>Costantini et. al. (2022)</a></li>
                </ul>
                When <code>Collinearity</code> is set to 0, the number of components selected by the <b>50% rule</b> used in this study is quite high (around 19).
                As you increase the value of <code>Collinearity</code>, the number selected by this rule decreases and reaches 1 for correlation values greater than 0.5.
                For the same values, the <b>Kaiser rule</b> and the <b>parallel analysis</b> always identify 2 or more PCs.
                <br>
                <br>"
            )
        })

        # 5. CPVE --------------------------------------------------------------
        output$scatter_int <- renderText({
            shiny::HTML(
                "<br>
                The <b>scatter plot in Panel D</b> reports the cumulative proportion of variance explained (CPVE) by the first 10 PCs obtained from the potential auxiliary variables.
                <br>
                <br>
                When <code>Collinearity</code> is set to 0, the <b>first PC</b> explains very little of the total variance in the variables considered (CPVE < 0.01).
                For larger values of the <code>Collinearity</code> input, the first PC explains more and more of the total variance.
                As the strength of the association between the variables from v11 to v50 increases, more of the total variance is explained by the one component representing these variables.
                For the highest values of <code>Collinearity</code>, an <b>elbow shape</b> appears in Panel D: the first 3 PCs collectively explain the majority of the variance in Z, and additional PCs only add a trivial amount of explained variance.
                <br>
                <br>"
            )
        })

        # 6. Conclusions -------------------------------------------------------
        output$conclusions <- renderText({
            shiny::HTML(
                "<br>
                For a <code>Collinearity</code> of 0, the <b>MAR predictors</b> contribute strongly to the computation of the first PC, because of the high loadings.
                In this scenario, it would be sufficient to use this single PC as a predictor in the imputation models defined by MI-PCA, but based on the 50% rule <a href='https://arxiv.org/abs/2208.13656'>Costantini et. al. (2022a)</a> actually used the first PC together with the next 18 PCs.
                As a result, the MAR predictors were present in the imputation model through the first PC, leading to the good imputation performance seen in the original study.
                <br>
                <br>
                For a <code>Collinearity</code> of .9, the 50% rule retains a single PC that explains more than 70% of the variance in X.
                However, this single component is a linear combination where the <b>noise variables</b> are weighted much more than the MAR predictors.
                As a result, in <a href='https://arxiv.org/abs/2208.13656'>Costantini et. al. (2022a)</a>, the only additional predictor MI-PCA used compared to MI-AM was a linear combination of noise variables.
                <br>
                <br>
                In <a href='https://arxiv.org/abs/2208.13656'>Costantini et. al. (2022a)</a>, MI-PCA performed similarly to MI-AM in conditions with high correlations among the noise variables because the only additional predictor was a linear combination of noise variables.
                However, this does not suggest MI-PCA is inadequate for highly collinear data.
                PCA does what it should: higher correlation values in the block of noise variables make this block the largest variation to explain, and the first PC will summarise it well.
                The problem for MI-PCA was that the first component extracted had little relevance for the imputation task.
                However, as confirmed in <a href='https://arxiv.org/abs/2206.15107'>Costantini et. al. (2022b)</a>, when using the <b>true number of components</b> summarizing all axis of variation of a data set, MI-PCA leads to low bias and close-to-nominal confidence interval coverage.
                Furthermore, as shown in <a href='https://arxiv.org/abs/2206.15107'>Costantini et. al. (2023)</a>, adding a <b>supervision</b> element to MI-PCA can help in making sure that the first PCs computed is helpful for the imputation task.
                <br>
                <br>"
            )
        })
    }
#' Tab text: MI-PCA
#'
#' A function to add text to the server output object for the MI-PCA tab.
#'
#' @param output list of output objects used in the server
#' @return Updated `output` object to be processed by ui and server
#' @author Anonymized, 2023
#'
#' @export
tab_mi_pca_text <- function(output) {
    # Introduction ---------------------------------------------------------
    output$introduction <- renderText({
        shiny::HTML(
            "<br>
                The simulation study developed by <a href='https://arxiv.org/abs/anonymized'>Anonymized et. al. (2022)</a> to compare different data-driven imputation model-building strategies showed that, for higher degrees of collinearity, MI-PCA performed similarly to MI-AM.
                The numbered tabs in this section will help to understand the MI-PCA performance observed in the study.
                <br>
                <br>"
        )
    })


    # 1. Set up ------------------------------------------------------------
    output$setup <- renderText({
        shiny::HTML(
            "<br>
                This Shiny app generates data according to the same data-generating procedure defined in Anonymized et. al. (2022).
                The original study sampled 200 observations from a multivariate normal distribution using a known covariance matrix with a 3-block structure:
                <ul>
                    <li><b>Block 1</b>: variables z1 to z5 were correlated by 0.6 among themselves.</li>
                    <li><b>Block 2</b>: variables z6 to z10 were correlated by 0.3 among themselves and with the variables in block 1.</li>
                    <li><b>Block 3</b>: variables z11 to z50 were uncorrelated among themselves and with the variables in blocks 2 and 3,</li>
                </ul>
                In the original simulation study, missing values were imposed on items z1 to z3 and z5 to z7 based on variables z4, z5, z9, and z10 (MAR predictors).
                For each of the variables with missing values, <b>MI-PCA</b> included the following variables as predictors in the <b>imputation model</b>:
                <ul>
                    <li> The other variables with missing values (same as MI-AM.)</li>
                    <li> The first principal components explaining at least 50% of the variance in the set of potential auxiliary predictors (composed of the MAR predictors and the variables in block 3, namely z4, z5, z9, z10, and z11 to z50.)</li>
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
                We introduced collinearity as an experimental factor in the simulation study by allowing the correlation between certain variables to zary.
                <br>
                <br>
                The <b>heatmap in Panel A</b> shows the correlation matrix of the data for different levels of the collinearity factor.
                The <b>color gradient</b> represents the strength of the bivariate associations, with darker colors representing higher correlation values.
                <br>
                <br>
                As you change the values of the <code>Collinearity</code> input, you will notice changes in the correlation between variables z4 and z5, z9 and z10, and variables z11 to z50.
                <br>
                Note the following:
                <ul>
                    <li>The correlation between the block of variables z1 to z10 and the block of variables z11 to z50 is fixed to approximately 0, irrespective of the value of the input <code>Collinearity</code>.
                        This <b>preserved</b> the understanding of variables z11 to z50 as <b>noise variables</b> in the task of imputing any variable in the block z1 to z10.
                    <li>Expect for variables z4, z5, z9, and z10, the correlation between variables z1 to z10 remained constant. Because z4, z5, z9, and z10 are the MAR predictors used to generate missing values on z1 to z3 and z5 to z7, these correlations needed to stay constant to preserve the same strength of the MAR mechanism produced in the original simulation study. Had we changed the correlation between the MAR predictors and the variables with missing values, we would have changed the <b>strength of the MAR mechanism</b> together with the collinearity. This would have then made it impossible to attribute any change in the performance of the methods to either the different degrees of collinearity or the different strength of MAR.</li>
                </ul>
                <br>
                <br>"
        )
    })

    # 3. Loading matrix ------------------------------------------------------------
    output$heatmap_load_int <- renderText({
        shiny::HTML(
            "<br>
                 The <b>heatmap in Panel B</b> shows the absolute value of the <b>principal component loadings</b> for the first 10 principal components (PCs).
                 The color gradient represents the weight of a variable in the linear combination defining the PCs.
                 Darker colors indicate higher loadings (or weights) compared to other items.
                 Although no loading is ever exactly equal to 0, white and light gray indicate that a variable is only a weak contribution to the computation of a given PC.
                 <br>
                 <br>
                 When <code>Collinearity</code> is set to 0, the first PC loadings for z4, z5, z9, and z10 are many orders of magnitudes larger than the ones for all other variables.
                 This configuration can be understood as generating a first PC that is a good representation of the <b>MAR predictors</b>.
                 As you increase the <code>Collinearity</code> input value, you will notice that the first PC becomes a linear combination of the <b>noise variables</b>, while z4, z5, z9, and z10 become important only in the computation of the second and third PCs.
                 <br>
                 <br>"
        )
    })

    # 4. NPCS --------------------------------------------------------------
    output$hist_int <- renderText({
        shiny::HTML(
            "<br>
                The <b>histogram in Panel C</b> shows the number of PCs kept when performing PCA on the potential auxiliary variables (v4, z5, z9, z10, and z11 to z50).
                Five <b>non-graphical decision rules</b> were considered:
                <ul>
                    <li>Optimal coordinates index (noc)</li>
                    <li>Acceleration factor (naf)</li>
                    <li>Parallel analysis (nparallel)</li>
                    <li>Kaiser criterion (nkaiser)</li>
                    <li>50% rule (rule50) - This is the rule used in <a href='https://arxiv.org/abs/anonymized'>Anonymized et. al. (2022)</a></li>
                </ul>
                When <code>Collinearity</code> is set to 0, the number of components selected by the <b>50% rule</b> is quite high (around 19).
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
                As the strength of the association between the variables from z11 to z50 increases, more of the total variance is explained by the one component representing these variables.
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
                In this scenario, it would be sufficient to use this single PC as a predictor in the imputation models defined by MI-PCA, but based on the 50% rule <a href='https://arxiv.org/abs/anonymized'>Anonymized et. al. (2022a)</a> actually used the first PC together with the next 18 PCs.
                As a result, the MAR predictors were present in the imputation model through the first PC, leading to the good imputation performance seen in the original study.
                <br>
                <br>
                For a <code>Collinearity</code> of .9, the 50% rule retains a single PC that explains more than 80% of the variance in the potential auxiliary variables.
                However, this single component is a linear combination where the <b>noise variables</b> are weighted much more than the MAR predictors.
                As a result, in this situation, the only additional predictor MI-PCA would use, compared to MI-AM, is a linear combination of noise variables.
                <br>
                <br>
                In <a href='https://arxiv.org/abs/anonymized'>Anonymized et. al. (2022a)</a>, MI-PCA performed similarly to MI-AM in conditions with high correlations among the potential auxiliary variables because the only additional predictor was a linear combination of noise variables.
                However, this does not suggest MI-PCA is inadequate for highly collinear data.
                PCA does what it should: higher correlation values in the block of noise variables make this block the largest variation to explain, and the first PC will summarise it well.
                The problem for MI-PCA was that the first component extracted had little relevance for the imputation task.
                However, as confirmed in <a href='https://arxiv.org/abs/2206.15107'>Anonymized et. al. (2022b)</a>, when using the <b>true number of components</b> summarizing all axis of variation of a data set, MI-PCA leads to low bias and close-to-nominal confidence interval coverage.
                Furthermore, as shown in <a href='https://arxiv.org/abs/2206.15107'>Anonymized et. al. (2023)</a>, adding a <b>supervision</b> element to MI-PCA can help in making sure that the first PCs computed is helpful for the imputation task.
                <br>
                <br>"
        )
    })

    # Return updated output object
    output
}
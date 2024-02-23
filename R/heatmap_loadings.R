#' Heatmap loadings
#'
#' Creates a heatmap of the principal component loadings for a PCA analysis.
#'
#' @param load_mat a matrix of principal component loadings of dimensionality p x q (with p = number of variables and q = number of PCs)
#' @param absolute a logical value describing whether the absolute values of `load_mat` should be considered
#' @param var_range range of variables for which to plot the loadings
#' @param PCs_range range of PCs for which to plot the loadings
#' @return Description of function output
#' @author Edoardo Costantini, 2023
#' @examples
#' # Perform SVD of some data
#' svd_mtcars <- svd(scale(mtcars))
#'
#' # Crete an example loadings matrix
#' load_mat <- svd_mtcars$v
#'
#' # Give meaningful names to dimensions
#' colnames(load_mat) <- paste0("PC", 1:ncol(mtcars))
#' rownames(load_mat) <- colnames(mtcars)
#'
#' # use it
#' heatmap_loadings(load_mat)
#'
#' @export
heatmap_loadings <- function(load_mat, absolute = TRUE, var_range = 1:nrow(load_mat), PCs_range = 1:ncol(load_mat), panel_title = "Panel B", var_omit = NULL) {
    # Prepare differences if the absolute value is requested
    if (absolute == TRUE) {
        # Take the absolute value of the correlation matrix
        load_mat <- abs(load_mat)
    }

    # Subset correlation matrix
    load_mat_melt <- reshape2::melt(load_mat[, PCs_range])

    # Round values
    load_mat_melt$value <- round(load_mat_melt$value, 3)

    # Define variables to omit
    omit_vector <- var_range %in% var_omit

    # Drop levels in a way that can still be represented
    levels(load_mat_melt[, "Var1"])[omit_vector] <- "..."

    # Make heatmap
    hml <- load_mat_melt %>%
        dplyr::filter(
            Var1 != "..."
        ) %>%
        ggplot2::ggplot(
            ggplot2::aes(x = Var1, y = Var2, fill = value)
        ) +
        ggplot2::geom_tile(color = "white") +
        ggplot2::scale_fill_gradient2(
            low = "blue",
            mid = "white",
            high = "darkgray",
            midpoint = 0,
            limit = c(min(load_mat_melt$value), max(load_mat_melt$value)),
            space = "Lab",
            name = ""
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            plot.title = ggplot2::element_text(
                size = 10,
                face = "bold",
                hjust = 0.5
            ),
            axis.title.y = ggplot2::element_blank(),
            axis.title.x = ggplot2::element_blank(),
            legend.position = "bottom"
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

    # Add panel title if requested
    if (!is.null(panel_title)) {
        hml <- hml + ggplot2::ggtitle(panel_title)
    }

    # Return plot
    hml
}
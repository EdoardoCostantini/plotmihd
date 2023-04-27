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
heatmap_loadings <- function(load_mat, absolute = TRUE, var_range = 1:nrow(load_mat), PCs_range = 1:ncol(load_mat)) {
    # Prepare differences if the absolute value is requested
    if (absolute == TRUE) {
        # Take the absolute value of the correlation matrix
        load_mat <- abs(load_mat)
    }

    # Subset correlation matrix
    load_mat_melt <- reshape2::melt(load_mat[var_range, PCs_range])

    # Round values
    load_mat_melt$value <- round(load_mat_melt$value, 3)

    # Modify based on range of values requested
    if (identical(seq_along(var_range), var_range) == FALSE) {
        # Identify split
        end_first_part <- max(which(seq_along(var_range) %in% var_range))

        # Define new levels
        new_levels <- c(
            first_part = levels(load_mat_melt[, "Var1"])[1:end_first_part],
            ellipse = "...",
            second_part = levels(load_mat_melt[, "Var1"])[-c(1:end_first_part)]
        )

        # Add ellipsis as an empty level
        load_mat_melt[, "Var1"] <- factor(
            x = load_mat_melt[, "Var1"],
            levels = new_levels
        )
    } else {
        # Just make it a factor
        load_mat_melt[, "Var1"] <- factor(x = load_mat_melt[, "Var1"])
    }

    # Make heatmap
    ggplot2::ggplot(
        data = load_mat_melt,
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
        ggplot2::ggtitle("Panel B") +
        ggplot2::theme(
            plot.title = ggplot2::element_text(
                size = 10,
                face = "bold",
                hjust = 0.5
            ),
            axis.title.y = ggplot2::element_blank(),
            axis.title.x = ggplot2::element_blank(),
            legend.position = "bottom"
            # aspect.ratio = 1
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
}
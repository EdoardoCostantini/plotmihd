#' Heatmap correlation
#'
#' Creates a heatmap of correlation coefficients.
#'
#' @param cor_mat any squared matrix (usually a correlation matrix.)
#' @return A ggplot object containing the heatmap.
#' @author Edoardo Costantini, 2023
#'
#' @examples
#' # Crete an example correlation matrix
#' cor_mat <- cor(mtcars)
#'
#' # use it
#' heatmap_correlation(cor_mat)
#'
#' # Check if you want absolute values or not
#' cor_mat <- matrix(c(1, -.5, -.5, 1), ncol = 2, dimnames = list(c("V1", "V2"), c("V1", "V2")))
#'
#' # use it
#' heatmap_correlation(cor_mat)
#'
#' @export
heatmap_correlation <- function(cor_mat, absolute = TRUE, var_range = 1:ncol(cor_mat)) {
    # Prepare differences if absolute value is requested
    if (absolute == TRUE) {
        # Take the absolute value of the correlation matrix
        cor_mat <- abs(cor_mat)

        # Define 0 1 as the correlation range
        gradient_limits <- c(0, 1)
    } else {
        # Define -1 and 1 as the correlation range
        gradient_limits <- c(-1, 1)
    }

    # Subset correlation matrix
    cor_mat_melt <- reshape2::melt(cor_mat)

    # Add ellipsis as an empty level
    cor_mat_melt[, "Var1"] <- factor(
        x = cor_mat_melt[, "Var1"],
        levels = c(levels(cor_mat_melt[, "Var1"])[var_range], "...", levels(cor_mat_melt[, "Var1"])[-c(var_range)])
    )
    cor_mat_melt[, "Var2"] <- factor(
        x = cor_mat_melt[, "Var2"],
        levels = c(levels(cor_mat_melt[, "Var2"])[var_range], "...", levels(cor_mat_melt[, "Var2"])[-c(var_range)])
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
            limit = gradient_limits,
            space = "Lab",
            name = ""
        ) +
        ggplot2::theme_bw() +
        ggplot2::ggtitle("Panel A") +
        ggplot2::theme(
            plot.title = ggplot2::element_text(
                size = 10,
                face = "bold",
                hjust = 0.5
            ),
            axis.title.y = ggplot2::element_blank(),
            axis.title.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text(angle = 90),
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
}
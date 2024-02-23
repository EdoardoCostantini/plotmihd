#' Scatter plot of the CPVE for subsequent PCs
#'
#' Scatter plot of the cumulative proportion of variance explained (CPVE) by principal components (PCS)
#'
#' @param cpve vector of cumulative proportion of variance explained values
#' @details
#' This function does such and such.
#' @return A ggplot object containing the scatter plot.
#' @author Edoardo Costantini, 2023
#' @examples
#' # Perform SVD of some data
#' svd_mtcars <- svd(scale(mtcars))
#'
#' # Compute CPVE
#' cpve <- cumsum(prop.table(svd_mtcars$d^2))
#'
#' # use it
#' scatter_cpve(cpve)
#'
#' @export
scatter_cpve <- function(cpve, PCs_range = 1:length(cpve), panel_title = "Panel D") {
    # Give PC number as name
    names(cpve) <- 1:length(cpve)

    # Attach collinearity as column
    cpve_data <- data.frame(
        npcs = factor(1:length(cpve)),
        value = cpve
    )[PCs_range, ]

    # ggplot
    scpve <- cpve_data %>%
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
            plot.title = ggplot2::element_text(
                size = 10,
                face = "bold",
                hjust = 0.5
            ),
            axis.title.y = ggplot2::element_text(size = 8),
            axis.title.x = ggplot2::element_text(size = 8)
        )

    # Add panel title if requested
    if (!is.null(panel_title)) {
        scpve <- scpve + ggplot2::ggtitle(panel_title)
    }

    # Return plot
    scpve
}
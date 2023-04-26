#' Histogram Number of Principal Components
#'
#' Creates a histogram of number of components selected based on different decision rules.
#'
#' @param npcs_nscree data.frame of nScree results
#' @param npcs_50rule number of PCs kept by 50$ rule
#' @details
#' This function does such and such.
#' @return A ggplot object containing the histogram.
#' @author Edoardo Costantini, 2023
#' @examples
#' # Crete an example correlation matrix
#' npcs_nscree <- nFactors::nScree(as.data.frame(X_ma))$Components
#' npcs_50rule <- 5
#'
#' # use it
#' histogram_npcs(npcs_nscree, npcs_50rule)
#' 
#' @export
histogram_npcs <- function(npcs_nscree, npcs_50rule) {
    # Number of factors underlying data
    npcs_kept <- cbind(
        npcs_nscree,
        rule50 = npcs_50rule
    )

    # Plot npcs
    npcs_kept <- reshape2::melt(npcs_kept)

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
        ggplot2::ylab("Number of PCs") +
        ggplot2::theme_bw() +
        ggplot2::theme(
            axis.title.x = ggplot2::element_blank(),
            aspect.ratio = 1
        )
}
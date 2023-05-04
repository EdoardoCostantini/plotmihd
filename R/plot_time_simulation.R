#' Plot time for different data results
#'
#' @param res result object from the simulation study
#' @param dims number of variables used in the data generation procedure
#' @param meths names of imputation methods to plot
#' @param prop_NA vector of the correlation coefficients to plot
#' @param x_lims minimum and maxim values for the x axis values
#' @return ggplot object
#' @author Edoardo Costantini, 2023
#' @examples
#' # Example internals main simulation study
#' res <- res_exp_1_time
#' meths <- levels(res_exp_1_time$variable)[1:3]
#' dims <- 50
#' prop_NA <- .3
#' rho <- 0
#' x_lims <- c(0, 90)
#' 
#' # Example internals collinearity simulation study 
#' res <- res_exp_1_2_time
#' meths <- levels(res_exp_1_2_time$variable)[1:3]
#' dims <- 50
#' prop_NA <- .3
#' rho <- .6
#' x_lims <- c(0, 90)
#'
#' @export
plot_time_simulation <- function(res, meths, dims, prop_NA, rho, x_lims) {

    # Graphical parameters
    segme.thick <- 1 # thickness of lines reporting results (was 1)
    small.color <- "darkgray" # color of lines |PRB| < 10%
    large.color <- "lightgray" # color of lines |PRB| < 10%
    h.lines.thick <- .35 # thickness of lines separating methods (was .375)
    h.lines.color <- "gray" # color of lines separating methods (was gray)
    v.lines.thick <- .375 # thickness of reference lines
    v.lines.color <- "darkgray" # color of reference lines
    v.lines.type <- "dashed" # line type of reference lines

    # Plot limits
    x_lims <- c(0, 90)
    plot_breaks <- c(0, 30, 60, 90)
    plot_labels <- c("", "30", "60", "90")
    plot_vlines <- c(30, 60)
    plot_hlines <- NULL

    # Filter data
    res_filtered <- res %>%
        dplyr::filter(
            pm == prop_NA,
            p == dims,
            collinearity == rho,
            variable %in% meths
        )

    # Round results
    res_filtered$value <- round(res_filtered$value, 2)

    # Main Plot
    plot_main <- ggplot2::ggplot(
        res_filtered,
        ggplot2::aes(x = value, y = variable)
    ) +
        ggplot2::geom_bar(stat = "identity", fill = large.color) +
        ggplot2::geom_text(
            ggplot2::aes(
                label = value
            ),
            hjust = -.2,
            size = 2
        )

    plot_themed <- plot_main + ggplot2::labs(
        # Cosmetic
        title = ggplot2::element_blank(),
        x = ggplot2::element_blank(),
        y = ggplot2::element_blank()
    ) +
        # X Axis
        ggplot2::scale_x_continuous(
            breaks = plot_breaks,
            labels = plot_labels
        ) +
        ggplot2::geom_vline(
            xintercept = plot_vlines,
            size = v.lines.thick,
            linetype = v.lines.type,
            color = v.lines.color
        ) +
        ggplot2::coord_cartesian(xlim = x_lims) +
        # Horizontal lines
        ggplot2::geom_hline(
            yintercept = plot_hlines,
            size = h.lines.thick,
            color = h.lines.color
        ) +
        # Theme, title, and axis
        ggplot2::labs(
            x = "Imputation time in minutes"
        ) + 
        ggplot2::theme(
            plot.title = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank(),
            # Background
            panel.background = ggplot2::element_rect(fill = "white", colour = "white"),
            axis.ticks = ggplot2::element_blank(),
            panel.border = ggplot2::element_rect(
                fill = NA,
                color = "gray", size = 0.35
            ),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            # Legend
            legend.key = ggplot2::element_rect(colour = "gray", fill = NA, size = .15),
            legend.text = ggplot2::element_text(size = 8),
            legend.position = "bottom"
        )

    plot_themed

}
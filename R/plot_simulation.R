#' Plot results for the simulation study
#'
#' @param res result object from the simulation study
#' @param dims number of variables used in the data generation procedure
#' @param outcome type of performance measure to be plotted
#' @param meths names of imputation methods to plot
#' @param prop_NA vector of the correlation coefficients to plot
#' @param rho vector of the correlation coefficients to plot
#' @param reps number of repetitions used in the simulation
#' @param x_lims minimum and maxim values for the x axis values
#' @return ggplot object
#' @author Edoardo Costantini, 2023
#' @examples
#' # Example internals main simulation study
#' data(res_exp_1)
#' res <- res_exp_1
#' dims <- 50
#' outcome <- c("PRB", "CIC", "CIW")[1]
#' meths <- levels(res_exp_1$methods)[1:11]
#' prop_NA <- c(0.1, .3)
#' rho <- unique(res_exp_1$collinearity)
#' 
#' # Example use
#' plot_simulation(
#'     res = res_exp_1,
#'     dims = dims,
#'     outcome = outcome,
#'     meths = meths,
#'     x_lims = x_lims,
#'     rho = unique(res_exp_1$collinearity)
#'     prop_NA = prop_NA
#' )
#' 
#' # Example internals collinearity study
#' data(res_exp_1_2)
#' res <- res_exp_1_2
#' prop_NA <- unique(res_exp_1_2$pm)
#' rho <- c(0, .9)
#' x_lims <- c(1, 50)
#'
#' # Example use
#' plot_simulation(
#'     res = res_exp_1_2,
#'     dims = dims,
#'     outcome = outcome,
#'     meths = meths,
#'     x_lims = x_lims,
#'     rho = rho
#'     prop_NA = unique(res_exp_1_2$pm)
#' )
#'
#' @export
plot_simulation <- function(res, dims, outcome, meths, rho = NULL, prop_NA = NULL, x_lims, reps) {
    # Condition Names / Labels
    label_cond <- unique(res$cond)
    label_parm <- unique(res$parm)

    # Filter data (common to both studies)
    res_filtered <- res %>%
        dplyr::filter(
            analysis == outcome,
            variable %in% c("Min", "Mean", "Max"),
            p == dims,
            pm %in% prop_NA,
            collinearity %in% rho,
            methods %in% meths
        )

    # Define plots main objects of interest
    if (outcome == "PRB") {
        # Threshold lines
        reference_line <- 10
        reference_linewidth <- 0.15
        x_breaks <- c(0, 10, 20, 50)
    }
    if (outcome == "CIC") {
        # SE for threshold
        ci_lvl <- .95
        reference_line <- ci_lvl * 100
        reference_linewidth <- 0.15
        SEp <- sqrt(ci_lvl * (1 - ci_lvl) / reps)
        low_thr <- (.95 - SEp * 2) * 100
        hig_thr <- (.95 + SEp * 2) * 100
        x_breaks <- c(0, 50, 80, 90, round(low_thr, 0), 95, round(hig_thr, 0), 100)
    }
    if (outcome == "CIW") {
        reference_line <- 0
        reference_linewidth <- 0
        x_breaks <- 0:5
    }

    # Main plot
    plot_main <- res_filtered %>%
        # Main Plot
        ggplot2::ggplot(
            data = .,
            ggplot2::aes(
                y = methods,
                x = value,
                shape = variable
            )
        ) +
        ggplot2::geom_point(size = 1.75) +
        ggplot2::geom_line(
            ggplot2::aes(group = methods),
            linewidth = .25
        )

    # Grid
    plot_grid <- plot_main + ggplot2::facet_grid(
        rows = ggplot2::vars(
            factor(parm,
                levels = unique(parm)
            )
        ),
        cols = ggplot2::vars(cond)
    )

    # References
    plot_refs <- plot_grid + ggplot2::geom_vline(
        ggplot2::aes(xintercept = reference_line),
        linetype = "solid",
        linewidth = reference_linewidth,
    )

    # Format
    plot_format <- plot_refs +
        ggplot2::scale_x_continuous(
            labels = x_breaks,
            breaks = x_breaks
        ) +
        ggplot2::scale_y_discrete(limits = rev) +
        ggplot2::scale_shape_manual(values = c("I", "I", "I")) +
        ggplot2::coord_cartesian(xlim = x_lims) +
        ggplot2::labs(
            x = NULL,
            y = NULL,
            linetype = NULL,
            shape = NULL
        ) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(
                angle = 90,
                vjust = 0.5,
                hjust = 1
            ),
            panel.background = ggplot2::element_rect(
                fill = NA,
                color = "gray"
            ),
            panel.grid.major = ggplot2::element_line(
                color = "gray",
                linewidth = 0.15,
                linetype = 1
            ),
            legend.key = ggplot2::element_rect(
                colour = "gray",
                fill = NA,
                linewidth = .15
            ),
            axis.ticks = ggplot2::element_blank(),
            legend.position = "none"
        )

    # Return plot
    plot_format
}
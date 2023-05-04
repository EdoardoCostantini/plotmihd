#' Plot resampling study
#'
#' @param argument_1 A description of the first argument
#' @param argument_2 A description of the second argument
#' @details
#' This function does such and such.
#' @return Description of function output
#' @author Edoardo Costantini, 2023
#' @examples
#' # Example input
#' res <- res_exp_4
#' model <- c("m1", "m2")[1]
#' type <- "bias"
#' dt_reps <- 500
#' ci_lvl <- .95
#' meth_compare <- c("DURR_la", "IURR_la", "blasso", "bridge", "MI_PCA", "MI_CART", "MI_RF", "stepFor", "CC")
#' @references
#'
#' Such, S. (2006). Such and such. Journal such and such, 101(473), 119-137.
#'
#' @export
# Custom plot_gg function for experiment 4
plot_resampling <- function(res,
                            outcome = c("bias_per", "ci_cov", "CIW")[1],
                            model = c("m1", "m2")[1],
                            type = "bias",
                            dt_reps = 500,
                            ci_lvl = .95,
                            meth_compare) {
    # Extract the result type you desire
    dt_outcome <- lapply(
        1:nrow(res$conds),
        function(x) res[[model]][[x]][[outcome]]
    )

    # Take absolute value:
    dt_absolute <- lapply(
        dt_outcome,
        function(d) abs(d[, meth_compare])
    )

    # Define condition names
    conds <- rep(c(
        "low-dim \n p = 243, n = 1000",
        "high-dim \n p = 243, n = 300"
    ), 2)

    # Change methods names
    dt_edit <- lapply(
        1:nrow(res$conds),
        function(id) {
            # Extract result
            x <- dt_absolute[[id]]

            # Adapt method names
            colnames(x) <- sub("_la", "", colnames(x))
            colnames(x) <- sub("_", "-", colnames(x))
            colnames(x) <- sub("MI-qp", "MI-QP", colnames(x))
            colnames(x) <- sub("MI-am", "MI-AM", colnames(x))
            colnames(x) <- sub("MI-OP", "MI-OR", colnames(x))
            colnames(x) <- sub("stepFor", "MI-SF", colnames(x))

            # Extract Methods Name
            methods <- names(x)

            # Extract Results for a method
            output_2 <- lapply(1:ncol(x), function(l) {
                par_names <- rownames(x[l])[order(x[[l]], decreasing = TRUE)]
                par_value <- x[l][order(x[[l]], decreasing = TRUE), ]

                # Compose output for method
                output_1 <- data.frame(
                    key = methods[l], # method
                    par = c(par_names, ""),
                    value = c(par_value, 0),
                    conds = factor(conds[id])
                )
                return(output_1)
            })

            return(output_2)
        }
    )

    # Put them in groups by condition
    dt_edit <- lapply(
        dt_edit,
        function(x) {
            do.call(rbind, x)
        }
    )

    dt_edit <- lapply(dt_edit, function(x) {
        x$id <- 1:nrow(x)
        return(x)
    })

    # Combine for facet in one list
    dt_edit <- do.call(rbind, dt_edit)

    # Change Methods labels
    dt_edit$key <- str_replace(dt_edit$key, "blasso", "BLasso")
    dt_edit$key <- str_replace(dt_edit$key, "bridge", "BRidge")

    # Define Step Size for all parameters sets
    step_size <- (
        nrow(dt_absolute[[1]]) + # number of parameters
            1 # account for additional empty row
    ) / 2 # place label in the middle
    plot_steps <- seq(0,
        (nrow(dt_absolute[[1]]) + 1) * # number of parameters + empty row
            ncol(dt_absolute[[1]]), # number of methods
        by = step_size
    )
    plot_ybreaks <- plot_steps[c(FALSE, TRUE)] # position label skip
    plot_hlines <- plot_steps[c(TRUE, FALSE)]

    # Methods labels
    plot_ylabels <- as.character(unique(dt_edit$key)) # unique for everyone

    # Parameter Labels
    # Ticks
    if (type == "bias") {
        # Levels order
        levs <- c(no = "<10%", yes = ">10%")

        # Grid Plot Color based on exceeding or not PRB reference
        flag <- ifelse(dt_edit$value >= 10, yes = levs[2], no = levs[1])
        # flag[dt_edit$par %in% small.ef] <- "Largest Bias"
        dt_edit$flag <- factor(flag, levels = levs)

        # Plot Limits
        plot_xlim <- c(0, 70)

        # X axis
        plot_xbreaks <- seq(min(plot_xlim), max(plot_xlim), by = 10)
        plot_xlabels <- as.character(plot_xbreaks)
        plot_vlines <- 10
    }

    if (type == "ci") {
        # Redefine values as differences from target
        dt_edit$value[dt_edit$value != 0] <- dt_edit$value[dt_edit$value != 0] - 95

        # Plot Limits (reference: 0 = .95)
        plot_xlim <- c(-5, 5)

        # SE for threshold
        SEp <- sqrt(ci_lvl * (1 - ci_lvl) / dt_reps)
        low_thr <- ((.95 - SEp * 2) - .95) * 100
        hig_thr <- ((.95 + SEp * 2) - .95) * 100

        # Levels order
        levs <- c(
            no = "Not significant",
            yes = "Significant"
        )

        # Grid Plot Color based on exceeding or not PRB reference
        flag <- ifelse(dt_edit$value >= hig_thr | dt_edit$value <= low_thr,
            yes = levs[2], no = levs[1]
        )
        dt_edit$flag <- factor(flag, levels = levs)

        # X axis
        plot_xbreaks <- c(-5, low_thr, 0, hig_thr, 4, 5)
        plot_xlabels <- as.character(round((plot_xbreaks + 95) / 100, 2))
        plot_vlines <- c(-5, low_thr, hig_thr, 4)
    }

    if (type == "CIW") {
        # Plot Limits
        plot_xlim <- c(0, 10)

        # Color bras that are above x limit differently
        levs <- c(no = "<10%", yes = ">10%")

        # Grid Plot Color based on exceeding or not PRB reference
        flag <- ifelse(dt_edit$value >= 10, yes = levs[2], no = levs[1])
        # flag[dt_edit$par %in% small.ef] <- "Largest Bias"
        dt_edit$flag <- factor(flag, levels = levs)

        # X axis
        plot_xbreaks <- seq(min(plot_xlim), max(plot_xlim), by = 1)
        plot_xlabels <- as.character(plot_xbreaks)
        plot_vlines <- NULL
    }

    # Colors and texts
    segme.thick <- 1 # thickness of lines reporting results (was 1)
    small.color <- "darkgray" # color of lines |PRB| < 10%
    large.color <- "lightgray" # color of lines |PRB| < 10%
    h.lines.thick <- .35 # thickness of lines separating methods (was .375)
    h.lines.color <- "gray" # color of lines separating methods (was gray)
    v.lines.thick <- .375 # thickness of reference lines
    v.lines.color <- "darkgray" # color of reference lines
    v.lines.type <- "dashed" # line type of reference lines

    # Base plot
    p <- ggplot2::ggplot(dt_edit, ggplot2::aes(x = value, y = id))

    # Add colored segments
    if (type == "CIW") {
        p <- p + ggplot2::geom_segment(
            ggplot2::aes(
                xend = 0,
                yend = id
            ),
            colour = large.color,
            linewidth = segme.thick
        )
    } else {
        p <- p + ggplot2::geom_segment(
            ggplot2::aes(
                xend = 0,
                yend = id,
                colour = flag
            ),
            linewidth = segme.thick
        ) +
            ggplot2::scale_color_manual(
                name = "",
                values = c(
                    small.color,
                    large.color
                )
            )
    }

    # X Axis
    p <- p +
        ggplot2::scale_x_continuous(
            breaks = plot_xbreaks,
            labels = plot_xlabels
        ) +
        ggplot2::geom_vline(
            xintercept = plot_vlines,
            size = v.lines.thick,
            linetype = v.lines.type,
            color = v.lines.color
        ) +
        ggplot2::coord_cartesian(
            xlim = plot_xlim,
            ylim = c(min(dt_edit$id), max(dt_edit$id))
        ) +

        # Y axis
        ggplot2::scale_y_continuous(
            breaks = plot_ybreaks,
            labels = plot_ylabels, expand = c(.010, .010)
        ) +
        ggplot2::geom_hline(
            yintercept = plot_hlines,
            size = h.lines.thick,
            color = h.lines.color
        )

    # Facet
    p <- p +
        ggplot2::facet_grid(cols = ggplot2::vars(conds))

    # Title and axis labels
    p <- p +
        ggplot2::theme(
            plot.title = ggplot2::element_blank(),
            axis.title.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank(),
            plot.margin = ggplot2::unit(c(0, 0.1, 0, 0.05), "cm"),
            # Background
            panel.background = ggplot2::element_rect(fill = "white", colour = "white"),
            axis.ticks = ggplot2::element_blank(),
            panel.border = ggplot2::element_rect(
                fill = NA,
                color = "gray", size = 0.35
            ),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            # Condition + Parameter type (Facet Related)
            strip.text = ggplot2::element_text(
                margin = ggplot2::unit(c(.10, .10, .10, .10), "cm")
            ),
            # Legend
            legend.key = ggplot2::element_rect(colour = "gray", fill = NA, size = .15),
            legend.position = "bottom"
        )
    p
}

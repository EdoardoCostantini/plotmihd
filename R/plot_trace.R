#' Trace plots
#'
#' Generate trace plots to study convergence of imputation methods
#'
#' @param mids_data object containing imputation history produced by the simulation study
#' @param method unit character vector naming the method to display
#' @param var unit character vector naming the variable to display
#' @param iters numeric vector containing the iteration bounds (from-to) to plot
#' @param rp numeric vector of length 1 indexing the repetitions
#' @return Returns the lattice plot
#' @author Edoardo Costantini, 2023
#' @examples
#' # Define example inputs
#' mids_data <- res_exp_1_mids
#' method <- "IURR"
#' layout <- c(2, 6)
#' iters <- c(0, 25)
#' rp <- 1
#'
#' # Use the function
#' plot_trace(
#'     mids_data = res_exp_1_mids,
#'     method = "MI-PCA",
#'     layout <- c(2, 6),
#'     iters = c(0, 25)
#' )
#'
#' @export
plot_trace <- function(mids_data, method, iters = c(0, 25), layout = c(2, 1), rp = 1) {

    # Extract the mids object of interest
    x <- mids_data[[rp]][[method]]

    # Default arguments that you could change in MICE
    type <- "l"
    col <- 1:10
    lty <- 1
    theme <- mice::mice.theme()

    # Extract objects I need
    mn <- x$chainMean
    sm <- sqrt(x$chainVar)
    m <- x$m
    it <- x$iteration

    # select subset of non-missing entries
    obs <- apply(!(is.nan(mn) | is.na(mn)), 1, all)
    varlist <- names(obs)[obs]

    # Prepare objects for plotting
    mn <- matrix(aperm(mn[varlist, , , drop = FALSE], c(2, 3, 1)), nrow = m * it)
    sm <- matrix(aperm(sm[varlist, , , drop = FALSE], c(2, 3, 1)), nrow = m * it)
    adm <- expand.grid(seq_len(it), seq_len(m), c("mean", "sd"))
    data <- cbind(adm, rbind(mn, sm))
    colnames(data) <- c(".it", ".m", ".ms", varlist)

    # Create formula
    formula <- as.formula(paste0(
        paste0(varlist, collapse = "+"),
        "~.it|.ms"
    ))

    # Dummy to trick R CMD check
    .m <- NULL
    rm(.m)

    # Load function to obtain the correct plot arrangement
    strip.combined <- function(which.given, which.panel, factor.levels, ...) {
        if (which.given == 1) {
            lattice::panel.rect(0, 0, 1, 1,
                col = theme$strip.background$col, border = 1
            )
            lattice::panel.text(
                x = 0, y = 0.5, pos = 4,
                lab = factor.levels[which.panel[which.given]]
            )
        }
        if (which.given == 2) {
            lattice::panel.text(
                x = 1, y = 0.5, pos = 2,
                lab = factor.levels[which.panel[which.given]]
            )
        }
    }

    # Make plot
    lattice::xyplot(
        x = formula,
        data = data,
        groups = .m,
        type = type,
        lty = lty,
        col = col,
        layout = layout,
        scales = list(
            y = list(relation = "free"),
            x = list(alternating = FALSE)
        ),
        as.table = TRUE,
        xlim = c(iters[1] - 1, iters[2] + 1),
        xlab = "Iteration",
        ylab = "",
        strip = strip.combined,
        par.strip.text = list(lines = 0.5)
    )
}

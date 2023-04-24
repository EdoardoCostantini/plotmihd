#' Function full name
#'
#' A high level description of the function.
#'
#' @param n A description of the first argument
#' @param p A description of the second argument
#' @details
#' This function does such and such.
#' @return Description of function output
#' @author Edoardo Costantini, 2023
#' @references
#'
#' Costantini, E., Lang, K. M., Reeskens, T., & Sijtsma, K. (2022). High-dimensional imputation for the social sciences: a comparison of state-of-the-art methods. arXiv preprint arXiv:2208.13656.
#'
#' @examples 
#' 
#' # Example internals
#' n <- 1e3
#' block_sizes <- c(5, 5, 10)
#' block_rhos <- c(.6, .3, .01)
#' inflated_rhos <- list(c(4, 5), c(9, 10), c(11:p))
#' inflated_values <- .9
#' 
#' # Specification that reproduces results from the simulation study in Costantini et al 2022
#' 
#' gen_data(
#'     n = 200,
#'     block_sizes = c(5, 5, 50),
#'     block_rhos = c(.6, .3, .01),
#'     inflated_rhos = list(c(4, 5), c(9, 10), c(11:p)),
#'     inflated_values = ''.9
#' )
#' 
#' @export
gen_data <- function(n = 1e3, block_sizes = c(5, 5, 10), block_rhos = c(.6, .3, .01), inflated_rhos = NULL, inflated_values = NULL) {
    # For internals
    
    # Check block / rho sizes
    if(length(block_sizes) != length(block_rhos)){
        stop("The number of correlation coefficients provided in `block_rhos` must be equal to the number of block sizes.")
    }

    # count the number of variables
    p <- sum(block_sizes)

    # count the number of blocks
    b <- length(block_sizes)

    # Generate covariance matrix
    Sigma <- diag(p)

    # Generate block index
    block_index <- lapply(1:b, function(i){
        paste0("b", i, "v", 1:block_sizes[i])
    })

    # Make variable names out of it
    dimnames(Sigma) <- list(unlist(block_index), unlist(block_index))

    # Assign correlations to b
    for(i in 1:b){
        # variables index
        Sigma[block_index[[i]], ] <- block_rhos[i]
    }

    # Inflate correlations
    if(length(inflated_rhos) != 0){
        for(set in inflated_rhos){
            Sigma[set, set] <- inflated_values
        }
    }
    
    # Fix diagonal
    diag(Sigma) <- 1

    # Make symmetric
    Sigma[upper.tri(Sigma)] <- t(Sigma)[upper.tri(Sigma)]

    # Sample data from MVN
    Z <- mvtnorm::rmvnorm(
        n = n,
        mean = rep(0, p),
        sigma = Sigma
    )

    # Give meaningful names
    colnames(Z) <- paste0("v", 1:ncol(Z))

    return(as.data.frame(Z))
}
#' A grand tour path
#'
#' This method generates target bases by randomly sampling on
#' the space of all d-dimensional planes in p-space.
#'
#' @param target dimensionality
#'
#' @examples
#' # All animation methods use the grand tour path by default
#' animate_dist(flea[, 1:6])
#' animate_xy(flea[, 1:6])
grand_tour <- function(d = 2) {
  generator <- function(current, data) {
    if (is.null(current)) return(basis_init(ncol(data), d))

    basis_random(ncol(data), d)      
  }

  new_tour_path("grand", generator) 
}

#' Generate a random basis
#'
#' @keywords internal
#' @param n dimensionality of data
#' @param d dimensionality of target projection
basis_random <- function(n, d = 2) {  
  mvn <- matrix(rnorm(n * d), ncol = d)
  orthonormalise(mvn)
}

basis_init <- function(n, d) {
  start <- matrix(0, nrow = n, ncol = d)
  diag(start) <- 1    
  start
}

#' A grand tour path
#'
#' This method generates target bases by randomly sampling on
#' the space of all d-dimensional planes in p-space.
#'
#' @param starting projection matrix
#' @param other arguments not used by this tour path method
#'
#' @examples
#' # All animation methods use the grand tour path by default
#' animate_dist(flea[, 1:6])
#' animate_xy(flea[, 1:6])
grand_tour <- function(current, ...) {
  new_target <- function(current) {
    basis_random(nrow(current), ncol(current))
  }

  tour(current, new_target)
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
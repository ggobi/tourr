#' A grand tour path.
#'
#' This method generates target bases by randomly sampling on
#' the space of all d-dimensional planes in p-space.
#'
#' Usually, you will not call this function directly, but will pass it to
#' a method that works with tour paths like \code{\link{animate}},
#' \code{\link{save_history}} or \code{\link{render}}.
#'
#' @param d target dimensionality
#' @param ... arguments sent to the generator
#' @export
#' @examples
#' # All animation methods use the grand tour path by default
#' animate_dist(flea[, 1:6])
#' animate_xy(flea[, 1:6])
#' animate_pcp(flea[, 1:6])
#' animate_pcp(flea[, 1:6], grand_tour(4))
#'
#' # The grand tour is a function:
#' tour2d <- grand_tour(2)
#' is.function(tour2d)
#'
#' # with two parameters, the previous projection and the data set
#' args(tour2d)
#' # if the previous projection is null, it will generate a starting
#' # basis, otherwise the argument is ignored
#' tour2d(NULL, mtcars)
#' # the data argument is just used to determine the correct dimensionality
#' # of the output matrix
#' tour2d(NULL, mtcars[, 1:2])
grand_tour <- function(d = 2, ...) {
  generator <- function(current, data, ...) {
    if (is.null(current)) {
      return(basis_init(ncol(data), d))
    }

    target <- basis_random(ncol(data), d)
    list(target = target)
  }

  new_geodesic_path("grand", generator)
}

#' Generate a random basis
#'
#' @keywords internal
#' @param n dimensionality of data
#' @param d dimensionality of target projection
#' @export
basis_random <- function(n, d = 2) {
  mvn <- matrix(stats::rnorm(n * d), ncol = d)
  orthonormalise(mvn)
}

#' Generate initial basis.
#'
#' First two variables are projected on first two axes.
#'
#' @keywords internal
#' @param n dimensionality of data
#' @param d dimensionality of target projection
#' @export
basis_init <- function(n, d) {
  start <- matrix(0, nrow = n, ncol = d)
  diag(start) <- 1
  start
}

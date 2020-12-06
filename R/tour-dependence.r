#' A dependence tour path.
#'
#' The dependence tour combines a set of independent 1d tours to produce
#' a nd tour.  For the special case of 2d, this is known as a correlation
#' tour.  This tour corresponds to the multivariate method known as
#' generalised canonical correlation, and is used to investigate dependence
#' between groups of variables.
#'
#' Usually, you will not call this function directly, but will pass it to
#' a method that works with tour paths like \code{\link{animate}},
#' \code{\link{save_history}} or \code{\link{render}}.
#'
#' @param pos a numeric vector describing which variables are mapped to
#'   which dimensions: 1 corresponds to first, 2 to second etc.
#' @export
#' @examples
#' animate_xy(flea[, 1:3], dependence_tour(c(1, 2, 2)))
#' animate_xy(flea[, 1:4], dependence_tour(c(1, 2, 1, 2)))
#' animate_pcp(flea[, 1:6], dependence_tour(c(1, 2, 3, 2, 1, 3)))
dependence_tour <- function(pos) {
  stopifnot(is.numeric(pos))
  stopifnot(all.equal(pos, trunc(pos)))

  d <- max(pos)
  generator <- function(current, data, ...) {
    if (is.null(current)) {
      return(basis_init(ncol(data), d))
    }

    mat <- matrix(0, ncol = d, nrow = length(pos))
    for (i in seq_len(d)) {
      mat[pos == i, i] <- basis_random(sum(pos == i), 1)
    }

    target <- mat
    list(target = target)
  }

  new_geodesic_path("independent", generator)
}

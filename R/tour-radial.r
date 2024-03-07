#' A radial tour path.
#'
#' The radial tour rotates a chosen variable axis out of the current
#' projection.
#'
#' Usually, you will not call this function directly, but will pass it to
#' a method that works with tour paths like \code{\link{animate}},
#' \code{\link{save_history}} or \code{\link{render}}.
#'
#' @param start initial projection matrix
#' @param mvar variable(s) chosen to rotate out
#' @param ... additional arguments for drawing
#' @export
#' @examples
#' animate_xy(flea[, 1:6], radial_tour(basis_random(6, 2), mvar = 4), rescale=TRUE)
#' animate_xy(flea[, 1:6], radial_tour(basis_random(6, 2), mvar = c(3,4)), rescale=TRUE)
#' animate_dist(flea[, 1:6], radial_tour(basis_random(6, 1), mvar = 4), rescale=TRUE)
#' animate_scatmat(flea[, 1:6], radial_tour(basis_random(6, 3), mvar = 4), rescale=TRUE)
radial_tour <- function(start, mvar = 1, ...) {
  first <- TRUE
  out <- TRUE

  generator <- function(current, data, ...) {
    if (first) {
      new_basis <- start
      first <<- FALSE
      return(new_basis)
    }
    if (out) {
      new <- start
      new[mvar,] <- rep(0, ncol(start))
      new <- orthonormalise(new)
      new_basis <- new
      out <<- !out
    }
    else {
      new_basis <- start
      out <<- !out
    }

    target <- new_basis
    list(target = target)
  }

  new_geodesic_path("radial", generator)
}

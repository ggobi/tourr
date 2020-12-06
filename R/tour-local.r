#' A local tour path.
#'
#' The local tour alternates between the starting position and a
#' nearby random projection.
#'
#' Usually, you will not call this function directly, but will pass it to
#' a method that works with tour paths like \code{\link{animate}},
#' \code{\link{save_history}} or \code{\link{render}}.
#'
#' @param start initial projection matrix
#' @param angle distance in radians to stay within
#' @export
#' @examples
#' animate_xy(flea[, 1:3], local_tour(basis_init(3, 2)))
#' animate_xy(flea[, 1:3], local_tour(basis_init(3, 2), 0.2))
#' animate_xy(flea[, 1:3], local_tour(basis_random(3, 2), 0.2))
local_tour <- function(start, angle = pi / 4) {
  odd <- TRUE

  generator <- function(current, data, ...) {
    if (odd) {
      new_basis <- start
      odd <<- !odd
      return(new_basis)
    } else {
      new <- basis_random(nrow(start), ncol(start))
      dist <- stats::runif(1, 0, angle)
      new_basis <- step_angle(geodesic_info(start, new), dist)
    }


    target <- new_basis
    list(target = target)
  }

  new_geodesic_path("local", generator)
}

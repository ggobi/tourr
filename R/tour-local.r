#' The local tour
#' 
#' The local tour never strays far from where it started.
#' 
#' Usually, you will not call this function directly, but will pass it to 
#' a method that works with tour paths like \code{\link{save_history}}, 
#' or \code{\link{animate}}
#' 
#' @param start initial projection matrix
#' @param angle distance in radians to stay within
#' @examples
#' animate_xy(flea[, 1:3], local_tour(basis_init(3, 2)))
#' animate_xy(flea[, 1:3], local_tour(basis_init(3, 2), 0.2))
#' animate_xy(flea[, 1:3], local_tour(basis_random(3, 2), 0.2))
local_tour <- function(start, angle = pi / 4) {
  generator <- function(current, data) {
    if (is.null(current)) return(start)
    
    new <- basis_random(nrow(current), ncol(current))
    step_angle(geodesic_info(start, new), angle)
  }

  new_tour_path("local", generator) 
}
#' The independent tour.
#' 
#' The independent tour combines a set of independent 1d tours to produce
#' a nd tour.  For the special case of 2d, this is known as a correlation 
#' tour
#'
#' Usually, you will not call this function directly, but will pass it to 
#' a method that works with tour paths like \code{\link{save_history}}, 
#' or \code{\link{animate}}
#' 
#' @param pos a numeric vector describing which variables are mapped to
#'   which dimensions: 1 corresponds to first, 2 to second etc.
#' @examples
#' animate_xy(flea[, 1:3], independent_tour(c(1, 2, 2)))
#' animate_xy(flea[, 1:4], independent_tour(c(1, 2, 1, 2)))
#' animate_pcp(flea[, 1:6], independent_tour(c(1, 2, 3, 2, 1, 3)))
independent_tour <- function(pos) {
  stopifnot(is.numeric(pos))
  stopifnot(all.equal(pos, trunc(pos)))

  d <- max(pos)
  generator <- function(current, data) {
    if (is.null(current)) return(basis_init(ncol(data), d))
    
    mat <- matrix(0, ncol = d, nrow = length(pos))
    for(i in seq_len(d)) {
      mat[pos == i, i] <- basis_random(sum(pos == i), 1)
    }
    
    mat
  }

  new_tour_path("independent", generator)
}

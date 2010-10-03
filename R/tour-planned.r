#' A planned tour path.
#' 
#' The planned tour takes you from one basis to the next in a 
#' set order.  Once you have visited all the planned bases, you either stop
#' or start from the beginning once more (if \code{cycle = TRUE}). 
#' 
#' Usually, you will not call this function directly, but will pass it to 
#' a method that works with tour paths like \code{\link{animate}}, 
#' \code{\link{save_history}} or \code{\link{render}}.
#' 
#' @param basis_set the set of bases as a list of projection matrices
#'   or a 3d array
#' @param cycle cycle through continuously (\code{TRUE}) or stop after 
#'   first pass (\code{FALSE})
#' @keywords hplot dynamic
#' @seealso The \code{\link{little_tour}}, a special type of planned tour
#'   which cycles between all axis parallel projections.  
#' @export
#' @examples
#' twod <- save_history(flea[, 1:3], max = 5)
#' str(twod)
#' animate_xy(flea[, 1:3], planned_tour(twod))
#' animate_xy(flea[, 1:3], planned_tour(twod, TRUE))
#'
#' oned <- save_history(flea[, 1:6], grand_tour(1), max = 3)
#' animate_dist(flea[, 1:6], planned_tour(oned))
planned_tour <- function(basis_set, cycle = FALSE) {
  index <- 0
  basis_set <- as.list(basis_set)
  
  n <- length(basis_set)
  if (cycle) {
    generator <- function(current, data) {
      index <<- (index %% n) + 1
      basis_set[[index]]
    }        
  } else {
    generator <- function(current, data) {
      index <<- index + 1
      if (index > n) return(NULL)
      basis_set[[index]]
    }        
  }
        
  new_geodesic_path("planned", generator)
}

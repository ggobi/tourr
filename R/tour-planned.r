#' The planned tour
#' 
#' The planned tour takes you from one basis to the next in a 
#' set order.  Once you have visited all the planned bases, you start from 
#' the beginning once more. 
#' 
#' Usually, you will not call this function directly, but will pass it to 
#' a method that works with tour paths like \code{\link{save_history}}, 
#' \code{\link{animate}}
#' 
#' @TODO add cycle argument
#' @param current the starting projection
#' @param basis_set the set of bases as a list of projection matrices
#'   or a 3d array
#' @param ... other arguments passed on to \code{\link{tour}}
#' @keywords hplot, dynamic
#' @seealso The \code{\link{little_tour}}, a special type of planned tour
#'   which cycles between all axis parallel projections
#' @seealso \code{save_history} for saving the output of another tour path
#'   to replay later with the planned tour
#' @examples
#' twod <- save_history(flea[, 1:6], nbases = 3)
#' str(twod)
#' animate_xy(flea[, 1:6], planned_tour(twod))
#'
#' oned <- save_history(flea[, 1:6], grand_tour(1), nbases = 3)
#' animate_dist(flea[, 1:6], planned_tour(oned))
planned_tour <- function(basis_set) {
  index <- 0
  
  if (is.list(basis_set)) {
    n <- length(basis_set)
    generator <- function(current, data) {
      index <<- (index %% n) + 1
      basis_set[[index]]
    }    
  } else {
    n <- dim(basis_set)[3]
    generator <- function(current, data) {
      index <<- (index %% n) + 1
      basis <- basis_set[, , index, drop = FALSE]
      dim(basis) <- dim(basis)[1:2]
      basis
    }
  }
        
  new_tour_path("planned", generator)
}



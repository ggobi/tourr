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
#' t1 <- save_history(flea[, 1:6], nbases = 3)
#' str(t1)
#' animate_xy(flea[, 1:6], planned_tour, basis_set = t1)
planned_tour <- function(current, basis_set, ...) {
  index <- 0
  
  if (is.list(basis_set)) {
    n <- length(basis_set)
    new_target <- function(current) {
      index <<- (index %% n) + 1
      basis_set[[index]]
    }    
  } else {
    n <- dim(basis_set)[3]
    new_target <- function(current) {
      index <<- (index %% n) + 1
      basis_set[, , index, drop = FALSE]
    }
  }
        
  tour(current, new_target, ...)
}


#' The little tour
#'
#' The little tour is a planned tour with travels between all axis parallel
#' projections.
#'
#' @param current the starting projection
#' @param ... other arguments passed on to \code{\link{tour}}
#' @examples
#' animate_xy(flea[, 1:6], little_tour)
#' animate_pcp(flea[, 1:6], little_tour, d = 3)
#' animate_pcp(flea[, 1:6], little_tour, d = 4)
little_tour <- function(current, ...) {
  little <- bases_little(nrow(current), ncol(current))
  planned_tour(current, little, ...)
}

#' Generate bases for the little tour
#'
#' @keywords internal
#' @param n dimensionality of data
#' @param d dimensionality of target projection
bases_little <- function(p, d = 2) {
  b <- diag(rep(1, p))
  vars <- combn(p, d)
  lapply(seq_len(ncol(vars)), function(i) b[, vars[, i]] )
}

#' Interpolate geodesically between bases.
#' 
#' This function takes a set of bases and produces a tour by geodesically 
#' interpolating between each basis
#' 
#' @param basis_set input basis set
#' @param angle target distance (in radians) between bases
#' @keywords hplot
#' @export
#' @examples
#' t1 <- save_history(flea[, 1:6], grand_tour(1), max = 10)
#' dim(t1)
#' dim(interpolate(t1, 0.01))
#' dim(interpolate(t1, 0.05))
#' dim(interpolate(t1, 0.1))
interpolate <- function(basis_set, angle = 0.05) {
  tour <- new_tour(data, planned_tour(basis_set))

  basis_set <- as.array(basis_set)
  n <- dim(basis_set)[3]
  if (n < 2) return(basis_set)  
  
  # Estimate number of bases in output
  dists <- sapply(2:n, function(i) {
    proj_dist(basis_set[[i - 1]], basis_set[[i]])
  })
  steps <- sum(ceiling(dists / angle)) * 2
  
  new_basis <- rep(NA, steps)
  new_basis[1] <- TRUE
  
  # Initialise result storage
  projs <- array(NA_real_, c(dim(basis_set)[1:2], steps))
  
  i <- 1
  step <- tour(0)

  while(!is.null(step)) {    
    new_basis[i] <- step$step == 0
    projs[, , i] <- step$proj
    
    i <- i + 1
    step <- tour(angle)
  }
  
  # Trim off extra bases
  projs <- projs[, , seq_len(i) - 1, drop = FALSE]
  new_basis <- new_basis[seq_len(i) - 1, drop = FALSE]

  attr(projs, "new_basis") <- new_basis
  attr(projs, "data") <- attr(basis_set, "data")
  class(projs) <- c("history_array", class(projs))
  projs
}

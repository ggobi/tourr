#' Interpolate geodesically between bases.
#' 
#' This function takes a set of bases and produces a tour by geodesically 
#' interpolating between each basis
#' 
#' @param basis_set input basis set
#' @param angle target distance (in radians) between bases
#' @keywords hplot
#' @examples
#' t1 <- save_history(flea[, 1:6], grand_tour(1), max = 10)
#' dim(t1)
#' dim(interpolate(t1, 0.01))
#' dim(interpolate(t1, 0.05))
#' dim(interpolate(t1, 0.1))
interpolate <- function(basis_set, angle = 0.05) {
  basis_set <- as.array(basis_set)
  n <- dim(basis_set)[3]
  if (n < 2) return(basis_set)  
  
  # Estimate number of bases in output
  dists <- sapply(2:n, function(i) {
    proj_dist(basis_set[[i - 1]], basis_set[[i]])
  })
  steps <- sum(ceiling(dists / angle)) - 1

  # Initialise result storage
  projs <- array(NA_real_, c(dim(basis_set)[1:2], steps))
  projs[, , 1] <- basis_set[[1]]
  
  new_basis <- rep(NA, steps)
  new_basis[1] <- TRUE
  
  # Loop through bases
  path <- geodesic_path(basis_set[[1]], basis_set[[2]])
  dist <- proj_dist(basis_set[[1]], basis_set[[2]])

  i <- 2       # Counter for bases
  step <- 2    # Counter for steps along geodesic
  total <- 2   # Counter for total number of steps
  nsteps <- ceiling(dist / angle)

  while(i < n | step < nsteps) {
    proj <- path$interpolate(step / nsteps)
    projs[, , total] <- proj
    new_basis[total] <- step == 1

    if (step == nsteps) {
      i <- i + 1
      path <- geodesic_path(proj, basis_set[[i]])
      dist <- proj_dist(basis_set[[i - 1]], basis_set[[i]])

      step <- 0
      nsteps <- ceiling(dist / angle)
    }
    step <- step + 1
    total <- total + 1
  }  
  
  attr(projs, "new_basis") <- new_basis
  attr(projs, "data") <- attr(basis_set, "data")
  class(projs) <- c("history_array", class(projs))
  projs
}
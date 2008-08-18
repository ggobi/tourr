# t1 <- save_history(flea[, 1:6], nbases = 10, d = 1)
# tinterp <- interpolate(t1)

# This function takes a set of bases and produces a tour by geodesically 
# interpolating between each basis
interpolate <- function(basis_set, velocity = 0.05) {
  
  if (is.array(basis_set)) {
    get_basis <- function(i) {
      x <- basis_set[, , i, drop = FALSE]
      dim(x) <- dim(x)[1:2]
      x
    }
    n <- dim(basis_set)[3]
  } else {
    get_basis <- function(i) basis_set[[i]]
    n <- length(basis_set)
  }
  if (n < 2) return(basis_set)
  output <- list()
  
  path <- geodesic(get_basis(1), get_basis(2))
  dist <- sqrt(sum(path$tau ^ 2))

  step <- 0
  nsteps <- ceiling(dist / velocity)

  i <- 2
  while(i < n | step < nsteps) {
    proj <- step_fraction(path, step / nsteps)
    output <- append(output, list(proj))

    if (step == nsteps) {
      i <- i + 1
      path <- geodesic(proj, get_basis(i))
      dist <- sqrt(sum(path$tau ^ 2))

      step <- 0
      nsteps <- ceiling(dist / velocity)
    }
    step <- step + 1
  }  
  oarray <- unlist(output)
  dim(oarray) <- c(nrow(output[[1]]), ncol(output[[2]]), length(output))
  attr(oarray, "data") <- attr(basis_set, "data")
  oarray
}

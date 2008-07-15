
basis_random <- function(n, d = 2) {  
  mvn <- matrix(rnorm(n * d), ncol = d)
  orthonormalise(mvn)
}

grand_tour <- function(current, ...) {
  new_target <- function(current) {
    basis_random(nrow(current), ncol(current))
  }

  tour(current, new_target, ...)
}
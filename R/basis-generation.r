
basis_random <- function(n, d = 2) {  
  mvn <- matrix(rnorm(n * d), ncol = d)
  orthonormalise(mvn)
}


# Generate nearby bases, e.g. for simulated annealing
basis_nearby <- function(current, alpha = 0.5, method = "linear") {
  method <- match.arg(method, c("linear", "geodesic"))
  new <- basis_random(nrow(current), ncol(current))
  
  switch(method,
    linear =   orthonormalise((1 - alpha) * current + alpha * new),
    geodesic = step_fraction_rel(geodesic(current, new), alpha)
  )
}

# Basic basis generation function for simulated annealing
basis_better <- function(current, alpha = 0.5, index, method = "linear", max.tries = 100) {
  cur_index <- index(current)
  
  for(i in seq_len(max.tries)) {
    new_basis <- basis_nearby(current, alpha, method)
    if (index(new_basis) > cur_index) return(new_basis)
  }
  
  NA
}

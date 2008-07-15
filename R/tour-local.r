# Generate nearby bases, e.g. for simulated annealing
basis_nearby <- function(current, alpha = 0.5, method = "linear") {
  method <- match.arg(method, c("linear", "geodesic"))
  new <- basis_random(nrow(current), ncol(current))
  
  switch(method,
    linear =   orthonormalise((1 - alpha) * current + alpha * new),
    geodesic = step_fraction(geodesic(current, new), alpha)
  )
}

local_tour <- function(current, data, distance = 0.5, method="geodesic", ...) {
  new_target <- function(...) {
    basis_nearby(current, alpha = distance, method = method)
  }
  
  tour(current, new_target, ...)
}
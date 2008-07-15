correlation_tour <- function(current, n1, n2, ...) {
  new_target <- function(current) {
    cbind(basis_random(n1, 1), basis_random(n2, 1))
  }

  tour(current, new_target, ...)
}

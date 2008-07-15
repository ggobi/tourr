# Basic basis generation function for simulated annealing
basis_better <- function(current, alpha = 0.5, index, method = "linear", max.tries = 100) {
  cur_index <- index(current)
  
  for(i in seq_len(max.tries)) {
    new_basis <- basis_nearby(current, alpha, method)
    if (index(new_basis) > cur_index) return(new_basis)
  }
  
  NA
}


guided_tour <- function(current, data, index_f, temp = 1, cooling = 0.99, ...) {
  index <- function(proj, data) index_f(as.matrix(data) %*% proj)
  
  temp <- 1
  new_target <- function(current) {
    basis <- basis_better(current, temp, index)
    temp <<- temp * cool
    basis
  }
  
  tour(current, new_target, ...)
}
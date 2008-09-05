# Basic basis generation function for simulated annealing
basis_better <- function(current, alpha = 0.5, index, max.tries = Inf,
  method = "linear"
) {
  cur_index <- index(current)
  
  cat("Old", cur_index, "\n")
  try <- 1
  while(try < max.tries) {
    new_basis <- basis_nearby(current, alpha, method)
    
    new_index <- index(new_basis)
    if (new_index > cur_index) {
      cat("New", new_index, "try", try, "\n")
      return(new_basis)
    }
    try <- try + 1
  }
  
  NULL
}


basis_better_plus_random<- function(current, alpha = 0.5, index,
  max.tries = Inf, method = "linear", eps = 0.001
) {
  cur_index <- index(current)
  
  cat("Old", cur_index, "\n")
  try <- 1
  while(try < max.tries) {
    new_basis <- basis_nearby(current, alpha, method)
    new_index <- index(new_basis)
    if (new_index > cur_index) {
      cat("New", new_index, "try", try, "\n")
      return(new_basis)
    }
    else if (abs(new_index-cur_index) < eps) {
      new_basis <- basis_random(nrow(current), ncol(current))
      cat("Adding random step", cur_index, new_index, "\n")
      return(new_basis)
    }
    try <- try + 1
  }
  
  NULL
}

guided_tour <- function(current, data, index_f, temp = 1, cooling = 0.99, max.tries = 5, basis_f = basis_better, ...) {
  index <- function(proj) {
    index_f(as.matrix(data) %*% proj)
  }

  temp <- 1
  new_target <- function(current) {
    basis <- basis_f(current, temp, index, max.tries)
    temp <<- temp * cooling

    basis
  }
  
  tour(current, new_target, ...)
}


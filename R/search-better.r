#' Search for a better projection near the current projection
search_better <- function(current, alpha = 0.5, index, max.tries = Inf,
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


search_better_random <- function(current, alpha = 0.5, index,
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


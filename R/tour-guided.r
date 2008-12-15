#' The guided tour
#'
#' Instead of choosing new projections at random like the grand tour, the 
#' guided tour always tries to find a projection that is more interesting
#' than the current projection.
#'
#' @seealso \code{\link{cm}}, \code{\link{holes}} and \code{\link{lda_pp}}
#'   for examples of index functions
guided_tour <- function(current, data, index_f, temp = 1, cooling = 0.99, max.tries = 25, basis_f = basis_geodesic_search, ...) {
  index <- function(proj) {
    index_f(as.matrix(data) %*% proj)
  }

  temp <- 1
  new_target <- function(current) {
    basis <- basis_f(current, temp, index, max.tries)
    temp <<- temp * cooling

    basis
  }
  
  tour(current, new_target)
}


basis_geodesic_search <- function(current, alpha = 1, index, max.tries = 5) {
  cur_index <- index(current)
  
  try <- 1
  while(try < max.tries) {
    # Try 10 random directions and pick the one that has the highest
    # index with a small step
    direction <- find_best_dir(current, index)
    
    # Travel right round (pi / 2 radians) the sphere in that direction
    # looking for the best projection
    peak <- find_path_peak(current, direction, index)
    
    pdiff <- (peak$index - cur_index) / cur_index
    if (pdiff > 0.001) {
      cat("New index: ", peak$index, " (", peak$dist, " away)\n", sep="")
      return(peak$basis)
    }
    cat("Best was   ", peak$index, " (", peak$dist, " away).  Trying again...\n", sep="")
    
    try <- try + 1
  }
  
  NULL  
}

find_best_dir <- function(old, index, dist = 0.01, tries = 5) {
  bases <- replicate(tries, basis_random(nrow(old), ncol(old)),
    simplify = FALSE)
  
  score <- function(new) {
    interpolator <- geodesic(old, new)
    forward <- step_angle(interpolator, dist)
    backward <- step_angle(interpolator, -dist)

    max(index(forward), index(backward))
  }
  scores <- sapply(bases, score)
  bases[[which.max(scores)]]
}

find_path_peak <- function(old, new, index, max_dist = pi / 4) {
  interpolator <- geodesic(old, new)

  index_pos <- function(alpha) index(step_angle(interpolator, alpha))
  
  alpha <- optimize(index_pos, c(-max_dist, max_dist), maximum = TRUE, tol = 0.01)
  
  list(
    basis = step_angle(interpolator, alpha$maximum),
    index = alpha$objective,
    dist = alpha$maximum
  )
}

# f <- rescale(as.matrix(flea[, 1:3]))
# findex <- function(proj) {
#   cm(f %*% proj)
# }
# 
#
# animate_xy(flea[, 1:6], guided_tour, index_f = cm, basis_f = basis_geodesic_search, sphere = T)
#
# tries <- replicate(5, save_history(flea[, 1:3], guided_tour, index_f = holes, basis_f = basis_geodesic_search, sphere = T), simplify = F)
# tion function for simulated annealing
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


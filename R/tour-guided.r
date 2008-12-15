#' The guided tour
#'
#' Instead of choosing new projections at random like the grand tour, the 
#' guided tour always tries to find a projection that is more interesting
#' than the current projection.
#'
#' @param search_f 
#' @param alpha the initial size of the search window, in radians
#' @param cooling the amount the size of the search window should be adjusted
#'   by after each step
#' @seealso \code{\link{cm}}, \code{\link{holes}} and \code{\link{lda_pp}}
#'   for examples of index functions.  The function should take a numeric
#'   matrix and return a single number, preferrably between 0 and 1.
#' @seealso \code{\link{search_geodesic}}, \code{\link{search_better}},
#'   \code{\link{search_better_random}} for different search functions
guided_tour <- function(index_f, d = 2, alpha = 1, cooling = 0.99, max.tries = 25, search_f = search_geodesic) {

  generator <- function(current, data) {
    if (is.null(current)) return(basis_init(ncol(data), d))    
    
    index <- function(proj) {
      index_f(as.matrix(data) %*% proj)
    }

    basis <- search_f(current, alpha, index, max.tries)
    alpha <<- alpha * cooling

    basis
  }
  
  new_tour_path("guided", generator)
}


search_geodesic <- function(current, alpha = 1, index, max.tries = 5) {
  cur_index <- index(current)
  
  try <- 1
  while(try < max.tries) {
    # Try 5 random directions and pick the one that has the highest
    # index after a small step in either direction
    direction <- find_best_dir(current, index, 5)
    
    # Travel right round (pi / 2 radians) the sphere in that direction
    # looking for the best projection
    peak <- find_path_peak(current, direction, index)
    
    pdiff <- (peak$index - cur_index) / cur_index
    if (pdiff > 0.001) {
      cat("New index: ", peak$index, " (", peak$dist, " away)\n", sep="")
      return(peak$basis)
    }
    cat("Best was:   ", peak$index, " (", peak$dist, " away).  Trying again...\n", sep="")
    
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


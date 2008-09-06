basis_geodesic_search <- function(current, alpha = 1, index, max.tries = 5) {
  cur_index <- index(current)
  
  try <- 1
  while(try < max.tries) {
    # Try 10 random directions and pick the one that has the highest
    # index with a small step
    direction <- find_best_dir(current, index)
    
    # Travel right round (pi / 2 radians) the sphere in that direction
    # looking for the best projection
    peak <- find_path_peak(current, direction, index, pi / 2)
    
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

find_best_dir <- function(old, index, dist = 0.01, tries = 10) {
  bases <- replicate(tries, basis_random(nrow(old), ncol(old)),
    simplify = FALSE)
  
  score <- function(new) {
    interpolator <- geodesic(old, new)
    small_step <- step_angle(interpolator, dist)
    index(small_step)
  }
  scores <- sapply(bases, score)
  bases[[which.max(scores)]]
}

find_path_peak <- function(old, new, index, max_dist = pi / 2) {
  interpolator <- geodesic(old, new)

  index_pos <- function(alpha) index(step_angle(interpolator, alpha))
  
  alpha <- optimize(index_pos, c(0, max_dist), maximum = TRUE, tol = 0.01)
  
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
# animate_xy(flea[, 1:3], guided_tour, index_f = cm, basis_f = basis_geodesic_search, sphere = T)
#
# tries <- replicate(5, save_history(flea[, 1:3], guided_tour, index_f = holes, basis_f = basis_geodesic_search, sphere = T), simplify = F)
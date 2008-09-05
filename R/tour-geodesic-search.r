basis_geodesic_search <- function(current, alpha = 1, index, max.tries = 5) {
  cur_index <- index(current)
  
  try <- 1
  while(try < max.tries) {
    direction <- basis_random(nrow(current), ncol(current))
    peak <- find_path_peak(current, direction, index, pi / 2)
    
    if (peak$index > cur_index) {
      cat("New index: ", peak$index, " (", peak$dist, " away)\n", sep="")
      return(peak$basis)
    }
    cat("Best was   ", peak$index, " (", peak$dist, " away).  Trying again...\n", sep="")
    
    try <- try + 1
  }
  
  NULL
  
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
# animate_xy(flea[, 1:3], guided_tour, index_f = cm, basis_f = basis_geodesic_search)

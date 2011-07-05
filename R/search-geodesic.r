#' Search for most interesting projection along random geodesics.
#'
#' This is a novel method for finding more interesting projections for the 
#' guided tour.  It works by first taking a small step in \code{n} random 
#' directions, and then picking the direction that looks most promising
#' (based on the height of the index function).  Once the best direction is 
#' found, it performs a linear search along the geodesic in that direction,
#' traveling up to half way around the sphere.
#'
#' You should not to have call this function directly, but should supply it 
#' to the \code{\link{guided_tour}} as a search strategy.
#'
#' @param current starting projection
#' @param alpha maximum distance to travel (currently ignored)
#' @param index interestingness index function
#' @param max.tries maximum number of failed attempts before giving up
#' @param n number of random steps to take to find best direction
#' @keywords optimize
search_geodesic <- function(current, alpha = 1, index, max.tries = 5, n = 5) {
  cur_index <- index(current)
  
  try <- 1
  while(try < max.tries) {
    # Try 5 random directions and pick the one that has the highest
    # index after a small step in either direction
    direction <- find_best_dir(current, index, tries = n)
    
    # Travel halfway round (pi / 4 radians) the sphere in that direction
    # looking for the best projection
    peak <- find_path_peak(current, direction, index)
    
    pdiff <- (peak$index - cur_index) / cur_index
    
    dig3 <- function(x) sprintf("%.3f", x)
    
    cat("Value ", dig3(peak$index), " ", 
        sprintf("%.1f", pdiff * 100), "% better ",
        "(", dig3(peak$dist), " away)", sep="")
    if (pdiff > 0.001) {
      cat(" - NEW BASIS\n")
      return(peak$basis)
    }
    cat("\n")
    
    try <- try + 1
  }
  cat("No better bases found after ", max.tries, " tries.  Giving up.\n",
   sep="")
  cat("Final projection: \n")
  if (ncol(current)==1) {
    for (i in 1:length(current))
      cat(sprintf("%.3f",current[i])," ")
    cat("\n")
  }
  else {
    for (i in 1:nrow(current)) {
      for (j in 1:ncol(current))
        cat(sprintf("%.3f",current[i,j])," ")
      cat("\n")
    }
  }
  
  NULL  
}

#' Find the most promising direction to travel in.
#'
#' Starting from the current projection, pick \code{tries} random location
#' and take a small step towards and away from each location.  The most 
#' promising direction has the highest value of the \code{index} function.
#'
#' @keywords optimize internal
#' @param old current projection
#' @param index interestingness index function
#' @param dist step size in radians, should be small
#' @param number of random steps to take
find_best_dir <- function(old, index, dist = 0.01, tries = 5) {
  bases <- replicate(tries, basis_random(nrow(old), ncol(old)),
    simplify = FALSE)
  
  score <- function(new) {
    interpolator <- geodesic_info(old, new)
    forward <- step_angle(interpolator, dist)
    backward <- step_angle(interpolator, -dist)

    max(index(forward), index(backward))
  }
  scores <- sapply(bases, score)
  bases[[which.max(scores)]]
}

#' Find the most interesting projection along a geodesic.
#'
#' Use \code{\link{optimize}} to find the most interesting projection amongst
#' all projections on a geodesic.  This method assumes that the function is 
#' continuous with a single maximum, but seems to do ok even if there are 
#' multiple maxima.
#' 
#' @param old currention project
#' @param new projection that gives direction to travel in
#' @param index interestingness index function
#' @param max_dist maximum distance to travel along in radians
#' @keywords optimize internal
find_path_peak <- function(old, new, index, max_dist = pi / 4) {
  interpolator <- geodesic_info(old, new)

  index_pos <- function(alpha) index(step_angle(interpolator, alpha))
  
  alpha <- optimize(index_pos, c(-max_dist, max_dist), maximum = TRUE, tol = 0.01)
  
  list(
    basis = step_angle(interpolator, alpha$maximum),
    index = alpha$objective,
    dist = abs(alpha$maximum)
  )
}

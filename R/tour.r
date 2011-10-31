#' Create a new tour.
#'
#' The tour function provides the common machinery behind all tour methods:
#' interpolating from basis to basis, and generating new bases when necessary.
#' You should not have to call this function.
#' 
#' @param data the data matrix to be projected
#' @param tour_path basis generator, a function that generates a new basis,
#'   called with the previous projection and the data set.  For more
#'   complicated tour paths, this will need to be a closure with local
#'   variables.  Should return NULL if the tour should terminate
#' @param start starting projection, if omitted will use default projection 
#'   from generator
#' @seealso \code{\link{save_history}}, \code{\link{render}} and
#'   \code{\link{animate}} for examples of functions that use this function
#'   to run dynamic tours.
#' @keywords hplot dynamic internal
#' @return a function with single argument, step_size.  This function returns
#'  a list containing the new projection, the currect target and the number
#'  of steps taken towards the target.
#' @export
new_tour <- function(data, tour_path, start = NULL) {
  stopifnot(inherits(tour_path, "tour_path"))

  if (is.null(start)) {
    start <- tour_path(NULL, data)
  }
  proj <- start

  # Initialise first step
  target <- NULL
  step <- 0

  cur_dist <- 0
  target_dist <- 0
  geodesic <- NULL
  
  function(step_size) {
    step <<- step + 1
    cur_dist <<- cur_dist + step_size
    
    # We're at (or past) the target, so generate a new one and reset counters
    if (cur_dist >= target_dist) {
      geodesic <<- tour_path(proj, data)
      if (is.null(geodesic)) return(NULL)

      target_dist <<- geodesic$dist
      target <<- geodesic$Fz
      cur_dist <<- 0
      # Only exception is if the step_size is infinite - we want to jump 
      # to the target straight away
      if (!is.finite(step_size)) {
        cur_dist <<- target_dist
      }
      
      step <<- 0
    }
    
    proj <<- geodesic$interpolate(cur_dist / target_dist)
    list(proj = proj, target = target, step = step)
  }
}

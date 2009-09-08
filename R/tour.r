#' Generic tour method.
#'
#' The tour function provides the common machinery behind all tour methods:
#' interpolating from basis to basis, and generating new bases when necessary.
#' You should not have to call this function unless you are writing your own
#' tour path method.
#' 
#' @param data the data matrix to be projected
#' @param tour_path basis generator, a function that generates a new basis,
#'   called with the previous projection and the data set.  For more
#'   complicated tour paths, this will need to be a closure with local
#'   variables.  Should return NULL if the tour should terminate
#' @param start starting projection, if omitted will use random projection
#' @param velocity tour velocity in radians per step
#' @param total_steps maximum number of steps before termination (set to Inf 
#'   to never terminate)
#' @param step_fun a function that is evaluated every time a step is taken.  
#'   the function should have arguments step (the number of steps), proj (the
#'   new projection matrix) and target (the current target frame)
#' @param target_fun a function that is evaluated every time a new target is 
#'   chosen
#' @param ... Not Used
#' @seealso \code{\link{grand_tour}}, \code{\link{guided_tour}}, 
#'   \code{\link{local_tour}}, \code{\link{little_tour}} for concrete 
#'   tour path implementations.
#' @keywords hplot dynamic
tour <- function(data, tour_path, start = NULL, velocity = 0.05, 
  total_steps = 100, step_fun = nul, target_fun = nul, ...){

  stopifnot(inherits(tour_path, "tour_path"))
  if (is.null(start)) {
    start <- tour_path(NULL, data)
  }

  # Initialise first step
  target <- tour_path(start, data)
  target_fun(target$Fz, target)
  step <- 0
  nsteps <- ceiling(target$dist / velocity)

  step_counter <- 0
  while(step_counter < total_steps) {
    proj <- target$interpolate(step / nsteps)
    step_fun(step, proj, target)
  
    if (step == nsteps) {
      target <- tour_path(proj, data)
      if (is.null(target)) return(invisible())
        
      target_fun(target$Fz, target)
      step <- 0
      nsteps <- ceiling(target$dist / velocity)
    }

    step <- step + 1
    step_counter <- step_counter + 1
  }
}

#' Internal Function
#' 
#' @keywords internal
tourer <- function(data, tour_path, proj = NULL, velocity = 0.05) {
  stopifnot(inherits(tour_path, "tour_path"))
  if (is.null(proj)) {
    proj <- tour_path(NULL, data)
  }

  # Initialise first step
  target <- NULL
  nsteps <- 0
  step <- 0
  
  take_step <- function() {
    if (step == nsteps) {
      target <<- tour_path(proj, data)
      if (is.null(target)) return(NULL)

      nsteps <<- ceiling(target$dist / velocity)
      step <<- 0
    }
    
    proj <<- target$interpolate(step / nsteps)
    step <<- step + 1
    list(step = step, proj = proj, target = target)
  }

  cur_dist <- 0
  target_dist <- 0
  take_step2 <- function(step_size) {
    if (cur_dist >= target_dist) {
      target <<- tour_path(proj, data)
      if (is.null(target)) return(NULL)

      target_dist <<- target$dist
      cur_dist <<- 0
      step <<- 0
    }
    
    proj <<- target$interpolate(cur_dist / target_dist)
    step <<- step + 1
    cur_dist <<- cur_dist + step_size
    list(step = step, proj = proj, target = target)
    
  }


  list(step = take_step, step2 = take_step2)
}
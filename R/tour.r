#' Generic tour method.
#'
#' The tour function provides the common machinery behind all tour methods:
#' interpolating from basis to basis, and generating new bases when necessary.
#' You should not have to call this function unless you are writing your own
#' tour path method.
#' 
#'
#' @param current the starting projection matrix (used to determine size etc.)
#' @param target_f a function that generates a new basis function, based on 
#'   on the previous project.  For more complicated tour paths, this will need
#'   to be a closure with local variables.  Should return NULL if the tour
#'   should terminate
#' @param velocity tour velocity in radians per step
#' @param total_steps maximum number of steps before termination (set to Inf 
#'   to never terminate)
#' @param step_fun a function that is evaluated every time a step is taken.  
#'   the function should have arguments step (the number of steps), proj (the
#'   new projection matrix) and target (the current target frame)
#' @param target_fun a function that is evaluated every time a new target is 
#'   chosen
#' @seealso \code{\link{grand_tour}}, \code{\link{guided_tour}}, 
#'   \code{\link{local_tour}}, \code{\link{little_tour}} for concrete 
#'   tour path implementations.
#' @keywords hplot, dynamic
tour <- function(
  current, target_f, velocity = 0.05, total_steps = 100,
  step_fun = nul, target_fun = nul, ...
){
  new_target <- geodesic_path(target_f)
  
  target <- new_target(current)
  target_fun(target$frame, target)
  step <- 0
  nsteps <- ceiling(target$dist / velocity)

  step_counter <- 1
  while(step_counter < total_steps) {
    proj <- target$interpolate(step / nsteps)
    step_fun(step, proj, target)

    if (step == nsteps) {
      target <- new_target(proj)
      if (is.null(target)) return()
      
      target_fun(target$frame, target)
      step <- 0
      nsteps <- ceiling(target$dist / velocity)
    }
    step <- step + 1
    step_counter <- step_counter + 1
  }
}

#' Generate geodesic path.
#'
#' Wrap basis generation method with a function that keeps computes the 
#' geodesic interpolation from the previous frame to the next frame, and
#' provides convenient access to all the information about the path.
#'
#' @param new_target_f function that generates new frame, with previous 
#'   frame as argument
#' @keywords internal
#' @return
#'   \item{frame}{The newly generated frame}
#'   \item{frame_prev}{The previous frame}
#'   \item{interpolate}{A wrapper around \code{\link{step_fraction} with the
#'     geodesic set.  It takes a single number between 0 and 1}}
#'   \item{dist}{The distance, radians, between the previous and current
#'     frame}
#'  \item{tau}{The principle angles}
#'  \item{Ga}{The starting plane}
#'  \item{Gz}{The target plane}
geodesic_path <- function(new_target_f) { 
  function(previous) {    
    dist <- 0
    # Keep trying until we get a target that's not equivalent to the previous
    while (dist < 1e-3) {
      frame <- new_target_f(previous)
      if (is.null(frame)) return(NULL)
      interpolator <- geodesic(previous, frame)
      dist <- sqrt(sum(interpolator$tau ^ 2))      
    }

    # cat(interpolator$tau[1],"\n")
    list(
      frame = frame,
      frame_prev = previous,
      interpolate = function(pos) step_fraction(interpolator, pos),
      dist = dist,
      tau = interpolator$tau,
      Ga = interpolator$Ga,
      Gz = interpolator$Gz
    )
  }
}

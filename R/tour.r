nul <- function(...) {}


# The tour function provides the common machinery behind all tour methods:
# interpolating from basis to basis, and generating new bases when necessary.

tour <- function(
  current, target_f, velocity = 0.05, total_steps = 100,
  step_fun = nul, target_fun = nul, ...
){
  new_target <- geodesic_path(target_f)
  
  target <- new_target(current)
  target_fun(target$frame)
  step <- 0
  nsteps <- ceiling(target$dist / velocity)

  step_counter <- 1
  while(step_counter < total_steps) {
    proj <- target$interpolate(step / nsteps)
    step_fun(step, proj)

    if (step == nsteps) {
      target <- new_target(proj)
      target_fun(target$frame)
      step <- 0
      nsteps <- ceiling(target$dist / velocity)
    }
    step <- step + 1
    step_counter <- step_counter + 1
    cat("nbases",step_counter,"\n")
  }
}


# new_target_f is the basis generator fn
geodesic_path <- function(new_target_f) { 
  function(previous) {    
    dist <- 0
    # Keep trying until we get a target that's not equivalent to the previous
    while (dist < 1e-3) {
      frame <- new_target_f(previous)
      interpolator <- geodesic(previous, frame)
      dist <- sqrt(sum(interpolator$tau ^ 2))      
    }
    
    list(
      frame = frame,
      frame_prev = previous,
      interpolate = function(pos) step_fraction(interpolator, pos),
      dist = dist,
      tau = interpolator$tau
    )
  }
}

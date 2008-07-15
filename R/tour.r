nul <- function(...) {}

grand_tour <- function(current, ...) {
  new_target <- function(current) {
    basis_random(nrow(current), ncol(current))
  }

  tour(current, new_target, ...)
}

guided_tour <- function(current, data, index_f, temp = 1, cooling = 0.99, ...) {
  index <- function(proj, data) index_f(as.matrix(data) %*% proj)
  
  temp <- 1
  new_target <- function(current) {
    basis <- basis_better(current, temp, index)
    temp <<- temp * cool
    basis
  }
  
  tour(current, new_target, ...)
}

correlation_tour <- function(current, n1, n2, ...) {
  new_target <- function(current) {
    cbind(basis_random(n1, 1), basis_random(n2, 1))
  }

  tour(current, new_target, ...)
}


tour <- function(
  current, target_f, velocity = 0.05, total_steps = 100,
  step_fun = nul, target_fun = nul
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
  }
}

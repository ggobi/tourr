# source("basis-generation.r")
nul <- function(...) {}

grand_tour <- function(current, ...) {
  new_target <- geodesic_path(function(...) {
    basis_random(nrow(current), ncol(current))
  })

  tour(new_target, current, ...)
}

guided_tour <- function(current, data, index_f, temp = 1, cooling = 0.99, ...) {
  index <- function(proj, data) index_f(as.matrix(data) %*% proj)
  
  temp <- 1
  new_target <- geodesic_path(function(current) {
    basis <- basis_better(current, temp, index)
    temp <<- temp * cool
    basis
  })
  
  tour(new_target, ...)
}

correlation_tour <- function(current, n1, n2, ...) {
  new_target <- geodesic_path(function(...) {
    cbind(basis_random(n1, 1), basis_random(n2, 1))
  })

  tour(new_target, ...)
}

little_tour <-function(basis_set, current, ...) {
  indx <- 2
#  while (indx < length(basis_set)) {
#    current <- basis_set[[indx]]
    cat("indx", indx)
    new_target <- geodesic_path(function(...) {
      cat("indx 2", indx)
      indx <<- indx + 1
      bases_fixed(basis_set, indx)
    })
    cat("current",current[1,1],current[1,2],"\n")
        
    tour(new_target, current, ...)
    indx <<- indx + 1
    if (indx > length(basis_set))
      indx <<- 1
#  }
}

tour <- function(new_target, current, velocity = 0.05, total_steps = 100,
                 step_fun = nul, target_fun = nul) {
  target <- new_target(current)
  target_fun(target$frame)

  step_counter <- 1
  step <- 1
  angle <- 0

  while(step_counter < total_steps) {
    nsteps <- ceiling(target$dist/velocity)
#    cat("nsteps",nsteps,"\n")
    
    angle <- step/nsteps
    proj <- target$interpolate(angle)
    step_fun(step, proj)

    if (angle == 1) {
      target <- new_target(proj)
      step <- 0
      target_fun(target$frame)
      nsteps <- ceiling(target$dist/velocity)
    }
    step <- step + 1
    step_counter <- step_counter + 1
  }
}

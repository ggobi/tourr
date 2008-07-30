#X t1 <- save_history(mtcars[, 1:5], nbases = 3)
#X animate_xy(mtcars[, 1:5], planned_tour, basis_set = t1)
#X animate_pcp(mtcars[, 1:5], planned_tour, basis_set = t1)

save_history <- function(data, tourf = grand_tour, d = 2, nbases = 100, interpolate = FALSE, ..., rescale = TRUE, sphere = FALSE){
  if (rescale) data <- rescale(data)
  if (sphere) data  <- sphere(data)
  
  # Start with plot of first two variables
  start <- matrix(0, nrow = ncol(data), ncol = d)
  diag(start) <- 1

  # A bit inefficient, but otherwise save changes to rest of tour code
  velocity <- if (interpolate) 0.05 else 10

  projs <- array(NA, c(ncol(data), d, nbases))
  count <- 0
  
  step <- function(step, proj) {
    if (interpolate) {
      count <<- count+1
      projs[, , count] <<- proj
    }
  }
  target <- function(target) {
    if (!interpolate) {
      count <<- count+1
      projs[, , count] <<- target      
    }
  }

  tourf(
    start, velocity = velocity, total_steps = nbases + 1,
    step_fun = step, target_fun = target,  ..., data=data
  )
  
  attr(projs, "data") <- data
  projs
}
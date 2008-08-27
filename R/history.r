#X t1 <- save_history(flea[, 1:6], nbases = 3)
#X animate_xy(flea[, 1:6], planned_tour, basis_set = t1)
#X t1 <- save_history(flea[, 1:6], nbases = 3, d = 4)
#X animate_pcp(flea[, 1:6], planned_tour, basis_set = t1)
#X t1 <- save_history(flea[, 1:6], nbases = 3, d = 1)
#X animate_density(flea[, 1:6], planned_tour, basis_set = t1)
#X testdata <- matrix(rnorm(100*2), ncol=2)
#X testdata[1:50,1] <- testdata[1:50,1] + 10
#X testdata <- sphere(testdata)
#X t2 <- save_history(testdata, tour_f = guided_tour, index_f = holes, nbases=5, d=1, rescale=F, sphere=F, max.tries = 100, cooling = 0.95)

save_history <- function(data, tour_f = grand_tour, d = 2, nbases = 100, interpolate = FALSE, ..., rescale = TRUE, sphere = FALSE){
  if (rescale) data <- rescale(data)
  if (sphere) data  <- sphere(data)
  
  # Start with plot of first two variables
  start <- basis_random(ncol(data), d)
#  start <- matrix(0, nrow = ncol(data), ncol = d)
#  diag(start) <- 1

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

  tour_f(
    start, velocity = velocity, total_steps = nbases + 1,
    step_fun = step, target_fun = target,  ..., data=data
  )
  
  attr(projs, "data") <- data
  projs
}

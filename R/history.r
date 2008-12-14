#X t1 <- save_history(flea[, 1:6], nbases = 3)
#X animate_xy(flea[, 1:6], planned_tour, basis_set = t1)
#X t1 <- save_history(flea[, 1:6], nbases = 3, d = 4)
#X animate_pcp(flea[, 1:6], planned_tour, basis_set = t1)
#X t1 <- save_history(flea[, 1:6], nbases = 3, d = 1)
#X animate_dist(flea[, 1:6], planned_tour, basis_set = t1)
#X testdata <- matrix(rnorm(100*2), ncol=2)
#X testdata[1:50,1] <- testdata[1:50,1] + 10
#X testdata <- sphere(testdata)
#X t2 <- save_history(testdata, tour_f = guided_tour, index_f = holes, nbases=5, d=1, rescale=F, sphere=F, max.tries = 100, cooling = 0.95)

save_history <- function(data, tour_f = grand_tour, d = 2, nbases = 100, ..., rescale = TRUE, sphere = FALSE){
  if (rescale) data <- rescale(data)
  if (sphere) data  <- sphere(data)
  
  # Start with random basis
  start <- basis_random(ncol(data), d)

  # A bit inefficient, but otherwise save changes to rest of tour code
  velocity <- 10

  projs <- array(NA, c(ncol(data), d, nbases + 1))
  projs[, , 1] <- start
  count <- 1
  
  target <- function(target, geodesic) {
    count <<- count+1
    projs[, , count] <<- target      
  }

  tour_f(
    start, velocity = velocity, total_steps = nbases + 1,
    step_fun = nul, target_fun = target,  ..., data=data
  )
  
  # Remove empty matrices for tours that terminated early
  # (e.g. guided tour)
  empty <- apply(projs, 3, function(x) all(is.na(x)))
  projs <- projs[, , !empty, drop = FALSE]
  
  attr(projs, "data") <- data
  projs
}

history_geodesics <- function(data, tour_f = grand_tour, d = 2, nbases = 100, ..., rescale = TRUE, sphere = FALSE){
  if (rescale) data <- rescale(data)
  if (sphere) data  <- sphere(data)
  
  # Start with random basis
  start <- basis_random(ncol(data), d)

  # A bit inefficient, but otherwise save changes to rest of tour code
  velocity <- 10

  projs <- array(NA, c(ncol(data), d, nbases + 1))
  projs[, , 1] <- start
  princ_dirs <- array(NA, c(ncol(data), d, nbases + 1))
  count <- 1
  
  target <- function(target, geodesic) {
    count <<- count+1
    projs[, , count] <<- target      

    princ_dirs[, , count] <<- geodesic$Gz
    cat(dim(princ_dirs[, , count]),"\n")
  }

  tour_f(
    start, velocity = velocity, total_steps = nbases + 1,
    step_fun = nul, target_fun = target,  ..., data=data
  )
  
  # Remove empty matrices for tours that terminated early
  # (e.g. guided tour)
  empty <- apply(projs, 3, function(x) all(is.na(x)))

  projs <- projs[, , !empty, drop = FALSE]
  princ_dirs <- princ_dirs[, , !empty, drop = FALSE]

  attr(projs, "data") <- data
  attr(princ_dirs, "data") <- data

  list(projs, princ_dirs)
}

#' Save tour history
#'
#' Save a tour path so it can later be displayed in many different ways.
#'
#' @param data matrix, or data frame containing numeric columns
#' @param tour_path tour path generator, defaults to the grand tour
#' @param max_bases maximum number of new bases to generate.  Some tour paths
#'  (like the guided tour) may generate less than the maximum.
#' @param start starting projection, if you want to specify one
#' @param rescale if true, rescale all variables to range [0,1]?
#' @param sphere if true, sphere all variables
#'
#' @examples
#' t1 <- save_history(flea[, 1:6], max = 3)
#' animate_xy(flea[, 1:6], planned_tour(t1))
#' andrews_history(t1)
#' andrews_history(interpolate(t1))
#'
#' t1 <- save_history(flea[, 1:6], grand_tour(4), max = 3)
#' animate_pcp(flea[, 1:6], planned_tour(t1))
#' animate_scatmat(flea[, 1:6], planned_tour(t1))
#'
#' t1 <- save_history(flea[, 1:6], grand_tour(1), max = 3)
#' animate_dist(flea[, 1:6], planned_tour(t1))
#'
#' testdata <- matrix(rnorm(100*3), ncol=3)
#' testdata[1:50, 1] <- testdata[1:50, 1] + 10
#' testdata <- sphere(testdata)
#' t2 <- save_history(testdata, guided_tour(holes, max.tries = 100), max = 5, rescale=F)
#' animate_xy(testdata, planned_tour(t2))
save_history <- function(data, tour_path = grand_tour(), max_bases = 100, start = NULL, rescale = TRUE, sphere = FALSE){
  if (rescale) data <- rescale(data)
  if (sphere) data  <- sphere(data)
  
  # A bit inefficient, but saves changes to rest of tour code
  # Basically ensures that we only ever jump from one basis to the next:
  # don't use any geodesic interpolation
  velocity <- 10
  
  if (is.null(start)) {
    start <- tour_path(NULL, data)    
  }

  projs <- array(NA, c(ncol(data), ncol(start), max_bases + 1))
  projs[, , 1] <- start
  count <- 1
  
  target <- function(target, geodesic) {
    count <<- count+1
    projs[, , count] <<- target
  }

  tour(data, tour_path, start = start,
    velocity = velocity, total_steps = max_bases + 1,
    target_fun = target
  )
  
  # Remove empty matrices for tours that terminated early
  # (e.g. guided tour)
  empty <- apply(projs, 3, function(x) all(is.na(x)))
  projs <- projs[, , !empty, drop = FALSE]
  
  attr(projs, "data") <- data
  projs
}

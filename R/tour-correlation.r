# pos is a logical vector - T corresponds to X, F corresponds to Y

correlation_tour <- function(pos) {
  x <- rep(0, length(pos))
  y <- rep(0, length(pos))
  
  generator <- function(current) {
    x[pos] <<- basis_random(sum(pos), 1)
    y[!pos] <<- basis_random(sum(!pos), 1)
    
    cbind(x, y)
  }

  new_tour_path("correlation", generator)
}

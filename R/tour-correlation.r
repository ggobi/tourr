# pos is a logical vector - T corresponds to X, F corresponds to Y

correlation_tour <- function(current, pos, ...) {
  x <- rep(0, length(pos))
  y <- rep(0, length(pos))
  
  new_target <- function(current) {
    x[pos] <<- basis_random(sum(pos), 1)
    y[!pos] <<- basis_random(sum(!pos), 1)
    
    cbind(x, y)
  }

  tour(current, new_target)
}

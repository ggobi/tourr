# The planned tour takes you from one basis to the next in a 
# set order.  It cycles back from the last basis to the first.
planned_tour <- function(current, basis_set, ...) {
  index <- 0
  n <- length(basis_set)
  
  new_target <- function(current) {
    index <<- (index %% n) + 1
    basis_set[[index]]
  }
        
  tour(current, new_target, ...)
}


# The little tour ------------------------------------------------------------

little_tour <- function(current, ...) {
  little <- bases_little(nrow(current), ncol(current))
  planned_tour(current, little, ...)
}

# Generate bases for the little tour
bases_little <- function(p, d = 2) {
  b <- diag(rep(1, p))
  vars <- combn(p, d)
  lapply(seq_len(ncol(vars)), function(i) b[, vars[, i]] )
}

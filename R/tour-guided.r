# Basic basis generation function for simulated annealing
basis_better <- function(current, alpha = 0.5, index, method = "linear", max.tries = Inf) {
  cur_index <- index(current)
  
  cat("Old", cur_index, "\n")
  try <- 1
  while(try < max.tries) {
    new_basis <- basis_nearby(current, alpha, method)
    new_index <- index(new_basis)
    if (new_index > cur_index) {
      # cat("New", new_index, "\n")
      return(new_basis)
    }
    try <- try + 1
  }
  
  NA
}


guided_tour <- function(current, data, index_f, temp = 1, cooling = 0.99, ...) {
  index <- function(proj) {
    index_f(as.matrix(data) %*% proj)
  }
  
  temp <- 1
  new_target <- function(current) {
    basis <- basis_better(current, temp, index)
    temp <<- temp * cooling
    basis
  }
  
  tour(current, new_target, ...)
}

# Not rotationally invariant - bad idea!
cor1 <- function(matrix) abs(cor(matrix[, 1], matrix[, 2]))

holes <- function(mat) {
  n <- nrow(mat)
  d <- ncol(mat)

  num <- 1 - 1/n * sum(exp(-0.5 * apply(mat, 1, crossprod)))
  den <- 1 - exp(-d / 2)
  
  num/den
}

cm <- function(mat) {
  n <- nrow(mat)
  d <- ncol(mat)

  num <- 1/n * sum(exp(-0.5 * apply(mat, 1, crossprod))) - exp(-d / 2)
  den <- 1 - exp(-d / 2)
  
  num/den
}

ldaPP <- function(mat, cl) {
  if (length(unique(cl)) < 2)
    return(NA)

  fit <- manova(mat~cl)

  1-summary(fit,test="Wilks")$stats[[3]]
}

#X r_tour(mtcars[, 1:5], guided_tour, index_f = cor1)

normalise <- function(x) {
  t(t(x) / sqrt(colSums(x ^ 2)))
}

# Orthonormalise using modified Gram-Schmidt_process
orthonormalise <- function(x) {
  x <- normalise(x) # to be conservative
  
  for (j in seq_len(ncol(x))) {
    for (i in seq_len(j - 1)) {
      x[, j] <- x[, j] - crossprod(x[, j], x[, i]) * x[, i]
    }
  }
  
  normalise(x)
}

orthonormalise_by <- function(x, by) {
  stopifnot(ncol(x) == ncol(by))
  stopifnot(nrow(x) == nrow(by))

  x <- normalise(x)
  
  for (j in seq_len(ncol(x))) {
    x[, j] <- x[, j] - crossprod(x[, j], by[, j]) * by[, j]
  }

  normalise(x)
}


proj_dist <- function(x, y) sqrt(sum((x %*% t(x) - y %*% t(y)) ^ 2))

is_orthonormal <- function(x) {
  itis = T
  
  for (j in seq_len(ncol(x))) 
    if (sqrt(sum(x[,j]^2)) < 0.999) itis=F
  for (j in 2:ncol(x)) {
    for (i in 1:(ncol(x)-1)) {
      if (sum(x[,j]*x[,i]) > 0.01) itis=F
    }
  }

  itis

}

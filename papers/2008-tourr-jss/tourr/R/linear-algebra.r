#' Normalise a numeric matrix.
#'
#' Ensure that columns of a numeric matrix have norm 1
#' 
#' @keywords internal algebra
#' @param x numeric matrix or vector
normalise <- function(x) {
  if (is.matrix(x)) {
    lengths <- sqrt(colSums(x ^ 2, na.rm = TRUE))
    sweep(x, 2, lengths, "/")
  } else {
    x / sqrt(sum(x ^ 2))    
  }
}

#' Orthonormalise using modified Gram-Schmidt process.
#'
#' @keywords internal algebra
#' @param x numeric matrix
orthonormalise <- function(x) {
  x <- normalise(x) # to be conservative
  
  if (ncol(x) > 1) {
    for (j in seq_len(ncol(x))) {
      for (i in seq_len(j - 1)) {
        x[, j] <- x[, j] - crossprod(x[, j], x[, i]) * x[, i]
      }
    }
  }
  
  normalise(x)
}


#' Test if a numeric matrix is orthonormal.
#'
#' @keywords internal algebra
#' @param x numeric matrix
#' @param tol tolerance used to test floating point differences
is_orthonormal <- function(x, tol = 0.001) {
  stopifnot(is.matrix(x))

  for (j in seq_len(ncol(x))) {
    if (sqrt(sum(x[, j] ^ 2)) < 1 - tol) return(FALSE)
  }
  
  if (ncol(x) > 1) {
    for (j in 2:ncol(x)) {
      for (i in 1:(ncol(x) - 1)) {
        if (abs(sum(x[, j] * x[, i])) > tol) return(FALSE)
      }
    }    
  }
  
  TRUE
}

#' Orthonnormalise one matrix by another.
#'
#' This ensures that each column in x is orthogonal to the corresponding
#' column in by.
#'
#' @keywords internal algebra
#' @param x numeric matrix
#' @param by numeric matrix, same size as x
orthonormalise_by <- function(x, by) {
  stopifnot(ncol(x) == ncol(by))
  stopifnot(nrow(x) == nrow(by))

  x <- normalise(x)
  
  for (j in seq_len(ncol(x))) {
    x[, j] <- x[, j] - crossprod(x[, j], by[, j]) * by[, j]
  }

  normalise(x)
}


#' Calculate the distance between two bases.
#'
#' Computes the Frobenius norm between two bases.  This is equals to the
#' Euclidean norm of the vector of sines of principal angles between the two
#' subspaces.
# 
#' @param x projection matrix a
#' @param y projection matrix b
#' @keywords algebra
proj_dist <- function(x, y) sqrt(sum((x %*% t(x) - y %*% t(y)) ^ 2))

#' Normalise a numeric matrix.
#'
#' Ensure that columns of a numeric matrix have norm 1
#'
#' @keywords internal algebra
#' @param x numeric matrix or vector
#' @export
normalise <- function(x) {
  if (is.matrix(x)) {
    lengths <- sqrt(colSums(x^2, na.rm = TRUE))
    sweep(x, 2, lengths, "/")
  } else {
    x / sqrt(sum(x^2))
  }
}

#' Orthonormalise using modified Gram-Schmidt process.
#'
#' @keywords internal algebra
#' @param x numeric matrix
#' @export
orthonormalise <- function(x) {
  x <- normalise(x) # to be conservative

  if (ncol(x) > 1) {
    for (j in seq_len(ncol(x))) {
      for (i in seq_len(j - 1)) {
        x[, j] <- x[, j] - as.vector(crossprod(x[, j], x[, i])) * x[, i]
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
#' @export
is_orthonormal <- function(x, tol = 0.001) {
  if(is.numeric(x) == FALSE) stop("'x', expected to be numeric and coercable to matrix.")
  if (!is.matrix(x)) x <- as.matrix(x)
  #stopifnot(is.matrix(x))
  nc <- ncol(x)
  iter <- seq_len(nc)
  for (j in iter) {
    if (abs(1-sqrt(sum(x[, j]^2))) > tol) {
      return(FALSE)
    }
  }
  if (nc > 1) {
    # dot product between columns is close to zero
    for (j in iter[2:nc]) {
      rem <- setdiff(iter, j)
      for (i in rem) {
        if (abs(sum(x[, j] * x[, i])) > tol) {
          return(FALSE)
        }
      }
    }
  }
  TRUE
}

#' Orthonormalise one matrix by another.
#'
#' This ensures that each column in x is orthogonal to the corresponding
#' column in by.
#'
#' @keywords internal algebra
#' @param x numeric matrix
#' @param by numeric matrix, same size as x
#' @returns orthonormal numeric matrix
#' @export
orthonormalise_by <- function(x, by) {
  stopifnot(ncol(x) == ncol(by))
  stopifnot(nrow(x) == nrow(by))

  x <- normalise(x)
  by <- normalise(by)

  for (j in seq_len(ncol(x))) {
    for (k in seq_len(ncol(by))) {
      x[, j] <- x[, j] - as.vector(crossprod(x[, j], by[, k])) * by[, k]
      x[, j] <- normalise(x[, j])
    }
  }

  # Last step, columns new matrix to orthonormal
  if (ncol(x) > 1) {
    for (j in 2:ncol(x)) {
      x[, j] <- x[, j] - as.vector(crossprod(x[, j], x[, j-1])) * x[, j-1]
      normalise(x[, j])
    }
  }

  return(x)
}

#' Calculate the distance between two bases.
#'
#' Computes the Frobenius norm between two bases, in radians.  This is
#' equals to the Euclidean norm of the vector of principal angles between
#' the two subspaces.
#
#' @param x projection matrix a
#' @param y projection matrix b
#' @keywords algebra
#' @export
proj_dist <- function(x, y) sqrt(sum((x %*% t(x) - y %*% t(y))^2))

#' Calculate the Mahalanobis distance between points and center.
#'
#' Computes the Mahalanobis distance using a provided variance-covariance
#' matrix of observations from 0.
#
#' @param x matrix of data
#' @param vc pre-determined variance-covariance matrix
#' @keywords algebra
#' @export
mahal_dist <- function(x, vc) {
  n <- dim(x)[1]
  p <- dim(x)[2]
  mn <- rep(0, p)
  ev <- eigen(vc)
  vcinv <- ev$vectors %*% diag(1/ev$values) %*% t(ev$vectors)
  x <- x - matrix(rep(mn, n), ncol = p, byrow = T)
  dx <- NULL
  for (i in 1:n)
    dx <- c(dx, x[i, ] %*% vcinv %*% as.matrix(x[i, ]))
  return(dx)
}

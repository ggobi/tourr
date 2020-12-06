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
  stopifnot(is.matrix(x))
  nc <- ncol(x)
  iter <- seq_len(nc)
  for (j in iter) {
    if (sqrt(sum(x[, j]^2)) < 1 - tol) {
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
#' @export
orthonormalise_by <- function(x, by) {
  stopifnot(ncol(x) == ncol(by))
  stopifnot(nrow(x) == nrow(by))

  x <- normalise(x)

  for (j in seq_len(ncol(x))) {
    x[, j] <- x[, j] - as.vector(crossprod(x[, j], by[, j])) * by[, j]
  }

  normalise(x)
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

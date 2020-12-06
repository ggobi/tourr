#' Calculate the squared Euclidean norm of a vector x
#'
#' @param x numeric vector
#' @keywords internal
eucl_norm_sq <- function(x) {
  sum(x^2)
}

#' Calculate orthogonal distances
#'
#' For each datapoint this function calculates the orthogonal distance from the
#' anchored projection plane.
#'
#' @param plane matrix specifying the projection plane
#' @param data data frame or matrix
#' @param anchor A vector specifying the reference point to anchor the plane
#'   If NULL (default) the slice will be anchored at the origin.
#' @return distance vector
#' @export
anchored_orthogonal_distance <- function(plane, data, anchor = NULL) {
  # by default anchor is at the origin
  if (is.null(anchor)) {
    anchor <- matrix(colMeans(data), ncol = length(plane[, 1]))
  } else {
    anchor <- matrix(anchor, ncol = length(plane[, 1]))
  }

  # this is the squared length of alpha (orthogonal component of anchor on plane wrt the origin)
  alpha_sq <- sum(anchor^2) - sum((anchor %*% plane)^2)

  # for each datapoint calculate the squarred length of orthogonal distance from plane (anchored at zero)
  full_norm_sq <- apply(data, 1, eucl_norm_sq)
  subtr1 <- as.matrix(data) %*% plane[, 1]
  subtr2 <- as.matrix(data) %*% plane[, 2]
  dist_sq <- full_norm_sq - subtr1^2 - subtr2^2

  # finally there is a cross term that we need to take into account
  xterm <- 2 * (as.matrix(data) %*% t(anchor) -
    subtr1 * drop(anchor %*% plane[, 1]) -
    subtr2 * drop(anchor %*% plane[, 2]))

  # putting all three components together we can calculate the anchored distance
  sqrt(dist_sq + alpha_sq - xterm)
}

#' If not set, compute epsilon based on half_range
#'
#' @keywords internal
compute_v_rel <- function(v_rel, half_range, n) {
  if (!is.null(v_rel)) {
    return(v_rel)
  }

  (half_range^(n - 2)) / 10.
}

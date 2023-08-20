#' Interpolate geodesically between bases.
#'
#' This function takes a set of bases and produces a tour by geodesically
#' interpolating between each basis
#'
#' @param basis_set input basis set
#' @param angle target distance (in radians) between bases
#' @param cycle For \code{\link{planned_tour}} cycle through continuously (TRUE) or stop after first pass (FALSE)
#' @keywords hplot
#' @export
#' @examples
#' t1 <- save_history(flea[, 1:6], grand_tour(1), max = 3)
#' dim(t1)
#' dim(interpolate(t1, 0.01))
#' dim(interpolate(t1, 0.05))
#' dim(interpolate(t1, 0.1))
#' t2 <- save_history(flea[, 1:6], grand_tour(2), max = 2)
#' dim(interpolate(t2, 0.05))
interpolate <- function(basis_set, angle = 0.05, cycle = FALSE) {
  basis_set <- as.array(basis_set)
  n <- dim(basis_set)[3]
  if (n < 2) {
    return(basis_set)
  }

  record <-
    tibble::tibble(
      basis = list(),
      index_val = numeric(),
      info = character(),
      method = character(),
      alpha = numeric(),
      tries = numeric(),
      loop = numeric()
    )
  # Estimate number of bases in output
  dists <- sapply(2:n, function(i) {
#    proj_dist(basis_set[[i - 1]], basis_set[[i]])
    proj_dist(apply(basis_set[,,i - 1], 2, c), apply(basis_set[,,i], 2, c))
  })
  steps <- sum(ceiling(dists / angle)) * 2

  new_basis <- rep(NA, steps)
  new_basis[1] <- TRUE

  # Initialise result storage
  projs <- array(NA_real_, c(dim(basis_set)[1:2], steps))

  i <- 1
  tour <- new_tour(basis_set[, , 1], planned_tour(basis_set, cycle))
  step <- tour(0)
  stop_next <- FALSE # use bool to stop after adding final projection

  while (TRUE) { # need to add final projection after generator runs out, so using break instead of while condition
    new_basis[i] <- step$step <= 0
    projs[, , i] <- step$proj
    if (step$step == -1) break # this signals we have reached the last target
    i <- i + 1
    step <- tour(angle)
  }

  # Trim off extra bases
  projs <- projs[, , seq_len(i), drop = FALSE]
  new_basis <- new_basis[seq_len(i), drop = FALSE]

  attr(projs, "new_basis") <- new_basis
  attr(projs, "data") <- attr(basis_set, "data")
  class(projs) <- c("history_array", class(projs))
  projs
}

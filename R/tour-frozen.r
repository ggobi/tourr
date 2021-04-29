#' A frozen tour path.
#'
#' A frozen tour fixes some of the values of the orthonormal projection
#' matrix and allows the others to vary freely according to any of the
#' other tour methods.  This frozen tour is a frozen grand tour.  See
#' \code{\link{frozen_guided_tour}} for a frozen guided tour.
#'
#' Usually, you will not call this function directly, but will pass it to
#' a method that works with tour paths like \code{\link{animate}},
#' \code{\link{save_history}} or \code{\link{render}}.
#'
#' @param d target dimensionality
#' @param frozen matrix of frozen variables, as described in
#'   \code{\link{freeze}}
#' @export
#' @examples
#' frozen <- matrix(NA, nrow = 4, ncol = 2)
#' frozen[3, ] <- .5
#' animate_xy(flea[, 1:4], frozen_tour(2, frozen))
#'
#' frozen <- matrix(NA, nrow = 4, ncol = 2)
#' frozen[1, 1] <- 0.5
#' animate_xy(flea[, 1:4], frozen_tour(2, frozen))
#'
#' # Doesn't work - a bug?
#' frozen <- matrix(NA, nrow = 4, ncol = 2)
#' frozen[1:2, 1] <- 1 / 4
#' animate_xy(flea[, 1:4], frozen_tour(2, frozen))
#'
#' \dontrun{
#' # This freezes one entire direction which causes a problem,
#' # and is caught by error handling.
#' # If you want to do this it would be best with a dependence
#' # tour, with one variable set one axis, eg 3rd variable to
#' # x axis would be indicated from the code below
#' frozen <- matrix(NA, nrow = 4, ncol = 2)
#' frozen[3, ] <- c(0, 1)
#' animate_xy(flea[, 1:4], frozen_tour(2, frozen))
#' }
#'
#' # Two frozen variables in five.
#' frozen <- matrix(NA, nrow = 5, ncol = 2)
#' frozen[3, ] <- .5
#' frozen[4, ] <- c(-.2, .2)
#' animate_xy(flea[, 1:5], frozen_tour(2, frozen))
frozen_tour <- function(d = 2, frozen) {
  generator <- function(current, data, ...) {
    if (is.null(current)) {
      # Here is a problem if the frozen part is in the
      # initialised basis, because it gets zero'd out by freeze.
      notfrozen <- rep(TRUE, ncol(data))
      for (i in 1:ncol(data))
        if (sum(frozen[i,]^2, na.rm=TRUE)>0)
          notfrozen[i] <- FALSE
      current <- matrix(0, nrow = ncol(data), ncol = d)
      for (i in 1:d) {
        current[c(1:ncol(data))[notfrozen][i],i] <- 1
      }
      # This will still throw an error if number of frozen is bigger than d

      return(current)
    }


    target <- basis_random(ncol(data), d)
    list(target = target)
  }

  check_freezer_safe(frozen)
  new_geodesic_path("frozen", generator, frozen = frozen)
}

#' The frozen guided tour
#'
#' @param frozen matrix of frozen variables, as described in
#'   \code{\link{freeze}}
#' @param index_f the index function to optimise.
#' @param d target dimensionality
#' @param max.tries the maximum number of unsuccessful attempts to find
#'   a better projection before giving up
#' @seealso \code{\link{cmass}}, \code{\link{holes}} and \code{\link{lda_pp}}
#'   for examples of index functions.  The function should take a numeric
#'   matrix and return a single number, preferrably between 0 and 1.
#' @export
#' @examples
#' frozen <- matrix(NA, nrow = 4, ncol = 2)
#' frozen[3, ] <- .5
#' animate_xy(flea[, 1:4], frozen_guided_tour(frozen, holes()))
frozen_guided_tour <- function(frozen, index_f, d = 2, max.tries = 25) {
  generator <- function(current, data, tries, ...) {
    if (is.null(current)) {
      return(basis_init(ncol(data), d))
    }

    index <- function(proj) {
      index_f(as.matrix(data) %*% proj)
    }

    basis <- search_frozen_geodesic(current, index, tries, max.tries, frozen = frozen)
    list(target = basis$target, index = index)
  }

  check_freezer_safe(frozen)
  new_geodesic_path("frozen-guided", generator, frozen = frozen)
}

#' Check matrix is a valid frozen matrix
#'
#' @keywords internal
#' @param frozen matrix to check for freezability
check_freezer_safe <- function(frozen) {
  stopifnot(is.matrix(frozen))

  lengths <- colSums(frozen^2, na.rm = TRUE)
  if (any(lengths >= 1)) {
    stop("Columns of frozen matrix must have squared norm < 1", call. = FALSE)
  }
}

#' Freeze and thaw matrices
#'
#' Some terminology:
#'   * frozen variables: the variables that have fixed values
#'   * warm variables: the remaining variables that vary freely
#'
#' A frozen matrix specifies which variables to fix in a projection matrix.
#' Warm variables should be missing (\code{NA}) while frozen variables should
#' be set to their fixed values.
#'
#' @keywords internal
#' @export
#' @examples
#' frozen <- matrix(NA, nrow = 4, ncol = 2)
#' frozen[3, ] <- .5
#'
#' input <- basis_random(4, 2)
#' freeze(input, frozen)
#' thaw(input, frozen)
#' freeze(basis_random(4, 2), frozen)
freeze <- function(input, frozen) {
  fixed <- !is.na(frozen)
  input[fixed] <- 0
  input
}

#' @export
#' @rdname freeze
thaw <- function(input, frozen) {
  fixed <- !is.na(frozen)

  input <- normalise(input)
  frozen_lengths <- colSums(frozen^2, na.rm = TRUE)

  input <- sweep(input, 2, sqrt(1 - frozen_lengths), "*")
  input[fixed] <- frozen[fixed]

  input
}

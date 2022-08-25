#' A pseudo-derivative, line search algorithm along frozen geodesics.
#'
#' @param current starting projection
#' @param index index function
#' @param tries the counter of the outer loop of the opotimiser
#' @param max.tries maximum number of iteration before giving up
#' @param n number of random steps to take to find best direction
#' @param frozen a matrix of frozen variables
#' @param cur_index the index value of the current basis
#' @param ... other arguments being passed into the \code{search_frozen_geodesic()}
#' These three functions perform a corresponding role to
#' \code{\link{search_geodesic}}, \code{\link{find_best_dir}} and
#' \code{\link{find_path_peak}} but for the frozen tour.  They work by
#' zero'ing out the frozen variables and travelling in that restricted
#' subspace.
#'
#' @section To do: eliminate these functions
#' @keywords internal
search_frozen_geodesic <- function(current, index, tries, max.tries = 5, n = 5, frozen, cur_index = NA, ...) {
  cur_index <- index(thaw(current, frozen))

  try <- 1
  while (try < max.tries) {
    # Try 5 random directions and pick the one that has the highest
    # index after a small step in either direction
    direction <- find_best_frozen_dir(current, frozen, index, n)
    best_dir <- direction$basis[[which.max(direction$index_val)]]
    direction_search <- dplyr::mutate(direction, tries = tries, loop = try)
    # Travel halfway round (pi / 4 radians) the sphere in that direction
    # looking for the best projection
    peak <- find_frozen_path_peak(current, best_dir, frozen, index)
    line_search <- dplyr::mutate(peak$best, tries = tries, loop = try)
    new_index <- tail(line_search$index_val, 1)
    new_basis <- tail(line_search$basis, 1)

    rcd_env <- parent.frame(n = 4)
    if (is.null(rcd_env[["record"]])) rcd_env <- parent.frame(n = 1)
    rcd_env[["record"]] <- dplyr::bind_rows(
      rcd_env[["record"]],
      direction_search,
      line_search
    )

    dig3 <- function(x) sprintf("%.3f", x)
    pdiff <- (new_index - cur_index) / cur_index
    if (pdiff > 0.001) {
      cat("New index: ", dig3(new_index), " (", dig3(peak$alpha$maximum), " away)\n", sep = "")
      current <<- new_basis
      cur_index <<- new_index

      return(list(target = new_basis[[1]]))
    }
    cat("Best was:  ", dig3(new_index), " (", dig3(peak$alpha$maximum), " away).  Trying again...\n", sep = "")

    try <- try + 1
  }

  NULL
}

#' Find most promising direction in frozen space.
#' @keywords internal
find_best_frozen_dir <- function(old, frozen, index, dist = 0.01, counter = 5) {
  new_basis <- function() freeze(basis_random(nrow(old), ncol(old)), frozen)
  bases <- replicate(counter, new_basis(), simplify = FALSE)
  old <- freeze(old, frozen)

  score <- function(new) {
    interpolator <- geodesic_info(old, new)
    forward <- thaw(step_angle(interpolator, dist), frozen)
    backward <- thaw(step_angle(interpolator, -dist), frozen)

    larger <- max(index(forward), index(backward))

    tibble::tibble(
      basis = c(list(forward), list(backward)),
      index_val = c(index(forward), index(backward)),
      info = "direction_search",
      method = "search_frozen_geodesic"
    )
  }

  ans <- dplyr::bind_rows(lapply(bases, score))
  ans[which.max(ans$index_val), "info"] <- "best_direction_search"
  ans
}

#' Find most highest peak along frozen geodesic.
#' @keywords internal
find_frozen_path_peak <- function(old, new, frozen, index, max_dist = pi / 4) {
  interpolator <- geodesic_info(freeze(old, frozen), freeze(new, frozen))

  index_pos <- function(alpha) {
    index(thaw(step_angle(interpolator, alpha), frozen))
  }

  alpha <- stats::optimize(index_pos, c(-max_dist, max_dist), maximum = TRUE, tol = 0.01)

  best <- dplyr::tibble(
    basis = list(thaw(step_angle(interpolator, alpha$maximum), frozen)),
    index_val = alpha$objective,
    info = "best_line_search",
    method = "search_frozen_geodesic"
  )

  # xgrid <- seq(-max_dist, max_dist, length = 100)
  # index <- sapply(xgrid, index_pos)
  # plot(xgrid, index, type = "l")

  list(best = best, alpha = alpha)
}

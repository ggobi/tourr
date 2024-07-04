#' A guided section tour path.
#'
#' The guided section tour is a variation of the guided tour that is
#' using a section pursuit index for the selection of target planes.
#'
#' Usually, you will not call this function directly, but will pass it to
#' a method that works with tour paths like \code{\link{animate_slice}},
#' \code{\link{save_history}} or \code{\link{render}}.
#'
#' @param index_f the section pursuit index function to optimise. The function
#'   needs to take three arguments, the projected data, the vector of distances
#'   from the current projection plane, and the slice thickness h.
#' @param d target dimensionality
#' @param alpha the initial size of the search window, in radians
#' @param cooling the amount the size of the search window should be adjusted
#'   by after each step
#' @param search_f the search strategy to use
#' @param max.tries the maximum number of unsuccessful attempts to find
#'   a better projection before giving up
#' @param max.i the maximum index value, stop search if a larger value is found
#' @param v_rel relative volume of the slice. If not set, suggested value
#'   is calculated and printed to the screen.
#' @param anchor A vector specifying the reference point to anchor the slice.
#'   If NULL (default) the slice will be anchored at the data center.
#' @param ... arguments sent to the search_f
#' @seealso \code{\link{slice_index}} for an example of an index functions.
#' \code{\link{search_geodesic}}, \code{\link{search_better}},
#'   \code{\link{search_better_random}} for different search strategies
#' @export
#' @examples
#' # Generate samples on a 3d hollow sphere using the geozoo package
#' set.seed(12345)
#' sphere3 <- geozoo::sphere.hollow(3)$points
#' # Columns need to be named before launching the tour
#' colnames(sphere3) <- c("x1", "x2", "x3")
#' # Off-center anchoring
#' anchor3 <- matrix(rep(0.75, 3), ncol=3)
#' # Index setup
#' r_breaks <- linear_breaks(5, 0, 1)
#' a_breaks <- angular_breaks(10)
#' eps <- estimate_eps(nrow(sphere3), ncol(sphere3), 0.1 / 1, 5 * 10, 10, r_breaks)
#' idx <- slice_index(r_breaks, a_breaks, eps, bintype = "polar", power = 1, reweight = TRUE, p = 3)
#' # Running the guided section tour select sections showing a big hole in the center
#' animate_slice(sphere3, guided_section_tour(idx, v_rel = 0.1, anchor = anchor3, max.tries = 5),
#'   v_rel = 0.1, anchor = anchor3
#' )
guided_section_tour <- function(index_f, d = 2, alpha = 0.5, cooling = 0.99,
                                max.tries = 25, max.i = Inf, v_rel = NULL, anchor = NULL,
                                search_f = search_geodesic, ...) {
  h <- NULL

  generator <- function(current, data, tries, ...) {
    if (is.null(current)) {
      return(basis_init(ncol(data), d))
    }

    if (is.null(h)) {
      half_range <- compute_half_range(NULL, data, FALSE)
      v_rel <- compute_v_rel(v_rel, half_range, ncol(data))
      h <<- v_rel^(1 / (ncol(data) - 2))
    }

    index <- function(proj) {
      dist_data <- anchored_orthogonal_distance(proj, data, anchor)
      index_f(as.matrix(data) %*% proj, dist_data, h)
    }

    cur_index <- index(current)

    if (cur_index > max.i) {
      cat("Found index ", cur_index, ", larger than selected maximum ", max.i, ". Stopping search.\n",
        sep = ""
      )
      cat("Final projection: \n")
      if (ncol(current) == 1) {
        for (i in 1:length(current)) {
          cat(sprintf("%.3f", current[i]), " ")
        }
        cat("\n")
      }
      else {
        for (i in 1:nrow(current)) {
          for (j in 1:ncol(current)) {
            cat(sprintf("%.3f", current[i, j]), " ")
          }
          cat("\n")
        }
      }
      return(NULL)
    }

    basis <- search_f(current, alpha, index, tries, max.tries, cur_index = cur_index, ...)
    alpha <<- alpha * cooling

    list(target = basis$target, index = index)
  }

  new_geodesic_path("guided", generator)
}

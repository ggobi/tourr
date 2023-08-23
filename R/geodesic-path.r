#' Generate a geodesic path between bases supplied by generator
#'
#' A tour path is a function that when called with the current projection
#' and data set, generates sequence of \code{\link{geodesic_path}}s.  The
#' path can either span the whole space of orthonormal matrices, the default
#' or be restricted to a subspace with the frozen argument.  More details
#' are given in the documentation for \code{\link{freeze}}.
#'
#' Subsequent frames are guaranteed to be at least 0.001 radians away from
#' the current frame.  If after 10 tries the generator does not give a new
#' basis at least this far away then we give up.
#'
#' If a suitable new basis can not be found, the path function returns NULL
#' indicating that the tour should stop.
#'
#' @param name name to give tour path
#' @param generator basis generator function
#' @param frozen matrix giving frozen variables, as described in
#'   \code{\link{freeze}}
#' @export
#' @keywords internal
new_geodesic_path <- function(name, generator, frozen = NULL, ...) {
  tries <- 1 # Needed for guided

  tour_path <- function(current, data, ...) {
    if (is.null(current)) {
      return(generator(NULL, data, tries, ...))
    }

    # Keep trying until we get a frame that's not too close to the
    # current frame
    dist <- 0
    while (dist < 1e-3) {
      if (name %in% c("guided", "frozen-guided")) tries <<- tries + 1

      gen <- generator(current, data, tries, ...)
      target <- gen$target

      # generator has run out, so give up
      if (is.null(target)) {
        return(NULL)
      }

      dist <- proj_dist(current, target)
      if (dist < 1e-2) {
        return(NULL)
      }

      #cat("generation:  dist =  ", dist, "\n")
    }
    list(ingred = geodesic_path(current, target, frozen, ...), index = gen$index, tries = tries)
  }

  structure(
    tour_path,
    name = name,
    class = c("tour_path", "function")
  )
}


#' @export
"print.tour_path" <- function(x, ...) {
  cat("Tour path:", attr(x, "name"), "\n")

  # params <- as.list(environment(get("generator", environment(g))))
  # str(params)
}

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
#' @param generate basis generator function
#' @param frozen matrix giving frozen variables, as described in
#'   \code{\link{freeze}}
#' @export
#' @keywords internal
new_geodesic_path <- function(name, generator, frozen = NULL, ...) {
  tour_path <- function(current, data, ...) {
    if (is.null(current)) {
      if (name == "guided") tries <<- 0
      return(generator(NULL, data, ...))
    }

    # Keep trying until we get a frame that's not too close to the
    # current frame
    dist <- 0
    while (dist < 1e-3) {

      if(name == "guided") tries <<- tries + 1

      gen <- generator(current, data, ...)
      target <- gen$target

      if ("polish_alpha" %in% gen$arg){
        if (verbose) return(record)
      }

      # generator has run out, so give up
      if (is.null(target)) return(NULL)

      # give up, generator produced 10 equivalent frames in a row

      if (name == "guided") if (tries > 500) return(NULL)

      dist <- proj_dist(current, target)

      if (verbose) cat("generation:  dist =  ", dist, "\n")

    }
    geo <- geodesic_path(current, target, frozen, ...)

    if (verbose)
      list(geo = geo, record = record)
    else
      list(geo = geo)
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

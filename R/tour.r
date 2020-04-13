#' Create a new tour.
#'
#' The tour function provides the common machinery behind all tour methods:
#' interpolating from basis to basis, and generating new bases when necessary.
#' You should not have to call this function.
#'
#' @param data the data matrix to be projected
#' @param tour_path basis generator, a function that generates a new basis,
#'   called with the previous projection and the data set.  For more
#'   complicated tour paths, this will need to be a closure with local
#'   variables.  Should return NULL if the tour should terminate
#' @param start starting projection, if omitted will use default projection
#'   from generator
#' @seealso \code{\link{save_history}}, \code{\link{render}} and
#'   \code{\link{animate}} for examples of functions that use this function
#'   to run dynamic tours.
#' @keywords hplot dynamic internal
#' @return a function with single argument, step_size.  This function returns
#'  a list containing the new projection, the currect target and the number
#'  of steps taken towards the target.
#' @export
new_tour <- function(data, tour_path, start = NULL, ...) {
  stopifnot(inherits(tour_path, "tour_path"))

  if (is.null(start)) {
    start <- tour_path(NULL, data, ...)
  }
  proj <- start

  # Initialise first step
  target <- NULL
  step <- 0

  cur_dist <- 0
  target_dist <- 0
  geodesic <- NULL

  function(step_size, verbose = FALSE) {
    #browser()

    if(verbose) cat("target_dist - cur_dist:", target_dist - cur_dist,  "\n")

    step <<- step + 1
    cur_dist <<- cur_dist + step_size

    if (target_dist == 0 & step > 1){ # should only happen for guided tour when no better basis is found (relative to starting plane)
      return(list(proj = proj, target = target, step = -1)) #use negative step size to signal that we have reached the final target
    }
    # We're at (or past) the target, so generate a new one and reset counters
    if (step_size > 0 & is.finite(step_size) & cur_dist >= target_dist) {
      proj <<- geodesic$interpolate(1.) #make sure next starting plane is previous target
    }

    if (cur_dist >= target_dist) {

      # add the interruption here!

      tour_path_obj <<- tour_path(proj, data, ...)
      geodesic <<- tour_path_obj[["geo"]]

      if (is.null(geodesic)) {
        return(list(proj = proj, target = target, step = -1)) #use negative step size to signal that we have reached the final target
      }

      target_dist <<- geodesic$dist
      target <<- geodesic$Fz
      cur_dist <<- 0
      # Only exception is if the step_size is infinite - we want to jump
      # to the target straight away
      if (!is.finite(step_size)) {
        cur_dist <<- target_dist
      }

      step <<- 0
    }

    proj <<- geodesic$interpolate(cur_dist / target_dist)

    record <- record %>% add_row(basis = list(proj),
                                 index_val = index(proj),
                                 info = "interpolation",
                                 tries = tries) %>%
      mutate(id = row_number())

    list(proj = proj, target = target, step = step, record = record)
  }
}

#' @importFrom grDevices dev.cur dev.flush dev.hold dev.off hcl rgb
#' @importFrom graphics abline axis box hist image lines pairs par plot points
#'   polygon rect segments stars text
NULL

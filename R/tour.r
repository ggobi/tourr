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
#'  a list containing the new projection, the current target and the number
#'  of steps taken towards the target.
#' @details
#'
#' If you are intended to call \code{new_tour()} from the global environment, try \code{save_history()} and then animate with a \code{planned_tour()}. See \code{\link[tourr]{save_history}} for examples on this.
#'
#' @export
new_tour <- function(data, tour_path, start = NULL, ...) {
  stopifnot(inherits(tour_path, "tour_path"))

  if (is.null(start)) {
    start <- tour_path(NULL, data, ...)
  }

  proj <- list()
  proj[[1]] <- start

  # Initialise first step
  target <- NULL
  step <- 0

  cur_dist <- 0
  target_dist <- 0
  geodesic <- NULL

  function(step_size, ...) {
    # cat("target_dist - cur_dist:", target_dist - cur_dist, "\n")

    step <<- step + 1
    cur_dist <<- cur_dist + step_size

    if (target_dist == 0 & step > 1) { # should only happen for guided tour when no better basis is found (relative to starting plane)
      return(list(proj = tail(proj, 1)[[1]], target = target, step = -1)) # use negative step size to signal that we have reached the final target
    }
    # We're at (or past) the target, so generate a new one and reset counters
    if (step_size > 0 & is.finite(step_size) & cur_dist >= target_dist) {

      ## interrupt
      if (attr(tour_path, "name") == "guided") {
        rcd_env <- parent.frame()
        if ("new_basis" %in% rcd_env[["record"]]$info & !rcd_env[["record"]]$method[2] %in% c("search_geodesic", "search_polish")) {
          last_two <- tail(dplyr::filter(rcd_env[["record"]], info == "new_basis"), 2)

          if (last_two$index_val[1] > last_two$index_val[2]) {
          # search_better_random may give probabilistic acceptance, leave it as it is
          } else {
            interp <- dplyr::filter(rcd_env[["record"]], tries == max(tries), info == "interpolation")
            interp <- dplyr::filter(interp, index_val == max(index_val))

            target <- dplyr::filter(rcd_env[["record"]], tries == max(tries), info == "new_basis")

          # deem the target basis as the new current basis if the interpolation doesn't reach the target basis
          # used when the index_f is not smooth
            if (target$index_val > interp$index_val) {
              proj[[length(proj) + 1]] <<- geodesic$ingred$interpolate(1.) # make sure next starting plane is previous target
              target <- dplyr::mutate(target, info = "interpolation", loop = step + 1, alpha = NA)
              rcd_env[["record"]] <- dplyr::add_row(rcd_env[["record"]], target)
            } else if (target$index_val < interp$index_val & nrow(interp) != 0) {
            # the interrupt
              proj[[length(proj) + 1]] <<- interp$basis[[1]]

              rcd_env[["record"]] <- dplyr::filter(
                rcd_env[["record"]],
                id <= which(rcd_env[["record"]]$index_val == interp$index_val)
              )
            }
          }
        } else {
          proj[[length(proj) + 1]] <<- geodesic$ingred$interpolate(1.)
          if (nrow(rcd_env[["record"]]) != 0){
            rcd_env[["record"]] <- dplyr::add_row(
              rcd_env[["record"]],
              basis = list(proj[[length(proj)]]),
              index_val = geodesic$index(proj[[length(proj)]]),
              info = "interpolation",
              tries = geodesic$tries,
              method = dplyr::last(rcd_env[["record"]]$method),
             loop = step + 1
            )

            rcd_env[["record"]] <- dplyr::mutate(
              rcd_env[["record"]],
              id = dplyr::row_number()
            )
          }
        }
      }
      else {
        proj[[length(proj) + 1]] <<- geodesic$ingred$interpolate(1)
      }
    }

    if (cur_dist >= target_dist) {
      geodesic <<- tour_path(proj[[length(proj)]], data, ...)
      if (is.null(geodesic$ingred)) {
        return(list(proj = proj[[length(proj)]], target = target, step = -1)) # use negative step size to signal that we have reached the final target
      }

      target_dist <<- geodesic$ingred$dist
      target <<- geodesic$ingred$Fz
      cur_dist <<- 0
      # Only exception is if the step_size is infinite - we want to jump
      # to the target straight away
      if (!is.finite(step_size)) {
        cur_dist <<- target_dist
      }

      step <<- 0
      proj <<- list()
      proj[[1]] <<- start
    }

    proj[[step + 2]] <<- geodesic$ingred$interpolate(cur_dist / target_dist)


    if (attr(tour_path, "name") == "guided") {
      rcd_env <- parent.frame()
      rcd_env[["record"]] <- dplyr::add_row(
        rcd_env[["record"]],
        basis = list(proj[[step + 2]]),
        index_val = geodesic$index(proj[[step + 2]]),
        info = "interpolation",
        tries = geodesic$tries,
        method = dplyr::last(rcd_env[["record"]]$method),
        loop = step + 1
      )
      rcd_env[["record"]] <- dplyr::mutate(
        rcd_env[["record"]],
        id = dplyr::row_number()
      )
    }


    list(proj = proj[[length(proj)]], target = target, step = step)
  }
}
# globalVariables(c("basis", "id", "index", "index_val"))

#' @importFrom grDevices dev.cur dev.flush dev.hold dev.off hcl rgb
#' @importFrom graphics abline axis box hist image lines pairs par plot points
#'   polygon rect segments stars text arrows
NULL

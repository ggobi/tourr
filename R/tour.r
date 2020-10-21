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
#' @export
new_tour <- function(data, tour_path, start = NULL, ...) {

  stopifnot(inherits(tour_path, "tour_path"))

  if (is.null(start)) {
    start <- tour_path(NULL, data, ...)
  }

  if (attr(tour_path, "name") == "guided"){
    if (getOption("tourr.verbose", default = FALSE) & is.null(record))
      record <<- dplyr::tibble(basis = list(start),
                       index_val = index(start),
                       tries = 1,
                       info = "new_basis",
                       loop = NA)
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

    if (getOption("tourr.verbose", default = FALSE)) cat("target_dist - cur_dist:", target_dist - cur_dist,  "\n")

    step <<- step + 1
    cur_dist <<- cur_dist + step_size

    if (target_dist == 0 & step > 1){ # should only happen for guided tour when no better basis is found (relative to starting plane)
      return(list(proj = proj[[-1]], target = target, step = -1)) #use negative step size to signal that we have reached the final target
    }
    # We're at (or past) the target, so generate a new one and reset counters
    if (step_size > 0 & is.finite(step_size) & cur_dist >= target_dist) {

      ## interrupt
      if (getOption("tourr.verbose", default = FALSE)){
        if ("new_basis" %in% record$info & record$method[2] != "search_geodesic"){

          last_two <- record %>% dplyr::filter(info == "new_basis") %>% utils::tail(2)

          if (last_two$index_val[1] > last_two$index_val[2]){
            # search_better_random may give probalistic acceptance
            current <<- last_two$basis[[2]]
            cur_index <<- last_two$index_val[[2]]
          }else{
            interp <- record %>%
              dplyr::filter(tries == max(tries), info == "interpolation") %>%
              dplyr::filter(index_val == max(index_val))

            target <- record %>%
              dplyr::filter(tries == max(tries), info == "new_basis")

            # deem the target basis as the new current basis if the interpolation doesn't reach the target basis
            # used when the index_f is not smooth
            if (target$index_val > interp$index_val) {
              proj[[length(proj) +1]] <<- geodesic$interpolate(1.) #make sure next starting plane is previous target

              record <<- record %>% dplyr::add_row(target %>% dplyr::mutate(info = "interpolation", loop = step))
              current <<- record %>% tail(1) %>% dplyr::pull(basis) %>% .[[1]]
              cur_index <<- record %>% tail(1) %>% dplyr::pull(index_val)

            } else if (target$index_val < interp$index_val & nrow(interp) != 0){
              # the interrupt
              proj[[length(proj) +1]] <<- interp$basis[[1]]

              record <<- record %>% dplyr::filter(id <= which(record$index_val == interp$index_val))
              current <<- record %>% tail(1) %>% dplyr::pull(basis) %>% .[[1]]
              cur_index <<- record %>% tail(1) %>% dplyr::pull(index_val)
            }
          }
        }
      } else{
        if(exists("index")){
          index_val <- vapply(proj, index, numeric(1))
          current <<- proj[[which.max(index_val)]]
          cur_index <<- max(index_val)
          proj[[length(proj) + 1]] <<- geodesic$interpolate(1.)
        }
        proj[[length(proj) + 1]] <<- geodesic$interpolate(1.)
      }
    }

    if (cur_dist >= target_dist) {
      geodesic <<- tour_path(proj[[length(proj)]], data, ...)
      if (is.null(geodesic)) {
        return(list(proj = proj[[length(proj)]], target = target, step = -1)) #use negative step size to signal that we have reached the final target
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
      proj <<- list(); proj[[1]] <<- start
    }

    proj[[step + 2]] <<- geodesic$interpolate(cur_dist / target_dist)
    #cat("update: step = ", step + 2, "proj = ", proj [[step + 2]], "\n")

    if (getOption("tourr.verbose", default = FALSE)) {
      record <<- record %>% dplyr::add_row(basis = list(proj[[step +2]]),
                                 index_val = index(proj[[step + 2]]),
                                 info = "interpolation",
                                 tries = tries,
                                 method = dplyr::last(record$method),
                                 loop = step + 1) %>% # start the counter for loop from 1
        dplyr::mutate(id = dplyr::row_number())
    }


    list(proj = proj[[length(proj)]], target = target, step = step)
  }
}
globalVariables(c("basis", "id", "index", "index_val"))

#' @importFrom grDevices dev.cur dev.flush dev.hold dev.off hcl rgb
#' @importFrom graphics abline axis box hist image lines pairs par plot points
#'   polygon rect segments stars text
NULL

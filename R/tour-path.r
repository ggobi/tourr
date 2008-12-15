new_tour_path <- function(name, generator) {
  structure(
    geodesic_path(generator),
    name = name,
    class = "tour-path"
  )
}

"print.tour-path" <- function(x, ...) {
  cat("Tour path:", attr(x, "name"), "\n")
  # 
  # params <- as.list(environment(x))
  # str(x)
}

#' Generate geodesic path.
#'
#' Wrap basis generation method with a function that keeps computes the 
#' geodesic interpolation from the previous frame to the next frame, and
#' provides convenient access to all the information about the path.
#'
#' @param new_target_f function that generates new frame, with previous 
#'   frame as argument
#' @keywords internal
#' @return
#'   \item{frame}{The newly generated frame}
#'   \item{frame_prev}{The previous frame}
#'   \item{interpolate}{A wrapper around \code{\link{step_fraction} with the
#'     geodesic set.  It takes a single number between 0 and 1}}
#'   \item{dist}{The distance, radians, between the previous and current
#'     frame}
#'  \item{tau}{The principle angles}
#'  \item{Ga}{The starting plane}
#'  \item{Gz}{The target plane}
geodesic_path <- function(generator) { 
  function(previous, data) {
    # Initialisation
    if (is.null(previous)) {
      return(generator(previous, data))
    }
    
    dist <- 0
    # Keep trying until we get a target that's not equivalent to the previous
    while (dist < 1e-3) {
      frame <- generator(previous, data)
      if (is.null(frame)) return(NULL)
      interpolator <- geodesic(previous, frame)
      dist <- sqrt(sum(interpolator$tau ^ 2))      
    }

    # cat(interpolator$tau[1],"\n")
    list(
      frame = frame,
      frame_prev = previous,
      interpolate = function(pos) step_fraction(interpolator, pos),
      dist = dist,
      tau = interpolator$tau,
      Ga = interpolator$Ga,
      Gz = interpolator$Gz
    )
  }
}

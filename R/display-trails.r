#' Display tour path with trails
#'
#' Animate a 2D tour path with a point trails
#'
#' @param axes position of the axes: center, bottomleft or off
#' @param center if TRUE, centers projected data to (0,0).  This pins the
#'  center of data cloud and make it easier to focus on the changing shape
#'  rather than position.
#' @param half_range half range to use when calculating limits of projected.
#'   If not set, defaults to maximum distance from origin to each row of data.
#' @param col color to be plotted.  Defaults to "black"
#' @param pch shape of the point to be plotted.  Defaults to 20.
#' @param cex size of the point to be plotted.  Defaults to 1.
#' @param past draw line between current projection and projection \code{past}
#'   steps ago
#' @param cex magnification of plotting text relative to default. Defaults to 1.
#' @param ...  other arguments passed on to \code{\link{animate}} and
#'   \code{\link{display_xy}}
#' @export
#' @examples
#' animate_trails(flea[,1:6], col=flea$species)
#'
display_trails <- function(center = TRUE, axes = "center", half_range = NULL, col = "black", pch = 20, cex = 1, past = 3, ...) {

  # Inherit most behaviour from display_xy.  This is a little hacky, but
  # the only way until tourr switch to a proper object system.
  xy <- display_xy(
    center = center, axes = axes, half_range = half_range,
    col = col, pch = pch, cex = cex, ...
  )
  xy_env <- environment(xy$init)

  xy_env$past <- past
  xy_env$past_x <- vector("list", past)

  # Only difference is the display method
  render_data <- function(data, proj, geodesic) {
    draw_tour_axes(proj, labels, 1, axes)

    x <- data %*% proj
    if (center) x <- center(x)
    x <- x / half_range

    # Render projected points
    last_x <- past_x[[1]]
    if (!is.null(last_x)) {
      segments(last_x[, 1], last_x[, 2], x[, 1], x[, 2], col = col, lwd = cex*2)
    }
    points(x, col = col, pch = pch, cex = cex)

    past_x <<- c(past_x[2:past], list(x))
  }
  environment(render_data) <- xy_env
  # Quiet R CMD check note:
  past_x <- NULL

  xy$render_data <- render_data
  xy
}

# globalVariables("past_x")

#' @rdname display_trails
#' @inheritParams animate
#' @export
animate_trails <- function(data, tour_path = grand_tour(), ...) {
  animate(data, tour_path, display_trails(...), ...)
}

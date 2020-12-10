#' Draw anaglyphs with base graphics.
#'
#' @param d3 3d numeric matrix giving position of points
#' @param blue blue colour (for right eye)
#' @param red red colour (for left eye)
#' @param cex size of the point to be plotted.  Defaults to 1.
#' @keywords internal
anaglyph <- function(d3, blue, red, cex = 1) {
  d2 <- project3d(d3)

  with(d2, points(right, y, col = blue, pch = 20, cex = cex))
  with(d2, points(left, y, col = red, pch = 20, cex = cex))
}

#' Stereographic projection
#'
#'  Math from http://dogfeathers.com/java/3dproj.html
#'
#' @keywords internal
#' @param d3 3d numeric matrix giving position of points
#' @param length width of plot, mm
#' @param z0 distance from eye to screen, mm
#' @param d half interpupilary distance, mm
project3d <- function(d3, length = par("din")[1] * 25.4, z0 = 300, d = 30) {
  length <- length * 0.3
  x <- d3[, 1] * length
  y <- d3[, 2] * length
  # Squash z dimension a bit more
  z <- (1.5 + d3[, 3]) * length / 2

  d2 <- data.frame(
    left =  (z0 * x - z * d) / (z0 - z),
    right = (z0 * x + z * d) / (z0 - z),
    y =     (z0 * y) / (z0 - z)
  ) / length * 0.5
}


#' Anaglpyh tour path animation.
#'
#' Uses red-blue anaglyphs to display a 3d tour path.  You'll need some red-
#' blue glasses to get much out of this displays!
#'
#' @param blue blue colour (for right eye)
#' @param red red colour (for left eye)
#' @param cex size of the point to be plotted.  Defaults to 1.
#' @param ... other arguments passed on to \code{\link{animate}}
#' @keywords hplot
#' @export
#' @examples
#' animate_stereo(flea[, 1:6])
display_stereo <- function(blue, red, cex = 1, ...) {
  labels <- NULL
  init <- function(data, ...) {
    labels <<- abbreviate(colnames(data), 2)
  }
  render_frame <- function() {
    par(pty = "s", bg = "grey85")
    blank_plot(xlim = c(-1, 1), ylim = c(-1, 1))
  }
  render_transition <- function() {
    # rect(-1, -1, 1, 1, col="#D9D9D9E6", border=NA)
  }
  render_data <- function(data, proj, geodesic) {
    render_frame()
    anaglyph(data %*% proj, blue, red, cex = cex)

    axes <- project3d(proj)
    with(axes, {
      segments(0, 0, right, y, col = blue)
      segments(0, 0, left, y, col = red)
      text(right, y, col = blue, label = labels)
      text(left, y, col = red, label = labels)
    })
  }



  list(
    init = init,
    render_frame = render_frame,
    render_transition = render_transition,
    render_data = render_data,
    render_target = nul
  )
}


#' @rdname display_stereo
#' @inheritParams animate
#' @export
animate_stereo <- function(data, tour_path = grand_tour(3), blue = rgb(0, 0.91, 0.89), red = rgb(0.98, 0.052, 0), ...) {
  animate(
    data = data, tour_path = tour_path,
    display = display_stereo(blue, red, ...),
    ...
  )
}

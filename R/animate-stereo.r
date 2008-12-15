#' Anaglpyh tour path animation.
#'
#' @examples
#' animate_stereo(flea[, 1:6])
animate_stereo <- function(data, tour_path = grand_tour(3), ...) {
  blue <- rgb(0, 0.91, 0.89)
  red <- rgb(0.98, 0.052, 0)
  green <- "green2"
  
  labels <- abbreviate(colnames(data), 2)
  
  render_frame <- function() {
    par(pty = "s", bg = "grey85")
    blank_plot(xlim = c(-1, 1), ylim = c(-1, 1))    
  }
  render_transition <- function() {
    rect(-1, -1, 1, 1, col="#D9D9D9E6", border=NA)
  }
  render_data <- function(data, proj, geodesic) {
    anaglyph(data %*% proj)
    
    axes <- project3d(proj)
    with(axes, {
      segments(0, 0, right, y, col=blue)
      segments(0, 0, left, y, col=red)
      text(right, y, col=blue, label = labels)
      text(left, y, col=red, label = labels)
    })
  }

  animate(
    data = data, tour_path = tour_path,
    render_frame = render_frame, render_data = render_data,
    render_transition = render_transition, ...
  )
}

anaglyph <- function(d3, ...) {
  d2 <- project3d(d3)

  with(d2, points(right, y, col=blue, pch = 20))
  with(d2, points(left, y, col=red, pch=20))
}

# Math from http://dogfeathers.com/java/3dproj.html
# z0 = distance from eye to screen, mm
# d = half interpupilary distance, mm
project3d <- function(d3, length = par("din")[1] * 25.4, z0 = 300, d = 30) {
  length <- length * 0.3
  x <- d3[, 1] * length
  y <- d3[, 2] * length
  # Squash z dimension a bit more
  z <- (1.5 + d3[, 3]) * length / 2
    
  d2 <- data.frame(
    left =  (z0 * x - z * d) / (z0 - z), 
    right = (z0 * x + z * d) / (z0 - z),
    y =     (z0 * y)         / (z0 - z)
  ) / length * 0.5
}

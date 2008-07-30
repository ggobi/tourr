# Math from http://dogfeathers.com/java/3dproj.html
# z0 = distance from eye to screen, mm
# d = half interpupilary distance, mm
anaglyph <- function(d3, length = par("din")[1] * 25.4, z0 = 300, d = 30) {
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

  with(d2, points(right, y, pch=20, col="green2"))
  with(d2, points(left, y, pch=20, col="red"))
}

animate_stereo <- function(data, tourf = grand_tour, ...) {
  render_frame <- function() {
    par(pch = "s", bg = "grey80")
    blank_plot(xlim = c(-1, 1), ylim = c(-1, 1))    
  }
  render_transition <- function() {
    rect(-1, -1, 1, 1, col="#CCCCCCE6", border=NA)
  }
  render_data <- function(data, proj) {
    anaglyph(data %*% proj)
  }

  animate(
    data = data, tourf = tourf, d = 3, 
    render_frame = render_frame, render_data = render_data,
    render_transition = render_transition, render_target = nul, 
    ...
  )
}


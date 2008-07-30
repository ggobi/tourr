#X r_tour(mtcars[, 1:5])
#X r_tour(mtcars[, 1:5], little_tour)
animate_xy <- function(data, tourf = grand_tour, ...) {
  labels <- abbreviate(colnames(data), 2)
  
  render_frame <- function() {
    par(pch = "s")
    blank_plot(xlim = c(-2, 2), ylim = c(-2, 2))    
  }
  render_transition <- function() {
    rect(-1.99, -1.99, 1.99, 1.99, col="#FFFFFFE6", border=NA)
  }
  render_data <- function(data, proj) {
    # Render axes
    segments(0, 0, proj[, 1], proj[, 2], col="grey50")
    theta <- seq(0, 2 * pi, length = 50)
    lines(cos(theta), sin(theta), col="grey50")
    text(proj, label = labels, col="grey50")

    # Render projected points
    points(data %*% proj, pch=20)
  }
  render_target <- function(target) {
    rect(-1.99, -1.99, 1.99, 1.99, col="#7F7F7F33", border=NA)
  }

  animate(
    data = data, tourf = tourf, d = 2, 
    render_frame = render_frame, render_data = render_data,
    render_transition = render_transition, render_target = render_target, 
    ...
  )
}
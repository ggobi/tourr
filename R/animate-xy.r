#' Scatterplot tour path animation.
#'
#' Animate a 2D tour path with a scatterplot.
#'
#' @param data matrix, or data frame containing numeric columns
#' @param tour_path tour path, defaults to the grand tour
#' @param center if TRUE, centers projected data to (0,0).  This pins the 
#'  center of data cloud and make it easier to focus on the changing shape
#'  rather than position.
#' @param axes position of the axes: center, bottomleft or off
#' @param ... other arguments passed on to \code{\link{animate}}
#'
#' @examples
#' animate_xy(flea[, 1:6])
#' animate_xy(flea[, 1:6], little_tour())
#' animate_xy(flea[, 1:3], guided_tour(holes))
#' animate_xy(flea[, 1:6], center = FALSE)
#'
#' # The default axes are centered, like a biplot, but there are other options
#' animate_xy(flea[, 1:6], axes = "bottomleft")
#' animate_xy(flea[, 1:6], axes = "off")
#' animate_xy(flea[, 1:6], independent_tour(c(1, 2, 1, 2, 1, 2)),
#'   axes = "bottomleft")
animate_xy <- function(data, tour_path = grand_tour(), center = TRUE, axes = "center", ...) {
  labels <- abbreviate(colnames(data), 2)
  axes <- match.arg(axes, c("center", "bottomleft", "off"))
  
  render_frame <- function() {
    par(pty = "s", mar = rep(1,4))
    blank_plot(xlim = c(-2, 2), ylim = c(-2, 2))    
  }
  render_transition <- function() {
    # rect(-1.99, -1.99, 1.99, 1.99, col="#FFFFFFE6", border=NA)
  }
  render_data <- function(data, proj, geodesic) {
    render_frame()
    # Render axes
    if (axes == "center") {
      segments(0, 0, proj[, 1], proj[, 2], col="grey50")
      theta <- seq(0, 2 * pi, length = 50)
      lines(cos(theta), sin(theta), col="grey50")
      text(proj, label = labels, col="grey50")
    } else if (axes == "bottomleft") {
      segments(-1.25, -1.25, -1.25+proj[, 1]/2, -1.25+proj[, 2]/2, col="grey50")
      theta <- seq(0, 2 * pi, length = 50)
      lines(-1.25+cos(theta)/2, -1.25+sin(theta)/2, col="grey50")
      text(-1.25+proj[, 1]/2, -1.25+proj[, 2]/2, label = labels, col="grey50")
    }

    g <- geodesic$Gz
    
    # Render projected points
    x <- data %*% proj
    if (center) x <- scale(x, center = TRUE, scale = FALSE)
    points(x, pch=20)
  }
  render_target <- function(target, geodesic) {
    rect(-1.99, -1.99, 1.99, 1.99, col="#7F7F7F33", border=NA)
  }

  animate(
    data = data, tour_path = tour_path,
    render_frame = render_frame, render_data = render_data,
    render_transition = render_transition, ...
  )
}

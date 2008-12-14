#' Parallel coordinates animation.
#'
#' Animate a nD tour path with a parallel coordinates plot. 
#'
#' The lines show the observations, and the points, the values of the 
#' projection matrix.
#'
#' @param data matrix, or data frame containing numeric columns
#' @param tourf tour path generator, defaults to the grand tour
#' @param d number of target dimensions
#' @param ... other arguments passed on to \code{\link{animate}}
#' @seealso \code{\link{animate}} for options that apply to all animations
#' @keywords hplot
#' @examples
#' animate_pcp(flea[, 1:6], d = 3)
#' animate_pcp(flea[, 1:6], d = 5)
animate_pcp <- function(data, tourf = grand_tour, d = 2, ...) {
  labels <- abbreviate(colnames(data), 2)
  xpos <- 1:d - 0.5
  
  render_frame <- function() {
    blank_plot(xlim = c(0, d), ylim = c(-2, 2))
  }
  render_transition <- function() {
    rect(0, -1.99, d, 1.99, col="#FFFFFFE6", border=NA)
  }
  render_data <- function(data, proj, geodesic) {    
    ys <- as.vector(t(cbind(data %*% proj, NA)))
    xs <- rep(c(xpos, NA), length = length(ys))
    
    # Grid lines
    segments(xpos, 1.99, xpos, -1.99, col="grey90")
    segments(0, 0, d, 0, col="grey90")
    
    # Projection values
    label_df <- data.frame(
      x = xpos, 
      y = as.vector(t(proj)),
      label = rep(labels, each = d)
    )
    with(
      subset(label_df, abs(y) > 0.05), 
      text(x, y, label = label, col="grey50", pos = 4)
    )
    with(label_df, points(x, y, col="grey50", pch = 20))
    
    # Data values
    lines(xs, ys)
  }
  render_target <- function(target, geodesic) {
    rect(0, -1.99, d, 1.99, col="#7F7F7F33", border=NA)
  }

  animate(
    data = data, tourf = tourf, d = d, 
    render_frame = render_frame, render_data = render_data,
    render_transition = render_transition, render_target = nul, 
    ...
  )
}

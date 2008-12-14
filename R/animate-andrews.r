#' Andrews' curves animation 
#'
#' Animate a nD tour path with Andrews' curves.  For more details about
#' Andrew's curves, see \code{\link{andrews}}
#'
#' @param data matrix, or data frame containing numeric columns
#' @param tourf tour path generator, defaults to the grand tour
#' @param d number of target dimensions
#' @param ... other arguments passed on to \code{\link{animate}}
#' @seealso \code{\link{animate}} for options that apply to all animations
#' @keywords hplot
#' 
#' @examples
#' animate_andrews(flea[, 1:6])
#' animate_andrews(flea[, 1:6], d = 3)
#' animate_andrews(flea[, 1:6], d = 6)
#' 
#' # It's easy to experiment with different tour paths:
#' animate_andrews(flea[, 1:6], guided_tour, index_f = cm)
animate_andrews <- function(data, tourf = grand_tour, d = 2, ...) {
  grid <- seq(-pi, pi, length = 50)
  data <- rescale(data)
  
  render_frame <- function() {
    blank_plot(xlim = c(-pi, pi), ylim = c(-1, 1))
  }
  render_transition <- function() {
    rect(-pi, -1, pi, 1, col="#FFFFFF", border=NA)
  }
  render_data <- function(data, proj, geodesic) {    
    xd <- data %*% proj
    xd <- rescale(xd)
    
    values <- lapply(seq_len(nrow(xd)), function(i) {
      rbind(
        cbind(grid, andrews(xd[i, ])(grid)),
        NA, NA
      )
    })
    segments(-pi, 0, pi, 0)
    lines(do.call("rbind", values))
  }

  animate(
    data = data, tourf = tourf, d = d, 
    render_frame = render_frame, render_data = render_data,
    render_transition = render_transition, render_target = nul, 
    ...
  )
}

#' Compute Andrews' curves
#' 
#' This function takes a numeric vector of input, and returns a function which
#' allows you to compute the value of the Andrew's curve at every point along
#' its path from -pi to pi.
#'
#' @param x input a new parameter
#' @return a function with single argument, theta
#'
#' @examples
#' a <- andrews(1:2)
#' a(0)
#' a(-pi)
#' grid <- seq(-pi, pi, length = 50)
#' a(grid)
#' 
#' plot(grid, andrews(1:2)(grid), type = "l")
#' plot(grid, andrews(runif(5))(grid), type = "l")
andrews <- function(x) {
  n <- length(x)
  y <- rep(x[1] / sqrt(2), length(t))

  function(t) {
    for(i in seq(2, n, by = 1)) {
      val <- i %/% 2 * t
      y <- y + x[i] * (if(i %% 2 == 0) sin(val) else cos(val))
    }
    y / n
  }
}

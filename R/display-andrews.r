#' Compute Andrews' curves
#'
#' This function takes a numeric vector of input, and returns a function which
#' allows you to compute the value of the Andrew's curve at every point along
#' its path from -pi to pi.
#'
#' @param x input a new parameter
#' @return a function with single argument, theta
#' @export
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
    for (i in seq(2, n, by = 1)) {
      val <- i %/% 2 * t
      y <- y + x[i] * (if (i %% 2 == 0) sin(val) else cos(val))
    }
    y / n
  }
}


#' Andrews' curves tour path animation.
#'
#' Animate a nD tour path with Andrews' curves.  For more details about
#' Andrew's curves, see \code{\link{andrews}}
#'
#' @param col color to be plotted.  Defaults to "black"
#' @param palette name of color palette for point colour, used by \code{\link{hcl.colors}}, default "Zissou 1"
#' @param ... other arguments passed on to \code{\link{animate}}
#' @seealso \code{\link{animate}} for options that apply to all animations
#' @keywords hplot
#' @export
#' @examples
#' animate_andrews(flea[, 1:6])
#' animate_andrews(flea[, 1:6], grand_tour(d = 3))
#' animate_andrews(flea[, 1:6], grand_tour(d = 6))
#'
#' # It's easy to experiment with different tour paths:
#' animate_andrews(flea[, 1:6], guided_tour(cmass()))
display_andrews <- function(col = "black", palette = "Zissou 1", ...) {
  grid <- NULL
  init <- function(data) {
    grid <<- seq(-pi, pi, length = 50)
  }

  # If colors are a variable, convert to colors
  if (is.factor(col) | !areColors(col)) {
    gps <- col
    col <- mapColors(col, palette)
  }

  render_frame <- function() {
    blank_plot(xlim = c(-pi, pi), ylim = c(-1, 1))
  }
  render_transition <- function() {
    # rect(-pi, -1, pi, 1, col="#FFFFFF", border=NA)
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

    render_frame()
    segments(-pi, 0, pi, 0)
    if (length(col) == 1) {
      col <- rep(col, nrow(data))
    }
    clrs <- rep(col, rep(52, nrow(data)))
    nclrs <- unique(clrs)
    ldat <- do.call("rbind", values)
    if (length(nclrs) > 1) {
      for (i in 1:length(nclrs)) {
        lines(ldat[clrs == nclrs[i], ], col = nclrs[i])
      }
    }
    else {
      lines(ldat)
    }
  }

  list(
    init = init,
    render_frame = render_frame,
    render_transition = render_transition,
    render_data = render_data,
    render_target = nul
  )
}


#' @rdname display_andrews
#' @inheritParams animate
#' @export
animate_andrews <- function(data, tour_path = grand_tour(3),
                            col = "black", ...) {
  animate(
    data = data, tour_path = tour_path,
    display = display_andrews(col, ...), ...
  )
}

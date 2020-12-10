#' Image tour path animation.
#'
#' Animate a 1d tour path with an image plot.  This animation requires a
#' different input data structure, a 3d array.  The first two dimensions are
#' locations on a grid, and the 3rd dimension gives the observations to be
#' mixed with the tour.
#'
#' @param xs x limit that is used in making the size of the plot
#' @param ys y limit that is used in making the size of the plot
#' @param ... other arguments passed on to \code{\link{animate}}
#' @seealso \code{\link{animate}} for options that apply to all animations
#' @keywords hplot
#' @export
#' @examples
#' str(ozone)
#' animate_image(ozone)
display_image <- function(xs, ys, ...) {
  render_frame <- function() {
    blank_plot(xlim = c(1, xs), ylim = c(1, xs))
  }

  render_data <- function(data, proj, geodesic) {
    z <- data %*% proj
    dim(z) <- c(xs, ys)
    image(
      x = seq_len(xs), y = seq_len(ys),
      z = t(z),
      zlim = c(-2, 2), add = TRUE
    )
  }

  list(
    init = nul,
    render_frame = render_frame,
    render_transition = nul,
    render_data = render_data,
    render_target = nul
  )
}


#' @rdname display_image
#' @inheritParams animate
#' @export
animate_image <- function(data, tour_path = grand_tour(1), ...) {
  xs <- dim(data)[1]
  ys <- dim(data)[2]
  zs <- dim(data)[3]
  dim(data) <- c(xs * ys, zs)

  animate(
    data = data, tour_path = tour_path,
    display = display_image(xs, ys, ...), ...
  )
}

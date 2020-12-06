#' Scatterplot matrix tour path animation.
#'
#' Animate a nD tour path with a scatterplot matrix.
#'
#' The lines show the observations, and the points, the values of the
#' projection matrix.
#'
#' @param ... other arguments passed on to \code{\link{animate}}
#' @seealso \code{\link{animate}} for options that apply to all animations
#' @keywords hplot
#' @export
#' @examples
#' animate_scatmat(flea[, 1:6], grand_tour(2))
#' animate_scatmat(flea[, 1:6], grand_tour(6))
display_scatmat <- function(...) {
  render_data <- function(data, proj, geodesic) {
    pairs(data %*% proj, pch = 20, ...)
  }

  list(
    init = nul,
    render_frame = nul,
    render_transition = nul,
    render_data = render_data,
    render_target = nul
  )
}


#' @rdname display_scatmat
#' @inheritParams animate
#' @export
animate_scatmat <- function(data, tour_path = grand_tour(3), ...) {
  animate(
    data = data, tour_path = tour_path,
    display = display_scatmat(...), ...
  )
}

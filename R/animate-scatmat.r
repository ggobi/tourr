#' Scatterplot matrix animation.
#'
#' Animate a nD tour path with a scatterplot matrix. 
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
#' animate_scatmat(flea[, 1:6], d = 3)
#' animate_scatmat(flea[, 1:6], d = 6)
animate_scatmat <- function(data, tourf = grand_tour, d = 3, ...) {
  render_data <- function(data, proj, geodesic) {
    pairs(data %*% proj, pch = 20)
  }

  animate(
    d = d, data = data, tourf = tourf, 
    render_frame = nul, render_data = render_data,
    render_transition = nul, render_target = nul, 
    ...
  )
}
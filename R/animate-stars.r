#' Star glyph  animation.
#'
#' Animate a nD tour path with star glyphs.
#'
#' Currently, scaling doesn't seem to be computed absolutely correctly, as 
#' centres move around as well as outside points.
#'
#' @param data matrix, or data frame containing numeric columns
#' @param tour_path tour path generator, defaults to the grand tour
#' @param d number of target dimensions
#' @param ... other arguments passed on to \code{\link{animate}} and 
#'   \code{\link{stars}}
#' @seealso \code{\link{animate}} for options that apply to all animations
#' @keywords hplot
#' @examples
#' animate_stars(flea[1:10, 1:6])
#' animate_stars(flea[1:10, 1:6], grand_tour(5))
#' animate_stars(flea[, 1:6], grand_tour(5))
#' animate_stars(flea[1:10, 1:6], grand_tour(5), 
#'  col.stars = rep("grey50", 10), radius = FALSE)
animate_stars <- function(data, tour_path = grand_tour(3), ...) {
  
  render_data <- function(data, proj, geodesic) {
    x <- data %*% proj
    x <- (x + 2) / 4
    stars(x, scale = FALSE, ...)
  }

  animate(
    data = data, tour_path = tour_path, 
    render_frame = nul, render_data = render_data,
    render_transition = nul, render_target = nul, 
    ...
  )
  
}

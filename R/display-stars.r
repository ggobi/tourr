#' Star glyph tour path animation.
#'
#' Animate a nD tour path with star glyphs.
#'
#' Currently, scaling doesn't seem to be computed absolutely correctly, as
#' centres move around as well as outside points.
#'
#' @param ... other arguments passed on to \code{\link{stars}}
#' @seealso \code{\link{animate}} for options that apply to all animations
#' @keywords hplot
#' @export
#' @examples
#' animate_stars(flea[1:10, 1:6])
#' animate_stars(flea[1:10, 1:6], grand_tour(5))
#' animate_stars(flea[, 1:6], grand_tour(5))
#' animate_stars(flea[1:10, 1:6], grand_tour(5),
#'   col.stars = rep("grey50", 10), radius = FALSE
#' )
display_stars <- function(...) {
  render_data <- function(data, proj, geodesic) {
    x <- data %*% proj
    x <- (x + 2) / 4
    stars(x, scale = FALSE, ...)
  }

  list(
    init = nul,
    render_frame = nul,
    render_transition = nul,
    render_data = render_data,
    render_target = nul
  )
}


#' @rdname display_stars
#' @inheritParams animate
#' @export
animate_stars <- function(data, tour_path = grand_tour(3), ...) {
  animate(
    data = data, tour_path = tour_path,
    display = display_stars(...), ...
  )
}

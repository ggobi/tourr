#' Display 3d projection with depth cues
#'
#' Suggestion to use gray background and colour saturation (instead of
#' gray shading) by Graham Wills.
#
#' @param center should projected data be centered to have mean zero (default:
#'   TRUE). This pins the centre of the data to the same place, and makes it
#'   easier to focus on the shape.
#' @param half_range half range to use when calculating limits of projected.
#'   If not set, defaults to maximum distance from origin to each row of data.
#' @param ... other arguments passed on to \code{\link{animate}}
#' @seealso \code{\link{animate}} for options that apply to all animations
#' @keywords hplot
#' @export
#' @examples
#' animate_depth(flea[, 1:6])
display_depth <- function(center = TRUE, half_range = NULL, ...) {
  shades <- hcl(240,
    c = seq(0, 60, length = 100),
    l = seq(80, 20, length = 100)
  )

  init <- function(data) {
    half_range <<- compute_half_range(half_range, data, center)
  }

  render_frame <- function() {
    par(pty = "s", mar = rep(1, 4))
    blank_plot(xlim = c(-1, 1), ylim = c(-1, 1))
    render_transition()
  }
  render_transition <- function() {
    rect(-1, -1, 1, 1, col = "grey80", border = NA)
  }
  render_data <- function(data, proj, geodesic) {
    x <- data %*% proj
    if (center) x <- center(x)
    x <- x / half_range

    depth <- x[, 3]
    # depth ranges between -1 and 1,
    # so depth_std should lie between 0 and 1
    depth_std <- depth / 2 + 0.5
    size <- 0.5 + depth_std * 3
    shade <- shades[round(depth_std * 100)]

    ord <- order(depth_std)
    points(x[ord, 1:2], pch = 20, cex = size[ord], col = shade[ord], ...)
  }

  list(
    init = init,
    render_frame = render_frame,
    render_transition = render_transition,
    render_data = render_data,
    render_target = nul
  )
}


#' @rdname display_depth
#' @inheritParams animate
#' @export
animate_depth <- function(data, tour_path = grand_tour(3), ...) {
  animate(
    data = data, tour_path = tour_path,
    display = display_depth(...),
    ...
  )
}

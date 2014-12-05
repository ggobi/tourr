#' Chernoff faces tour path animation.
#'
#' Animate a nD tour path with Chernoff's faces.  Can display up to 18
#' dimensions.
#'
#' This function requires the \code{TeachingDemos} package to draw the
#' Chernoff faces.  See \code{\link[TeachingDemos]{faces2}} for more details.
#'
#' @param ... other arguments passed on to \code{\link{animate}}
#' @seealso \code{\link{animate}} for options that apply to all animations
#' @keywords hplot
#' @export
#' @examples
#' # The drawing code is fairly slow, so this animation works best with a
#' # limited number of cases
#' animate_faces(flea[1:2, 1:6])
#' animate_faces(flea[1:4, 1:6])
#'
#' animate_faces(flea[1:2, 1:6], grand_tour(5))
display_faces <- function(...) {
  if (!requireNamespace("TeachingDemos", quietly = TRUE)) {
    stop("Please install the TeachingDemos package", call. = FALSE)
  }

  render_data <- function(data, proj, geodesic) {
    x <- data %*% proj
    x <- (x + 2) / 4
    TeachingDemos::faces2(x, scale = "none")
  }

  list(
    init = nul,
    render_frame = nul,
    render_transition = nul,
    render_data = render_data,
    render_target = nul
  )
}


#' @rdname display_faces
#' @inheritParams animate
#' @export
animate_faces <- function(data, tour_path = grand_tour(3), ...) {
  require("TeachingDemos")
  animate(data, tour_path, display = display_faces(...), ...)
}


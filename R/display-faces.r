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
#' flea_s <- rescale(flea[,1:6])
#' animate_faces(flea_s[19:24, 1:6])
#'
#' animate_faces(flea_s[19:24, 1:6], grand_tour(5))
display_faces <- function(...) {
  if (!requireNamespace("aplpack", quietly = TRUE)) {
    stop("Please install the aplpack package", call. = FALSE)
  }

  render_data <- function(data, proj, geodesic) {
    x <- data %*% proj
    x <- (x + 2) / 4
    aplpack::faces(x, scale = TRUE, face.type = 0,
                   cex = 0.1, print.info = FALSE)
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
  animate(data, tour_path, display = display_faces(...), ...)
}

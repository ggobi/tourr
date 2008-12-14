#' Chernoff faces animation.
#'
#' Animate a nD tour path with Chernoff's faces.  Can display up to 18
#' dimensions.
#'
#' This function requires the \code{TeachingDemos} package to draw the
#' Chernoff faces.  See \code{\link{faces2}} for more details.
#'
#' @param data matrix, or data frame containing numeric columns
#' @param tourf tour path generator, defaults to the grand tour
#' @param d number of target dimensions
#' @param ... other arguments passed on to \code{\link{animate}}
#' @seealso \code{\link{animate}} for options that apply to all animations
#' @keywords hplot
#' @examples
#' # The drawing code is fairly slow, so this animation works best with a 
#' # limited number of cases
#' animate_faces(flea[1:2, 1:6])
#' animate_faces(flea[1:4, 1:6])
#' 
#' animate_faces(flea[1:2, 1:6], d = 5)
animate_faces <- function(data, tourf = grand_tour, d = 3, ...) {
  require("TeachingDemos")
  
  render_data <- function(data, proj, geodesic) {
    x <- data %*% proj
    x <- (x + 2) / 4
    faces2(x, scale = "none")
  }

  animate(
    d = d, data = data, tourf = tourf, 
    render_frame = nul, render_data = render_data,
    render_transition = nul, render_target = nul, 
    ...
  )
}

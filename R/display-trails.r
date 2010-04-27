#' Display tour path with trails
#'
#' Animate a 2D tour path with a point trails
#'
#' @param axes position of the axes: center, bottomleft or off
#' @param center if TRUE, centers projected data to (0,0).  This pins the 
#'  center of data cloud and make it easier to focus on the changing shape
#'  rather than position.
#' @param limit limits of the projected data.  Defaults to 3 * square root
#'  of the largest eigenvalue.
#' @param col color to be plotted.  Defaults to "black"
#' @param pch size of the point to be plotted.  Defaults to 20.
#' @param past draw line between current projection and projection \code{past}
#'   steps ago 
#' @param ...  other arguments passed on to \code{\link{animate}} and
#'   \code{\link{display_xy}}
#' @aliases display_xy animate_xy
#' @examples
display_trails <- function(center = TRUE, axes = "center", limit = NULL, col = "black", pch  = 20, past = 3, ...) {
  
  past_x <- vector("list", past)
  labels <- rng <- limit <- NULL
  init <- function(data) {
    limit <<- xy_limits(data, limit, center)
    rng <<- c(-limit, limit)    
    labels <<- abbreviate(colnames(data), 3)
  }
  
  render_frame <- function() {
    par(pty = "s", mar = rep(1,4))
    blank_plot(xlim = rng, ylim = rng)
  }
  render_transition <- function() {
    rect(-limit, -limit, limit, limit, col="#FFFFFFE6", border=NA)
  }
  render_data <- function(data, proj, geodesic) {
    draw_tour_axes(proj, labels, limit, axes)
    
    x <- data %*% proj
    if (center) x <- scale(x, center = TRUE, scale = FALSE)    
    
    # Render projected points
    last_x <- past_x[[1]]
    if (!is.null(last_x)) {
      segments(last_x[, 1], last_x[, 2], x[, 1], x[, 2], col = col, pch = pch)
    }
    past_x <<- c(past_x[2:5], list(x))
  }
  
  list(
    init = init,
    render_frame = render_frame,
    render_transition = render_transition,
    render_data = render_data,
    render_target = nul
  )
}

animate_trails <- function(data, tour_path = grand_tour(), ...) {
  animate(data, tour_path, display_trails(...), ...)
}
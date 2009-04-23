#' Scatterplot tour path animation.
#'
#' Animate a 2D tour path with a scatterplot.
#'
#' @param data matrix, or data frame containing numeric columns
#' @param tour_path tour path, defaults to the grand tour
#' @param ... other arguments passed on to \code{\link{animate}} and
#'   \code{\link{display_xy}}
#'
#' @examples
#' animate_xy(flea[, 1:6])
#' animate_xy(flea[, 1:6], little_tour())
#' animate_xy(flea[, 1:3], guided_tour(holes))
#' animate_xy(flea[, 1:6], center = FALSE)
#'
#' # The default axes are centered, like a biplot, but there are other options
#' animate_xy(flea[, 1:6], axes = "bottomleft")
#' animate_xy(flea[, 1:6], axes = "off")
#' animate_xy(flea[, 1:6], dependence_tour(c(1, 2, 1, 2, 1, 2)),
#'   axes = "bottomleft")
animate_xy <- function(data, tour_path = grand_tour(), ...) {
print(head(data))
  animate2(data, tour_path, display_xy(data, ...), ...)
}

#' Display tour path with a scattploer
#'
#' Animate a 2D tour path with a scatterplot.
#'
#' @param axes position of the axes: center, bottomleft or off
#' @param center if TRUE, centers projected data to (0,0).  This pins the 
#'  center of data cloud and make it easier to focus on the changing shape
#'  rather than position.
#' @param limit limits of the projected data.  Defaults to 3 * square root
#'  of the largest eigenvalue.
#' @examples
#' animate2(flea[, 1:6], grand_tour(), display_xy()
#' animate2(flea[, 1:6], grand_tour(), display_xy(axes = "bottomleft"))
#' animate2(flea[, 1:6], grand_tour(), display_xy(limits = c(-3, 3))
display_xy <- function(data, center = TRUE, axes = "center", limit = NULL, ...) {
  axes <- match.arg(axes, c("center", "bottomleft", "off"))

  labels <- rng <- limit <- NULL
  init <- function(data) {
    if (is.null(limit)) {
      first_eigen <- sqrt(eigen(var(data))$values[1])
      limit <<- 3 * first_eigen
    }
    rng <<- c(-limit, limit)    
    labels <<- abbreviate(colnames(data), 2)
  }
  
  render_frame <- function() {
    par(pty = "s", mar = rep(1,4))
    blank_plot(xlim = rng, ylim = rng)
  }
  render_transition <- function() {
    rect(-1.99, -1.99, 1.99, 1.99, col="#FFFFFFE6", border=NA)
  }
  render_data <- function(data, proj, geodesic) {
    # Render axes
    if (axes == "center") {
      axis_scale <- 2 * limit / 3
      axis_pos <- 0
    } else if (axes == "bottomleft") {
      axis_scale <- limit / 6
      axis_pos <- -2/3 * limit
    }
    
    adj <- function(x) axis_pos + x * axis_scale

    segments(adj(0), adj(0), adj(proj[, 1]), adj(proj[, 2]), col="grey50")
    theta <- seq(0, 2 * pi, length = 50)
    lines(adj(cos(theta)), adj(sin(theta)), col = "grey50")
    text(adj(proj[, 1]), adj(proj[, 2]), label = labels, col = "grey50")

    g <- geodesic$Gz
    
    # Render projected points
    x <- data %*% proj
    if (center) x <- scale(x, center = TRUE, scale = FALSE)
    points(x, pch=20, ...)
  }
  render_target <- function(target, geodesic) {
    rect(-1.99, -1.99, 1.99, 1.99, col="#7F7F7F33", border=NA)
  }
  
  list(
    init = init,
    render_frame = render_frame,
    render_transition = render_transition,
    render_data = render_data,
    render_target = render_target
  )
}

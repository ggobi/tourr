#' Display tour path with a density and scatterplot
#'
#' Animate a 2D tour path with density contour(s) and a scatterplot.
#'
#' @param axes position of the axes: center, bottomleft or off
#' @param center if TRUE, centers projected data to (0,0).  This pins the
#'  center of data cloud and make it easier to focus on the changing shape
#'  rather than position.
#' @param half_range half range to use when calculating limits of projected.
#'   If not set, defaults to maximum distance from origin to each row of data.
#' @param edges A two column integer matrix giving indices of ends of lines.
#' @param col color to use for points, can be a vector or hexcolors or a factor.  Defaults to "black".
#' @param pch shape of the point to be plotted.  Defaults to 20.
#' @param cex size of the point to be plotted.  Defaults to 1.
#' @param contour_quartile Vector of quartiles to plot the contours at. Defaults to 5.
#' @param palette name of color palette for point colour, used by \code{\link{hcl.colors}}, default "Zissou 1"
#' @param ...  other arguments passed on to \code{\link{animate}} and
#'   \code{\link{display_density2d}}
#' @importFrom graphics contour
#' @importFrom stats quantile
#' @export
#' @examples
#' animate_density2d(flea[, 1:6])
#' animate(flea[, 1:6], tour_path = grand_tour(), display = display_density2d())
#' animate(flea[, 1:6],
#'   tour_path = grand_tour(),
#'   display = display_density2d(axes = "bottomleft")
#' )
#' animate(flea[, 1:6],
#'   tour_path = grand_tour(),
#'   display = display_density2d(half_range = 0.5)
#' )
#' animate_density2d(flea[, 1:6], tour_path = little_tour())
#'
#' animate_density2d(flea[, 1:3], tour_path = guided_tour(holes()), sphere = TRUE)
#' animate_density2d(flea[, 1:6], center = FALSE)
#'
#' # The default axes are centered, like a biplot, but there are other options
#' animate_density2d(flea[, 1:6], axes = "bottomleft")
#' animate_density2d(flea[, 1:6], axes = "off")
#' animate_density2d(flea[, 1:6], dependence_tour(c(1, 2, 1, 2, 1, 2)),
#'   axes = "bottomleft"
#' )
#'
#' animate_density2d(flea[, -7], col = flea$species)
#'
#' # You can also draw lines
#' edges <- matrix(c(1:5, 2:6), ncol = 2)
#' animate(
#'   flea[, 1:6], grand_tour(),
#'   display_density2d(axes = "bottomleft", edges = edges)
#' )
display_density2d <- function(center = TRUE, axes = "center", half_range = NULL,
                              col = "black", pch = 20, cex = 1,
                              contour_quartile = c(.25, .5, .75), edges = NULL,
                              palette = "Zissou 1", ...) {
  # If colors are a variable, convert to colors
  if (is.factor(col) | !areColors(col)) {
    gps <- col
    col <- mapColors(col, palette)
  }

  labels <- NULL
  init <- function(data) {
    half_range <<- compute_half_range(half_range, data, center)
    labels <<- abbreviate(colnames(data), 3)
  }

  if (!is.null(edges)) {
    if (!is.matrix(edges) && ncol(edges) == 2) {
      stop("Edges matrix needs two columns, from and to, only.")
    }
  }

  render_frame <- function() {
    par(pty = "s", mar = rep(0.1, 4))
    blank_plot(xlim = c(-1, 1), ylim = c(-1, 1))
  }
  render_transition <- function() {
    rect(-1, -1, 1, 1, col = "#FFFFFFE6", border = NA)
  }
  render_data <- function(data, proj, geodesic) {
    draw_tour_axes(proj, labels, limits = 1, axes)

    # Render projected points
    x <- data %*% proj
    if (center) x <- center(x)
    x <- x / half_range

    colrs <- unique(col)
    ngps <- length(colrs)
    if (ngps == 1) {
      xd <- MASS::kde2d(x[, 1], x[, 2])

      contour(xd,
        col = col,
        levels = quantile(xd$z, probs = contour_quartile),
        axes = FALSE
      )
    }
    else {
      for (i in 1:ngps) {
        x.sub <- x[col == colrs[i], ]
        xd <- MASS::kde2d(x.sub[, 1], x.sub[, 2])

        contour(xd,
          col = colrs[i],
          levels = quantile(xd$z, probs = contour_quartile),
          axes = FALSE, add = TRUE
        )
      }
    }
    points(x, col = col, pch = pch, cex = cex)

    if (!is.null(edges)) {
      segments(
        x[edges[, 1], 1], x[edges[, 1], 2],
        x[edges[, 2], 1], x[edges[, 2], 2]
      )
    }
  }

  list(
    init = init,
    render_frame = render_frame,
    render_transition = render_transition,
    render_data = render_data,
    render_target = nul
  )
}

#' @rdname display_density2d
#' @inheritParams animate
#' @export
animate_density2d <- function(data, tour_path = grand_tour(), ...) {
  animate(data, tour_path, display_density2d(...))
}

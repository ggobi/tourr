#' Display tour path with a sliced scatterplot
#'
#' Animate a 2D tour path with a sliced scatterplot.
#'
#' @param axes position of the axes: center, bottomleft or off
#' @param center if TRUE, centers projected data to (0,0).  This pins the
#'  center of data cloud and make it easier to focus on the changing shape
#'  rather than position.
#' @param half_range half range to use when calculating limits of projected.
#'   If not set, defaults to maximum distance from origin to each row of data.
#' @param edges A two column integer matrix giving indices of ends of lines.
#' @param edges.col colour of edges to be plotted, Defaults to "black.
#' @param col color to use for points, can be a vector or hexcolors or a factor.  Defaults to "black".
#' @param pch_slice marker for plotting points inside the slice.
#'   Defaults to 20.
#' @param pch_other marker for plotting points outside the slice.
#'   Defaults to 46.
#' @param cex_slice size of the points inside the slice. Defaults to 2.
#' @param cex_other size if the points outside the slice. Defaults to 1.
#' @param v_rel relative volume of the slice. If not set, suggested value
#'   is calculated and printed to the screen.
#' @param anchor A vector specifying the reference point to anchor the slice.
#'   If NULL (default) the slice will be anchored at the data center.
#' @param anchor_nav position of the anchor: center, topright or off
#' @param rescale Default FALSE. If TRUE, rescale all variables to range [0,1].
#' @param palette name of color palette for point colour, used by \code{\link{hcl.colors}}, default "Zissou 1"
#' @param ...  other arguments passed on to \code{\link{animate}} and
#'   \code{\link{display_slice}}
#' @export
#' @examples
#' # Generate samples on a 3d and 5d hollow sphere using the geozoo package
#' sphere3 <- geozoo::sphere.hollow(3)$points
#' sphere5 <- geozoo::sphere.hollow(5)$points
#'
#' # Columns need to be named before launching the tour
#' colnames(sphere3) <- c("x1", "x2", "x3")
#' colnames(sphere5) <- c("x1", "x2", "x3", "x4", "x5")
#'
#' # Animate with the slice display using the default parameters
#' animate_slice(sphere3)
#' animate_slice(sphere5)
#'
#' # Animate with off-center anchoring
#' anchor3 <- matrix(rep(0.7, 3), ncol=3)
#' anchor5 <- matrix(rep(0.3, 5), ncol=5)
#' animate_slice(sphere3, anchor = anchor3)
#' # Animate with thicker slice to capture more points in each view
#' animate_slice(sphere5, anchor = anchor5, v_rel = 0.02)
display_slice <- function(center = TRUE, axes = "center", half_range = NULL,
                          col = "black", pch_slice = 20, pch_other = 46,
                          cex_slice = 2, cex_other = 1, v_rel = NULL,
                          anchor = NULL, anchor_nav = "off",
                          edges = NULL, edges.col = "black",
                          palette = "Zissou 1", ...) {
  labels <- NULL
  h <- NULL

  # If colors are a variable, convert to colors
  if (is.factor(col) | !areColors(col)) {
    gps <- col
    col <- mapColors(col, palette)
  }
  if (is.factor(edges.col) | !areColors(edges.col)) {
    edges.gps <- edges.col
    edges.col <- mapColors(edges.col, palette)
  }

  init <- function(data) {
    half_range <<- compute_half_range(half_range, data, center)
    labels <<- abbreviate(colnames(data), 3)
    v_rel <<- compute_v_rel(v_rel, half_range, ncol(data))
    # Translate volume v_rel to cutoff h
    h <<- v_rel^(1 / (ncol(data) - 2))
    message("Using v_rel=", format(v_rel, digits = 2), ", corresponding to a cutoff h=", format(h, digits = 2))
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

  render_data <- function(data, proj, geodesic, with_anchor = anchor) {
    draw_tour_axes(proj, labels, limits = 1, axes)
    if (!is.null(with_anchor)) {
      rng <- apply(data, 2, range)
      colnames(with_anchor) <- colnames(data)
      draw_slice_center(with_anchor, rng, limits = 1, anchor_nav = anchor_nav)
    }

    # Render projected points
    x <- data %*% proj
    d <- anchored_orthogonal_distance(proj, data, with_anchor)
    pch <- rep(pch_other, nrow(x))
    pch[d < h] <- pch_slice
    cex <- rep(cex_other, nrow(x))
    cex[d < h] <- cex_slice
    if (center) x <- center(x)
    x <- x / half_range
    points(x, col = col, pch = pch, cex = cex)

    if (!is.null(edges)) {
      segments(x[edges[, 1], 1], x[edges[, 1], 2],
               x[edges[, 2], 1], x[edges[, 2], 2],
               col = edges.col
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

#' @rdname display_slice
#' @inheritParams animate
#' @export
animate_slice <- function(data, tour_path = grand_tour(), rescale = FALSE, ...) {
  animate(data, tour_path, display_slice(...), rescale = rescale)
}

#' Draw slice center guide with base graphics
#' @keywords internal
draw_slice_center <- function(anchor, rng, limits, anchor_nav, ...) {
  anchor_nav <- match.arg(anchor_nav, c("center", "topright", "off"))
  if (anchor_nav == "off") {
    return()
  }

  if (anchor_nav == "center") {
    axis_scale <- 2 * limits / 3
    axis_pos <- 0
  } else if (anchor_nav == "topright") {
    axis_scale <- limits / 6
    axis_pos <- 2 / 3 * limits
  }

  adj <- function(x) axis_pos + x * axis_scale

  n <- ncol(anchor)
  theta <- seq(90, 450, length = n + 1) * pi/180
  theta <- theta[1:n]

  xx <- cos(theta)
  yy <- sin(theta)
  cgap <- 1 # shift of min out from middle
  seg <- 4
  cglty <- 3
  cglwd <- 1
  cglcol <- "black"
  for (i in 0:seg) {
    polygon(adj(xx * (i + cgap)/(seg + cgap)),
            adj(yy * (i + cgap)/(seg + cgap)),
            lty = cglty, lwd = cglwd, border = cglcol)
    arrows(adj(xx/(seg + cgap)),
           adj(yy/(seg + cgap)),
           adj(xx * 1),
           adj(yy * 1),
           lwd = cglwd, lty = cglty,
           length = 0, col = cglcol)
  }
  VLABELS <- colnames(anchor)
  text(adj(xx * 1.2), adj(yy * 1.2), VLABELS, cex=0.8)
  xxs <- xx
  yys <- yy
  scale <- cgap/(seg + cgap) +
    (anchor[1, ] - rng[1, ])/
    (rng[2,] - rng[1, ]) * seg/(seg + cgap)
  for (j in 1:n) {
    xxs[j] <- adj(xx[j] * cgap/(seg + cgap) + xx[j] *
      (anchor[1, j] - rng[1, j])/(rng[2, j] - rng[1, j]) *
      seg/(seg + cgap))
    yys[j] <- adj(yy[j] * cgap/(seg + cgap) + yy[j] *
      (anchor[1, j] - rng[1, j])/(rng[2, j] - rng[1, j]) *
      seg/(seg + cgap))
  }
  polygon(xxs, yys, lty = 1, lwd = 2,
          border = "black",
          col = NULL)
  points(adj(xx * scale), adj(yy * scale), pch = 16,
         col = "black")

}

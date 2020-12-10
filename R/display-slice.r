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
#' @param col color to be plotted.  Defaults to "black"
#' @param pch_slice marker for plotting points inside the slice.
#'   Defaults to 20.
#' @param pch_other marker for plotting points outside the slice.
#'   Defaults to 46.
#' @param cex_slice size of the points inside the slice. Defaults to 2.
#' @param cex_other size if the points outside the slice. Defaults to 1.
#' @param v_rel relative volume of the slice. If not set, suggested value
#'   is caluclated and printed to the screen.
#' @param anchor A vector specifying the reference point to anchor the slice.
#'   If NULL (default) the slice will be anchored at the data center.
#' @param rescale if true, rescale all variables to range [0,1].
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
#' anchor3 <- rep(0.7, 3)
#' anchor5 <- rep(0.3, 5)
#' animate_slice(sphere3, anchor = anchor3)
#' # Animate with thicker slice to capture more points in each view
#' animate_slice(sphere5, anchor = anchor5, v_rel = 0.02)
display_slice <- function(center = TRUE, axes = "center", half_range = NULL,
                          col = "black", pch_slice = 20, pch_other = 46,
                          cex_slice = 2, cex_other = 1, v_rel = NULL,
                          anchor = NULL, edges = NULL, edges.col = "black", ...) {
  labels <- NULL
  h <- NULL

  if (!areColors(col)) col <- mapColors(col)

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

  render_data <- function(data, proj, geodesic) {
    draw_tour_axes(proj, labels, limits = 1, axes)


    # Render projected points
    x <- data %*% proj
    d <- anchored_orthogonal_distance(proj, data, anchor)
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
animate_slice <- function(data, tour_path = grand_tour(), rescale = TRUE, ...) {
  animate(data, tour_path, display_slice(...), rescale = rescale)
}

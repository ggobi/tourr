#' Display tour path with principal component scores with original axes
#'
#' Animate a 2D tour path on data that has been transformed into
#' principal components, and also show the original variable axes.
#'
#' @param axes position of the axes: center, bottomleft or off
#' @param center if TRUE, centers projected data to (0,0).  This pins the
#'  center of data cloud and make it easier to focus on the changing shape
#'  rather than position.
#' @param half_range half range to use when calculating limits of projected.
#'   If not set, defaults to maximum distance from origin to each row of data.
#' @param col color to use for points, can be a vector or hexcolors or a factor.  Defaults to "black".
#' @param pch shape of the point to be plotted.  Defaults to 20.
#' @param cex size of the point to be plotted.  Defaults to 1.
#' @param pc_coefs coefficients relating the original variables to
#'   principal components. This is required.
#' @param edges A two column integer matrix giving indices of ends of lines.
#' @param edges.col colour of edges to be plotted, Defaults to "black.
#' @param rescale Default FALSE. If TRUE, rescale all variables to range [0,1].
#' @param palette name of color palette for point colour, used by \code{\link{hcl.colors}}, default "Zissou 1"
#' @param ...  other arguments passed on to \code{\link{animate}} and
#'   \code{\link{display_slice}}
#' @export
#' @examples
#' flea_std <- apply(flea[,1:6], 2, function(x) (x-mean(x))/sd(x))
#' flea_pca <- prcomp(flea_std, center = FALSE, )
#' flea_coefs <- flea_pca$rotation[, 1:3]
#' flea_scores <- flea_pca$x[, 1:3]
#' animate_pca(flea_scores, pc_coefs = flea_coefs)
display_pca <- function(center = TRUE, axes = "center", half_range = NULL,
                        col = "black", pch = 20, cex = 1,
                        pc_coefs = NULL,
                        edges = NULL, edges.col = "black",
                        palette = "Zissou 1", ...) {
  labels <- NULL

  # If colors are a variable, convert to colors
  if (is.factor(col) | !areColors(col)) {
    gps <- col
    col <- mapColors(col, palette)
  }

  init <- function(data) {
    half_range <<- compute_half_range(half_range, data, center)
    labels <<- abbreviate(rownames(pc_coefs), 3)
    stopifnot(ncol(pc_coefs) == ncol(data))
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
    # Render axes
    pc_axes <- pc_coefs %*% proj
    draw_tour_axes(pc_axes, labels, limits = 1, axes)

    # Render projected points
    x <- data %*% proj
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

#' @rdname display_pca
#' @inheritParams animate
#' @export
animate_pca <- function(data, tour_path = grand_tour(), rescale = FALSE, ...) {
  animate(data, tour_path, display_pca(...), rescale = rescale)
}

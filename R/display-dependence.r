#' Display tour path with two groups of variables
#'
#' Animate a 2D tour path with a scatterplot.
#'
#' @param axes position of the axes: center, bottomleft or off
#' @param center if TRUE, centers projected data to (0,0).  This pins the
#'  center of data cloud and make it easier to focus on the changing shape
#'  rather than position.
#' @param half_range half range to use when calculating limits of projected.
#'   If not set, defaults to maximum distance from origin to each row of data.
#' @param edges A two column integer matrix giving indices of ends of lines.
#' @param col color to use for points, can be a vector or hexcolors or a factor.  Defaults to "black".
#' @param pch shape of the point to be plotted, can be a factor or integer.  Defaults to 20.
#' @param cex size of the point to be plotted.  Defaults to 1.
#' @param edges.col colour of edges to be plotted, Defaults to "black"
#' @param edges.width line width for edges, default 1
#' @param obs_labels vector of text labels to display
#' @param fit logical indicating where to add a loess line onto plot
#' @param palette name of color palette for point colour, used by \code{\link{hcl.colors}}, default "Zissou 1"
#' @param shapeset numbers corresponding to shapes in base R points, to use for mapping
#'        categorical variable to shapes, default=c(15:17, 23:25)
#' @param axislablong text labels only for the long axes in a projection, default FALSE
#' @param ...  other arguments passed on to \code{\link{animate}}
#' @importFrom graphics legend
#' @export
#' @examples
#' animate_dependence(flea[, 1:3],
#'   dependence_tour(c(1, 2, 2)),
#'   display_dependence(fit = TRUE, axes = "off"))
#'
display_dependence <- function(center = TRUE, axes = "center", half_range = NULL,
                       col = "black", pch = 20, cex = 1,
                       edges = NULL, edges.col = "black", edges.width=1,
                       obs_labels = NULL,
                       fit = FALSE,
                       palette="Zissou 1", shapeset=c(15:17, 23:25),
                       axislablong = FALSE, ...) {
  # Needed for CRAN checks
  labels <- NULL
  gps <- NULL
  shapes <- NULL

  # If colors are a variable, convert to colors
  if (is.factor(col) | !areColors(col)) {
    gps <- col
    col <- mapColors(col, palette)
  }
  if (is.factor(edges.col) | !areColors(edges.col)) {
    edges.gps <- edges.col
    edges.col <- mapColors(edges.col, palette)
  }
  # If shapes are a variable, convert shapes
  if (is.factor(pch)) {
    shapes <- mapShapes(pch, shapeset)
  } else {
    shapes <- pch
  }

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
    draw_tour_axes(proj, labels, limits = 1, axes, longlabels = axislablong, ...)

    x <- data %*% proj
    if (center) x <- center(x)
    x <- x / half_range
    points(x, col = col, pch = shapes, cex = cex)

    if (!is.null(obs_labels)) {
      text(x, labels = obs_labels, col = col, pos = 4, offset = 0.1)
    }
    if (!is.null(edges)) {
      segments(x[edges[, 1], 1], x[edges[, 1], 2],
               x[edges[, 2], 1], x[edges[, 2], 2],
               col = edges.col, lwd = edges.width)
    }
    if (is.factor(gps)) {
      numcol <- unique(col)
      if (length(numcol) > 1)
        legend("topright", legend = unique(gps), col = numcol, pch = 15)
    }
    if (is.factor(pch)) {
      numpch <- unique(shapes)
      if (length(numpch) > 1)
        legend("bottomright", legend = unique(pch), col = "black", pch = unique(shapes))
    }
    if (fit) {
      stopifnot(ncol(proj) == 2)
      data.fit <- loess(x[,2] ~ x[,1])
      ord <- order(x[, 1])
      lines(x[ord,1], data.fit$fitted[ord], col = "black")
    }
  }  # <-- render_data closes HERE

  list(     # <-- this list is now the return value of display_dependence()
    init = init,
    render_frame = render_frame,
    render_transition = render_transition,
    render_data = render_data,
    render_target = nul
  )
}  # <-- display_dependence closes here

#' @rdname display_dependence
#' @inheritParams animate
#' @export
animate_dependence <- function(data, tour_path = dependence_tour(),
                               display = display_dependence(), ...) {
  animate(data, tour_path, display, ...)
}

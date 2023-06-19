#' Display tour path with a scatterplot
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
#' @param obs_labels vector of text labels to display
#' @param edges.width line width for edges, default 1
#' @param palette name of color palette for point colour, used by \code{\link{hcl.colors}}, default "Zissou 1"
#' @param ...  other arguments passed on to \code{\link{animate}} and
#'   \code{\link{display_xy}}
#' @importFrom graphics legend
#' @export
#' @examples
#' animate_xy(flea[, 1:6])
#' animate(flea[, 1:6], tour_path = grand_tour(), display = display_xy())
#' animate(flea[, 1:6],
#'   tour_path = grand_tour(),
#'   display = display_xy(),
#'   scale = TRUE
#' )
#' animate(flea[, 1:6],
#'   tour_path = grand_tour(),
#'   display = display_xy(half_range = 0.5)
#' )
#' animate_xy(flea[, 1:6], tour_path = little_tour())
#' animate_xy(flea[, 1:3], tour_path = guided_tour(holes()), sphere = TRUE)
#' animate_xy(flea[, 1:6], center = FALSE)
#'
#' # The default axes are centered, like a biplot, but there are other options
#' animate_xy(flea[, 1:6], axes = "bottomleft")
#' animate_xy(flea[, 1:6], axes = "off")
#' animate_xy(flea[, 1:6], dependence_tour(c(1, 2, 1, 2, 1, 2)),
#'   axes = "bottomleft"
#' )
#'
#' animate_xy(flea[, -7], col = flea$species)
#' animate_xy(flea[, -7], col = flea$species,
#'              pch = flea$species)
#'
#' animate_xy(flea[, -7], col = flea$species,
#'   obs_labels=as.character(1:nrow(flea)), axes="off")
#'
#' # You can also draw lines
#' edges <- matrix(c(1:5, 2:6), ncol = 2)
#' animate(
#'   flea[, 1:6], grand_tour(),
#'   display_xy(axes = "bottomleft", edges = edges)
#' )
display_xy <- function(center = TRUE, axes = "center", half_range = NULL,
                       col = "black", pch = 20, cex = 1,
                       edges = NULL, edges.col = "black", edges.width=1,
                       obs_labels = NULL, palette="Zissou 1", ...) {
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
    shapes <- mapShapes(pch)
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
    draw_tour_axes(proj, labels, limits = 1, axes, ...)

    # Render projected points
    x <- data %*% proj
    if (center) x <- center(x)
    x <- x / half_range
    points(x, col = col, pch = shapes, cex = cex)

    # Render labels for obs, if provided
    if (!is.null(obs_labels)) {
      text(x, labels=obs_labels, col=col, pos=4, offset=0.1)
    }
    # Draw segments between points, if provided
    if (!is.null(edges)) {
      segments(x[edges[, 1], 1], x[edges[, 1], 2],
        x[edges[, 2], 1], x[edges[, 2], 2],
        col = edges.col,
        lwd = edges.width
      )
    }

    # add a legend, only if a variable was used
    if (is.factor(gps)) {
      numcol <- unique(col)
      if (length(numcol) > 1) {
        legend("topright", legend=unique(gps),
             col=numcol, pch=15)
      }
    }
    if (is.factor(pch)) {
      numpch <- unique(shapes)
      if (length(numpch) > 1) {
        legend("bottomright", legend=unique(pch),
             col="black", pch=unique(shapes))
      }
    }

    # Add index value if using guided tour
    #if (!is.na(cur_index))
    #  text(0, 0, labels=round(cur_index, 2))

  }

  list(
    init = init,
    render_frame = render_frame,
    render_transition = render_transition,
    render_data = render_data,
    render_target = nul
  )
}

#' @rdname display_xy
#' @inheritParams animate
#' @export
animate_xy <- function(data, tour_path = grand_tour(), ...) {
  animate(data, tour_path, display_xy(...), ...)
}

#' Draw tour axes on the projected data with base graphics
#'
#' @param proj matrix of projection coefficients
#' @param labels variable names for the axes, of length the same
#'   as the number of rows of proj
#' @param limits value setting the lower and upper limits of
#'   projected data, default 1
#' @param position position of the axes: center (default),
#'   bottomleft or off
#' @param ...  other arguments passed
#' @export
#' @examples
#' data(flea)
#' flea_std <- apply(flea[,1:6], 2, function(x) (x-mean(x))/sd(x))
#' prj <- basis_random(ncol(flea[,1:6]), 2)
#' flea_prj <- as.data.frame(as.matrix(flea_std) %*% prj)
#' par(pty = "s", mar = rep(0.1, 4))
#' plot(flea_prj$V1, flea_prj$V2,
#'      xlim = c(-3, 3), ylim = c(-3, 3),
#'      xlab="P1", ylab="P2")
#' draw_tour_axes(prj, colnames(flea)[1:6], limits=3)
#'
#' plot(flea_prj$V1, flea_prj$V2,
#'      xlim = c(-3, 3), ylim = c(-3, 3),
#'      xlab="P1", ylab="P2")
#' draw_tour_axes(prj, colnames(flea)[1:6], limits=3, position="bottomleft")
draw_tour_axes <- function(proj, labels, limits=1, position="center", ...) {
  position <- match.arg(position, c("center", "bottomleft", "off"))
  if (position == "off") {
    return()
  }

  if (position == "center") {
    axis_scale <- 2 * limits / 3
    axis_pos <- 0
  } else if (position == "bottomleft") {
    axis_scale <- limits / 6
    axis_pos <- -2 / 3 * limits
  }

  adj <- function(x) axis_pos + x * axis_scale

  segments(adj(0), adj(0), adj(proj[, 1]), adj(proj[, 2]), col = "grey50")
#  if (!is.null(mvar)) { # colour manip var
#    if ((mvar < (nrow(proj)+1)) & (mvar > 0)) {
#      segments(adj(0), adj(0), adj(proj[, 1]), adj(proj[, 2]), col = "orange")
#    }
#  }
  theta <- seq(0, 2 * pi, length = 50)
  lines(adj(cos(theta)), adj(sin(theta)), col = "grey50")
  text(adj(proj[, 1]), adj(proj[, 2]), label = labels, col = "grey50")
}

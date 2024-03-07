#' Display 2D tour projections displayed separately by groups
#'
#' This function is designed to allow comparisons across multiple groups,
#'  especially for examining things like two (or more) different models
#'  on the same data. The primary display is a scatterplot, with lines or
#'  contours overlaid.
#'
#' @param axes position of the axes: center, bottomleft or off
#' @param centr if TRUE, centers projected data to (0,0).  This pins the
#'  center of data cloud and make it easier to focus on the changing shape
#'  rather than position.
#' @param half_range half range to use when calculating limits of projected.
#'   If not set, defaults to maximum distance from origin to each row of data.
#' @param edges A two column integer matrix giving indices of ends of lines.
#' @param col color to use for points, can be a vector or hexcolors or a factor.  Defaults to "black".
#' @param pch shape of the point to be plotted.  Defaults to 20.
#' @param cex size of the point to be plotted.  Defaults to 1.
#' @param group_by variable to group by. Must have less than 25 unique values.
#' @param plot_xgp if TRUE, plots points from other groups in light grey
#' @param edges.col colour of edges to be plotted, Defaults to "black"
#' @param edges.width line width for edges, default 1
#' @param palette name of color palette for point colour, used by \code{\link{hcl.colors}}, default "Zissou 1"
#' @param shapeset numbers corresponding to shapes in base R points, to use for mapping
#'        categorical variable to shapes, default=c(15:17, 23:25)
#' @param ...  other arguments passed on to \code{\link{animate}} and
#'   \code{\link{display_groupxy}}
#' @export
#' @examples
#' animate_groupxy(flea[, 1:6], col = flea$species,
#'   pch = flea$species, group_by = flea$species)
#' animate_groupxy(flea[, 1:6], col = flea$species,
#'   pch = flea$species, group_by = flea$species,
#'   plot_xgp = FALSE)
#' # Edges example
#' x <- data.frame(x1=runif(10, -1, 1), x2=runif(10, -1, 1), x3=runif(10, -1, 1))
#' x$cl <- factor(c(rep("A", 3), rep("B", 3), rep("C", 4)))
#' x.edges <- cbind(from=c(1,2, 4,5, 7,8,9), to=c(2,3, 5,6, 8,9,10))
#' x.edges.col <- factor(c(rep("A", 2), rep("B", 2), rep("C", 3)))
#' animate_groupxy(x[,1:3], col=x$cl, group_by=x$cl, edges=x.edges, edges.col=x.edges.col)
display_groupxy <- function(centr = TRUE, axes = "center", half_range = NULL,
                            col = "black", pch = 20, cex = 1,
                            edges = NULL, edges.col = "black", edges.width=1,
                            group_by = NULL, plot_xgp = TRUE,
                            palette = "Zissou 1", shapeset=c(15:17, 23:25), ...) {
  labels <- NULL

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
    half_range <<- compute_half_range(half_range, data, centr)
    labels <<- abbreviate(colnames(data), 3)
  }

  if (!is.null(edges)) {
    if (!is.matrix(edges) && ncol(edges) >= 2) {
      stop("Edges matrix needs two columns, from and to, only.")
    }
  }

  render_frame <- function() {
    par(pty = "s", mar = rep(0.1, 4))
  }
  render_transition <- function() {
    rect(-1, -1, 1, 1, col = "#FFFFFFE6", border = NA)
  }
  render_data <- function(data, proj, geodesic) {
    gps <- unique(group_by)
    ngps <- length(gps)

    edge.gps <- unique(edges.col)

    if (ngps > 24) {
      stop("Please choose a group with 24 or less levels.\n")
    }
    grid <- ceiling(sqrt(ngps + 1))
    par(mfrow = c(grid, grid))

    # Render projected points
    x <- data %*% proj
    if (centr) x <- center(x)
    x <- x / half_range

    blank_plot(xlim = c(-1, 1), ylim = c(-1, 1))
    draw_tour_axes(proj, labels, limits = 1, axes)
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

    if (ngps < 2) {
      points(x, col = col, pch = shapes, cex = cex, new = FALSE)
      if (!is.null(edges)) {
        segments(
          x[edges[, 1], 1], x[edges[, 1], 2],
          x[edges[, 2], 1], x[edges[, 2], 2],
          col = edges.col,
          lwd = edges.width
        )
      }
    }
    else {
      for (i in 1:ngps) {
        x.sub <- x[group_by == gps[i], ]
        col.sub <- if (length(col) == nrow(x)) col[group_by == gps[i]] else col
        pch.sub <- if (length(shapes) == nrow(x)) shapes[group_by == gps[i]] else shapes
        cex.sub <- if (length(cex) == nrow(x)) cex[group_by == gps[i]] else cex

        blank_plot(xlim = c(-1, 1), ylim = c(-1, 1))
        if (plot_xgp) {
          points(x[group_by != gps[i], ],
            col = "#DEDEDEDE", new = FALSE,
            shapes = if (length(shapes) > 1) shapes[group_by != gps[i]] else shapes,
            cex = if (length(cex) > 1) cex[group_by != gps[i]] else cex
          )
        }
        points(x.sub, col = col.sub, pch = pch.sub, cex = cex.sub, new = FALSE)

        # This is potentially fragile!
        # It assumes the edge colour groups are the same as the point colour groups
        if (!is.null(edges)) {
          segments(
            x[edges[edges.gps == gps[i], 1], 1], x[edges[edges.gps == gps[i], 1], 2],
            x[edges[edges.gps == gps[i], 2], 1], x[edges[edges.gps == gps[i], 2], 2],
            col = edges.col[edges.gps == gps[i]],
            lwd = edges.width
          )
        }
      }
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

#' @rdname display_groupxy
#' @inheritParams animate
#' @export
animate_groupxy <- function(data, tour_path = grand_tour(), ...) {
  animate(data, tour_path, display_groupxy(...))
  par(mfrow = c(1, 1))
}

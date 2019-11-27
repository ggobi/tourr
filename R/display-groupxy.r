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
#' @param col color to be plotted.  Defaults to "black"
#' @param pch shape of the point to be plotted.  Defaults to 20.
#' @param cex size of the point to be plotted.  Defaults to 1.
#' @param group_by variable to group by. Must have less than 25 unique values.
#' @param plot_xgp if TRUE, plots points from other groups in light grey
#' @param ...  other arguments passed on to \code{\link{animate}} and
#'   \code{\link{display_groupxy}}
#' @export
#' @examples
#' f <- flea[,1:6]
#' col <- rainbow(length(unique(flea$species)))[as.numeric(as.factor(flea$species))]
#' pch <- as.numeric(flea$species)+14
#'
#' animate_groupxy(f, col = col, pch = pch, group_by = flea$species)
#' animate_groupxy(f, col = col, pch = pch, group_by = flea$species, plot_xgp = FALSE)
display_groupxy <- function(centr = TRUE, axes = "center", half_range = NULL,
                            col = "black", pch = 20, cex = 1, edges = NULL,
                            group_by = NULL, plot_xgp = TRUE, ...) {
  labels <- NULL
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
    rect(-1, -1, 1, 1, col="#FFFFFFE6", border=NA)
  }
  render_data <- function(data, proj, geodesic) {

    gps <- unique(group_by)
    ngps <- length(gps)
    if (ngps > 24) {
      stop("Please choose a group with 24 or less levels.\n")
    }
    grid <- ceiling(sqrt(ngps+1))
    par(mfrow=c(grid, grid))

    # Render projected points
    x <- data %*% proj
    if (centr) x <- center(x)
    x <- x / half_range

    blank_plot(xlim = c(-1, 1), ylim = c(-1, 1))
    draw_tour_axes(proj, labels, limits = 1, axes)

    if (ngps < 2) {
      points(x, col = col, pch = pch, cex = cex, new = FALSE)
      if (!is.null(edges)) {
          segments(x[edges[,1], 1], x[edges[,1], 2],
               x[edges[,2], 1], x[edges[,2], 2])
      }
    }
    else {
      for(i in 1:ngps) {
        x.sub <- x[group_by == gps[i],]
        col.sub  <- if (length(col) == nrow(x)) col[group_by == gps[i]] else col
        pch.sub  <- if (length(pch) == nrow(x)) pch[group_by == gps[i]] else pch
        cex.sub  <- if (length(cex) == nrow(x)) cex[group_by == gps[i]] else cex

        blank_plot(xlim = c(-1, 1), ylim = c(-1, 1))
        if (plot_xgp) {
          points(x[group_by != gps[i],], col = "#DEDEDEDE", new = FALSE,
            pch = if (length(pch) > 1) pch[group_by != gps[i]] else pch,
            cex = if (length(cex) > 1) cex[group_by != gps[i]] else cex)
        }
        points(x.sub, col = col.sub, pch = pch.sub, cex = cex.sub,  new = FALSE)

        if (!is.null(edges)) {
          segments(x[edges[group_by == gps[i],1], 1], x[edges[group_by == gps[i],1], 2],
                   x[edges[group_by == gps[i],2], 1], x[edges[group_by == gps[i],2], 2])
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

#' Display 2D tour projections displayed separately by groups
#'
#' This function is designed to allow comparisons across multiple groups,
#'  especially for examining things like two (or more) different models
#'  on the same data. The primary display is a scatterplot, with lines or
#'  contours overlaid.
#'
#' @param axes position of the axes: center, bottomleft or off
#' @param center if TRUE, centers projected data to (0,0).  This pins the
#'  center of data cloud and make it easier to focus on the changing shape
#'  rather than position.
#' @param half_range half range to use when calculating limits of projected.
#'   If not set, defaults to maximum distance from origin to each row of data.
#' @param edges A two column integer matrix giving indices of ends of lines.
#' @param col color to be plotted.  Defaults to "black"
#' @param pch size of the point to be plotted.  Defaults to 20.
#' @param group_by variable to group by. Must have less than 25 unique values.
#' @param plot_xgp if TRUE, plots points from other groups in light grey.
#' @param gp_legend if TRUE, adds a legend of the group at the top left.
#' @param ...  other arguments passed on to \code{\link{animate}} and
#'   \code{\link{display_groupxy}}
#' @export
#' @examples
#' #' animate_groupxy(flea[, 1:6])
#' animate(flea[, 1:6], tour_path=grand_tour(), display=display_groupxy())
#' animate(flea[, 1:6], tour_path=grand_tour(),
#'   display=display_groupxy(half_range = 0.5))
#' animate_groupxy(flea[, 1:6], tour_path=little_tour())
#' animate_groupxy(flea[, 1:3], tour_path=guided_tour(holes), sphere = TRUE)
#' animate_groupxy(flea[, 1:6], center = FALSE)
#'
#' # The default axes are centered, like a biplot, but there are other options
#' animate_groupxy(flea[, 1:6], axes = "off")
#' animate_groupxy(flea[, 1:6], dependence_tour(c(1, 2, 1, 2, 1, 2)) )
#'
#' # Adding color and group
#' require(colorspace)
#' pal <- rainbow_hcl(length(levels(flea$species)))
#' col <- pal[as.numeric(flea$species)]
#' gp = flea$species
#' animate_groupxy(flea[,-7], col=col, group_by=gp)
#'
#' # You can also draw lines
#' edges <- matrix(c(1:5, 2:6), ncol = 2)
#' animate(flea[, 1:6], grand_tour(),
#'   display_groupxy(axes="bottomleft", edges=edges, group_by=gp, col=col))
#'
#' # Point characters, plotting other groups, and plotting legends
#' pch <- as.numeric(flea$species)+14
#' animate_groupxy(f, col = col, pch = pch, group_by = gp)
#' animate_groupxy(f, col = col, pch = pch, group_by = gp, plot_xgp = FALSE)
#' animate_groupxy(f, col = col, pch = pch, group_by = gp, gp_legend = FALSE)
display_groupxy <- function(center = TRUE, axes = "center", half_range = NULL,
                            col = "black", pch  = 20, edges = NULL,
                            group_by = NULL, plot_xgp = TRUE, gp_legend = TRUE,
                            ...) {
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
  if (axes != "center" & axes != "off") {
    message("groupxy only accepts axes = 'center'|'off'.
            Defaulting to axes = 'center.")
    axes <- "center"
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
    if(ngps>24){stop("Choose a group with 24 or less levels.")}
    grid <- ceiling(sqrt(ngps+1))
    par(mfrow=c(grid, grid))

    # Render projected points
    x <- data %*% proj
    if (center) x <- center(x)
    x <- x / half_range

    blank_plot(xlim = c(-1, 1), ylim = c(-1, 1))
    draw_tour_axes(proj, labels, limits = 1, axes)

    if (ngps < 2) {
      points(x, col = col, pch = pch, new = FALSE)
    }
    else {
      for(i in 1:ngps){
        x.sub   <- x[group_by == gps[i],]
        col.sub <- if (length(col) >1) col[group_by == gps[i]] else col
        pch.sub <- if (length(pch) >1) pch[group_by == gps[i]] else pch

        if (!all(is.na(x.sub))) {
          blank_plot(xlim = c(-1, 1), ylim = c(-1, 1))
          if (gp_legend) {
            legend(-1, 1, legend=gps[i], col=col.sub, lty=1, lwd=5)
          }
          if (plot_xgp) {
            xgp.sub <- x[group_by != gps[i],]
            pch.xgp.sub  <- if (length(pch) >1) pch[group_by != gps[i]] else pch
            points(xgp.sub, col = "grey80", new = FALSE, pch = pch.xgp.sub)
          }
          points(x.sub, col = col.sub, pch = pch.sub, new = FALSE)
        }
      }
    }

    if (!is.null(edges)) {
      segments(x[edges[,1], 1], x[edges[,1], 2],
               x[edges[,2], 1], x[edges[,2], 2])
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

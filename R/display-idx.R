#' Display a 1D linear aggregation index
#'
#' Animate a 1D tour path for data where individuals are ranked
#' by a multivariate index. Allows one to examine the sensitivity
#' of the ranking on the linear combination. Variables should be
#' scaled to be between 0-1. This is only designed to work with
#' a local tour, or a radial tour.
#'
#' @param center should 1d projection be centered to have mean zero (default: TRUE).
#'   This pins the centre of distribution to the same place, and makes it
#'   easier to focus on the shape of the distribution.
#' @param half_range half range to use when calculating limits of projected.
#'   If not set, defaults to maximum distance from origin to each row of data.
#' @param abb_vars logical, whether to abbreviate the variable name, if long
#' @param col the color used for points, can be a vector or hexcolors or a
#'   factor, default to "red".
#' @param cex the size used for points, default to 0.5
#' @param panel_height_ratio input to the height argument in
#'   [graphics::layout()] for the height of data and axis panel.
#' @param add_ref_line whether to add a horizontal reference line for each
#' observation, logical default to TRUE
#' @param label the text label, a vector
#' @param label_cex the size for text labels
#' @param label_col the color for text labels
#' @param label_x_pos the x position of text label, currently labels are
#'   positioned at a fixed x value for each observation
#' @param axis_label_cex_upper the size of the axis label in the upper panel
#' @param axis_label_cex_lower the size of the axis label in the lower panel
#' @param axis_bar_col the color of the axis bar
#' @param axis_bar_lwd the width of the axis bar
#' @param axis_bar_label_cex the size of the axis label
#' @param axis_bar_label_col the color of the axis label
#' @param axis_var_cex the size of the variable name
#'   to the right of the axis panel
#' @param axis_var_col the color of the variable name
#'   to the right of the axis panel
#' @param palette name of color palette for point colour, used by
#'   \code{\link{hcl.colors}}, default "Zissou 1"
#' @export
#' @rdname display_idx
#' @examples
#' data(places)
#' places_01 <- apply(places[1:10,1:9], 2, function(x) (x-min(x))/(max(x)-min(x)))
#' b <- matrix(rep(1/sqrt(9), 9), ncol=1)
#' places_init <- cbind(places_01, idx = as.vector(as.matrix(places_01) %*% b))
#' places_sorted <- places_init[order(places_init[,10]), 1:9]
#' animate_idx(places_sorted, tour_path = local_tour(b, angle=pi/8),
#'             label=as.character(places$stnum[1:9]),
#'             label_x_pos = 0)
display_idx <- function(center = FALSE, half_range = NULL, abb_vars = TRUE,
                        col = "red", cex = 3, panel_height_ratio = c(3, 2),
                        label_x_pos = 0.7, label = NULL,
                        label_cex = 1, label_col = "grey80", add_ref_line = TRUE,
                        axis_bar_col = "#000000", axis_bar_lwd = 3,
                        axis_label_cex_upper = 1, axis_label_cex_lower = 1,
                        axis_bar_label_cex = 1, axis_bar_label_col = "#000000",
                        axis_var_cex = 1, axis_var_col = "#000000",
                        palette = "Zissou 1", ...) {
  labels <- NULL
  init <- function(data) {
    half_range <<- compute_half_range(half_range, data, center)
    if (abb_vars) {
      labels <<- abbreviate(colnames(data), 2)
    } else {
      labels <<- colnames(data)
    }
  }
  if (is.factor(col) | !areColors(col)) {
    gps <- col
    lgps <- levels(gps)
    col <- mapColors(col, palette)
  }
  colrs <- unique(col)

  render_frame <- function() {
    # define the plot to have an upper & lower panel, height as 6:1
    layout_m <- matrix(c(1, 2), nrow = 2, ncol = 1)
    graphics::layout(mat = layout_m, heights = panel_height_ratio)
    par(pty = "m", mar = c(1, 4, 1, 1))

  }
  render_transition <- function() {
    rect(-0.1, -1.1, 1.2, 3, col = "#FFFFFFE6", border = NA)
  }
  render_data <- function(data, proj, geodesic) {

    x <- data %*% proj
    if (center) x <- center(x)
    x <- x / half_range
    df <- cbind(x, .y = (3 - 0.1)/nrow(x) * (1:nrow(x)))
    # upper panel: define the axis margin (mgp) and panel margin (mar)
    par(mgp=c(2, 1, 0), mar = c(2, 4, 0.5, 1))
    # initialise the plot box
    plot(
      x = NA, y = NA, xlim = c(-0.1, 1.2), ylim = c(0, 3.05),
      xlab="", ylab = "", xaxs = "i", yaxs = "i", cex = 2,
      xaxt = "n", yaxt = "n"
    )
    axis(1, seq(0, 1, 0.2), cex.axis = axis_label_cex_upper)
    if (add_ref_line) abline(h = df[,".y"], col = "grey60")
    text(x=label_x_pos, y=df[,".y"], labels= label,
         col = label_col, cex = label_cex)
    points(df, col = col, pch = 20, cex = cex)

    # lower panel: define panel margin (mar)
    par(mgp=c(2, 1, 0), mar = c(2.5, 4, 1, 1))
    plot(
      x = NA, y = NA, xlim = c(0, 1.3), ylim = c(-1, 0),
      xlab = "", ylab = "", xaxt = "n", yaxt = "n"
    )
    lines(c(0, 0), c(-1.1, 0), col = "grey60")
    lines(c(1, 1), c(-1.1, 0), col = "grey60")
    axis(1, seq(0, 1, by = 0.2), cex.axis = axis_label_cex_lower)
    box(col = "grey60")

    # Render tour axes
    ax <- (seq_along(proj) - 0.5) / length(proj)
    idx_w <- proj/sum(proj)
    segments(0, -ax, idx_w, -ax, col = axis_bar_col, lwd = axis_bar_lwd)
    text(1.0, -ax, labels, pos = 4, cex = axis_var_cex, col = axis_var_col)
    text(idx_w + 0.01, -ax, format(round(idx_w, 2), nsmall = 2), pos = 4,
         cex = axis_bar_label_cex, col = axis_bar_label_col)
  }

  list(
    init = init,
    render_frame = render_frame,
    render_transition = render_transition,
    render_data = render_data,
    render_target = nul
  )
}

#' @rdname display_idx
#' @inheritParams animate
#' @export
animate_idx <- function(data, tour_path = grand_tour(1), ...) {
  animate(
    data = data, tour_path = tour_path,
    display = display_idx(...), ...
  )
}



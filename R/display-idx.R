#' Display 1D linear aggregation index
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
#' @param label the text label, a vector
#' @param label_cex the size for text labels
#' @param label_col the color for text labels
#' @param label_x_pos the x position of text label, currently labels are
#'   positioned at a fixed x value for each observation
#' @param palette name of color palette for point colour, used by
#'   \code{\link{hcl.colors}}, default "Zissou 1"
#' @export
#' @rdname display_idx
display_idx <- function(center = FALSE, half_range = NULL, abb_vars = TRUE,
                        col = "red", cex = 0.5, label_x_pos = 0.7,
                        label = NULL, label_cex = 1, label_col = "grey80",
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
    graphics::layout(mat = layout_m, heights = c(6,1))
    par(pty = "m", mar = c(1, 4, 1, 1))

  }
  render_transition <- function() {
    rect(-1, -1.1, 1.2, 3, col = "#FFFFFFE6", border = NA)
  }
  render_data <- function(data, proj, geodesic) {

    x <- data %*% proj
    if (center) x <- center(x)
    x <- x / half_range
    df <- cbind(x, .y = 3/nrow(x) * (1:nrow(x)))
    # upper panel: define the axis margin (mgp) and panel margin (mar)
    par(mgp=c(1, 0.3, 0), mar = c(0.5, 4, 0.5, 1))
    # initialise the plot box
    plot(
      x = NA, y = NA, xlim = c(0, 1.2), ylim = c(-0.05, 3.05),
      xlab="", ylab = "", xaxs = "i", yaxs = "i",
      xaxt = "n", yaxt = "n"
    )
    axis(1, seq(0, 1, 0.2), cex.axis = 0.8)
    abline(h = seq(0, 3, by = 0.5), col = "grey80")
    text(x=label_x_pos, y=df[,".y"], labels= label, col = label_col, cex = label_cex)
    points(df, col = col, pch = 20, cex = cex)

    # lower panel: define panel margin (mar)
    par(mar = c(2, 4, 1, 1))
    plot(
      x = NA, y = NA, xlim = c(0, 1.2), ylim = c(-1.1, 0),
      xlab = "", ylab = "Weights", cex.lab = 2,
      xaxt = "n", yaxt = "n"
    )
    lines(c(0, 0), c(-1.1, 0), col = "grey80")
    lines(c(1, 1), c(-1.1, 0), col = "grey80")
    axis(1, seq(0, 1, by = 0.2), cex.axis = 0.8)
    box(col = "grey70")

    # Render tour axes
    ax <- seq_along(proj) / length(proj)
    idx_w <- proj/sum(proj)
    segments(0, -ax, idx_w, -ax, col = "black", lwd = 3)
    text(1.0, -ax, labels, pos = 4, cex = 2)
    text(idx_w + 0.01, -ax, round(idx_w,2), pos = 4, cex = 2)
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

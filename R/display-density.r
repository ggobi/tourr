#' 1d distribution tour path animation.
#'
#' Animate a 1d tour path with a density plot or histogram.
#'
#' @param method display method, histogram or density plot
#' @param center should 1d projection be centered to have mean zero (default: TRUE).
#'   This pins the centre of distribution to the same place, and makes it
#'   easier to focus on the shape of the distribution.
#' @param half_range half range to use when calculating limits of projected.
#'   If not set, defaults to maximum distance from origin to each row of data.
#' @param col color to use for points, can be a vector or hexcolors or a factor.  Defaults to "black".
#' @param rug draw rug plot showing position of actual data points?
#' @param palette name of color palette for point colour, used by \code{\link{grDevices::hcl.colors}}, default "Zissou 1"
#' @param density_max allow control of the y range for density plot
#' @param ... other arguments passed on to \code{\link{animate}}
#' @seealso \code{\link{animate}} for options that apply to all animations
#' @keywords hplot
#' @export
#' @examples
#' animate_dist(flea[, 1:6])
#'
#' animate_dist(flea[, 1:6], col=flea$species, density_max=5)
#'
#' # When the distribution is not centred, it tends to wander around in a
#' # distracting manner
#' animate_dist(flea[, 1:6], center = FALSE)
#'
#' # Alternatively, you can display the distribution with a histogram
#' animate_dist(flea[, 1:6], method = "hist")
display_dist <- function(method = "density", center = TRUE, half_range = NULL,
                         col = "black", rug = FALSE,
                         palette = "Zissou 1",
                         density_max = 3, ...) {
  if (!requireNamespace("ash", quietly = TRUE)) {
    stop("Please install the ash package", call. = FALSE)
  }

  method <- match.arg(method, c("histogram", "density", "ash"))
  labels <- NULL
  init <- function(data) {
    half_range <<- compute_half_range(half_range, data, center)
    labels <<- abbreviate(colnames(data), 2)
  }

  # If colors are a variable, convert to colors
  if (is.factor(col) | !areColors(col)) {
    gps <- col
    col <- mapColors(col, palette)
  }
  colrs <- unique(col)
  ngps <- length(colrs)

  render_frame <- function() {
    if (ngps > 5) {
      stop("Please choose a group with 5 or less levels.\n")
    }

    if (ngps == 1)
      par(pty = "m", mar = c(4, 4, 1, 1), mfrow = c(2, 1))
    else {
      #if (method != "density")
        par(pty = "m", mar = c(4, 4, 1, 1), mfrow = c(ngps+1, 1))
      #else
      #  par(pty = "m", mar = c(4, 4, 1, 1))
    }

  }
  render_transition <- function() {
    rect(-1, -1.1, 1.2, density_max, col = "#FFFFFFE6", border = NA)
  }
  render_data <- function(data, proj, geodesic) {

    x <- data %*% proj
    if (center) x <- center(x)
    x <- x / half_range

    # Render projection data
    if (method == "histogram") {
      if (ngps == 1) {
        par(mar = c(0, 4, 0, 1))
        plot(
          x = NA, y = NA, xlim = c(-1, 1.2), ylim = c(0, density_max),
          xlab="", ylab = "Density",
          xaxt = "n", yaxt = "n"
        )
        axis(2, seq(0, density_max, by = 1))
        abline(h = seq(0.5, density_max, by = 0.5), col = "grey80")

        bins <- hist(x, breaks = seq(-1, 1, 0.2), plot = FALSE)
        with(bins, rect(mids - 0.1, 0, mids + 0.1, density,
          col = "black", border = "white"
        ))
        if (rug) {
          segments(x, 0, x, 0.1, ...)
        }

      }
      else {

        for (i in 1:ngps) {
          par(mar = c(0, 4, 0, 1))
          plot(
            x = NA, y = NA, xlim = c(-1, 1.2), ylim = c(0, density_max),
            xlab="", ylab = "Density",
            xaxt = "n", yaxt = "n"
          )
          axis(2, seq(0, density_max, by = 1))
          abline(h = seq(0.5, density_max, by = 0.5), col = "grey80")

          x.sub <- x[col == colrs[i], ]
          bins <- hist(x.sub, breaks = seq(-1, 1, 0.2), plot = FALSE)
          with(bins, rect(mids - 0.1, 0, mids + 0.1, density,
                          col = colrs[i], border = "white"
          ))
          if (rug) {
            segments(x, 0, x, 0.1, ...)
          }

        }
      }
    } else if (method == "density") {
      if (ngps == 1) {
        par(mar = c(0, 4, 0, 1))
        plot(
          x = NA, y = NA, xlim = c(-1, 1.2), ylim = c(0, density_max),
          xlab="", ylab = "Density",
          xaxt = "n", yaxt = "n"
        )
        axis(2, seq(0, density_max, by = 1))
        abline(h = seq(0.5, density_max, by = 0.5), col = "grey80")

        polygon(stats::density(x), lwd = 2, col = "black")
        if (rug) {
          segments(x, 0, x, 0.1, ...)
        }
      }
      else {
        for (i in 1:ngps) {
          par(mar = c(0, 4, 0, 1))
          plot(
            x = NA, y = NA, xlim = c(-1, 1.2), ylim = c(0, density_max),
            xlab="", ylab = "Density",
            xaxt = "n", yaxt = "n"
          )
          axis(2, seq(0, density_max, by = 1))
          abline(h = seq(0.5, density_max, by = 0.5), col = "grey80")

          x.sub <- x[col == colrs[i], ]
          polygon(stats::density(x.sub), lwd = 2, col = colrs[i])
          if (rug) {
            segments(x, 0, x, 0.1, col = colrs[i], ...)
          }
        }
      }
    } else if (method == "ash") {
      if (ngps == 1) {
        par(mar = c(0, 4, 0, 1))
        plot(
          x = NA, y = NA, xlim = c(-1, 1.2), ylim = c(0, density_max),
          xlab="", ylab = "Density",
          xaxt = "n", yaxt = "n"
        )
        axis(2, seq(0, density_max, by = 1))
        abline(h = seq(0.5, density_max, by = 0.5), col = "grey80")

        utils::capture.output(ash <- ash::ash1(ash::bin1(x, c(-half_range, half_range))))
        lines(ash)
        if (rug) {
          segments(x, 0, x, 0.1, ...)
        }
      }
      else {
        for (i in 1:ngps) {
          par(mar = c(0, 4, 0, 1))
          plot(
            x = NA, y = NA, xlim = c(-1, 1.2), ylim = c(0, density_max),
            yaxs = "i",
            xlab="", ylab = "Density",
            xaxt = "n", yaxt = "n"
          )
          axis(2, seq(0, 4, by = 1))
          abline(h = seq(0.5, density_max, by = 0.5), col = "grey80")

          utils::capture.output(ash <- ash::ash1(ash::bin1(x, c(-half_range, half_range))))
          lines(ash)
          if (rug) {
            segments(x, 0, x, 0.1, col = colrs[i], ...)
          }

        }
      }
    }
    par(mar = c(4, 4, 0, 1))
    plot(
      x = NA, y = NA, xlim = c(-1, 1.2), ylim = c(-1.1, 0),
      xaxs = "i", yaxs = "i",
      xlab = "Data Projection", ylab = "",
      yaxt = "n"
    )
    lines(c(0, 0), c(-1, 0), col = "grey80")
    lines(c(-1, -1), c(-1.1, 0), col = "grey80")
    lines(c(1, 1), c(-1.1, 0), col = "grey80")
    abline(h = 0)
    box(col = "grey70")

    # Render tour axes
    ax <- seq_along(proj) / length(proj)
    segments(0, -ax, proj, -ax, col = "black", lwd = 3)
    text(1.0, -ax, labels, pos = 4)
  }

  list(
    init = init,
    render_frame = render_frame,
    render_transition = render_transition,
    render_data = render_data,
    render_target = nul
  )
}


#' @rdname display_dist
#' @inheritParams animate
#' @export
animate_dist <- function(data, tour_path = grand_tour(1), ...) {
  animate(
    data = data, tour_path = tour_path,
    display = display_dist(...), ...
  )
}

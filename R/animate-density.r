#' 1d distribution tour path animation.
#'
#' Animate a 1d tour path with a density plot or histogram.
#'
#' @param data matrix, or data frame containing numeric columns
#' @param tour_path tour path generator, defaults to the grand tour
#' @param method display method, histogram or density plot
#' @param center should 1d projection be centered to have mean zero (default: TRUE).
#'   This pins the centre of distribution to the same place, and makes it
#'   easier to focus on the shape of the distribution.
#' @param ... other arguments passed on to \code{\link{animate}}
#' @seealso \code{\link{animate}} for options that apply to all animations
#' @keywords hplot
#' 
#' @examples
#' animate_dist(flea[, 1:6])
#'
#' # When the distribution is not centred, it tends to wander around in a 
#' # distracting manner
#' animate_dist(flea[, 1:6], center = FALSE)
#'
#' # Alternatively, you can display the distribution with a histogram
#' animate_dist(flea[, 1:6], method = "hist")
animate_dist <- function(data, tour_path = grand_tour(1), method="density", center = TRUE, ...) {
  labels <- abbreviate(colnames(data), 2)
  method <- match.arg(method, c("histogram", "density", "ash"))
  
  # Display 
  range <- c(-2, 2)
  render_frame <- function() {
    par(pty="m",mar=c(4,4,1,1))
    plot(
      x = NA, y = NA, xlim = range, ylim = c(-1.1,4), xaxs="i", yaxs="i",
      xlab = "Data Projection", ylab = "Density", yaxt = "n"
    )
    axis(2, seq(0, 4, by = 1))
  }
  render_transition <- function() {
    # rect(-1.99, -1.99, 1.99, 5, col="grey80", border=NA)
  }
  render_data <- function(data, proj, geodesic) {
    render_frame()
    abline(h = seq(0.5, 3.5, by=0.5), col="white")
    lines(c(0,0), c(-1,0), col="white")
    lines(c(-1,-1), c(-1,0), col="white")
    lines(c(1,1), c(-1,0), col="white")

    x <- data%*%proj
    if (center) x <- scale(x, center = TRUE, scale = FALSE)
    
    # Render projection data
    if (method == "histogram") {
      bins <- hist(x, breaks = seq(-2, 2, 0.2), plot = FALSE)
      with(bins, rect(mids - 0.1, 0, mids + 0.1, density,
          col="black", border="white"))
    } else if (method == "density") {
      polygon(density(x), lwd = 2, col="black")
    } else if (method == "ash") {
      library(ash)
      capture.output(ash <- ash1(bin1(x, range(x))))
      lines(ash)
    }
    abline(h = 0)
    box(col="grey70")
    
    # Render tour axes
    for (i in 1:length(proj)) {
      x <- i / length(proj)
      lines(c(0, proj[i]), c(-x, -x), col="black", lwd=3)
      text(1, -x, labels[i], pos=4)
    }
  }
  render_target <- function(target, geodesic) {
    rect(-1.99, -1.99, 1.99, 5, col="#7F7F7F33", border=NA)
  }

  animate(
    data = data, tour_path = tour_path,
    render_frame = render_frame, render_data = render_data,
    render_transition = render_transition, render_target = render_target, 
    ...
  )
}

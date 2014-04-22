#' Render frames of animation to disk
#'
#' @param data matrix, or data frame containing numeric columns
#' @param tour_path tour path generator
#' @param display the method used to render the projected data,
#'   e.g. \code{\link{display_xy}}, \code{\link{display_pcp}}
#' @param dev name of output device to use (e.g. \code{\link{png}},
#'   \code{\link{pdf}})
#' @param ... other options passed to output device
#' @param apf angle (in radians) per frame
#' @param frames number of frames in output
#' @param rescale if true, rescale all variables to range [0,1]
#' @param sphere if true, sphere all variables
#' @param start starting projection.  If \code{NULL}, uses path default.
#' @keywords hplot
#' @export
#' @references Hadley Wickham, Dianne Cook, Heike Hofmann, Andreas Buja
#'   (2011). tourr: An R Package for Exploring Multivariate Data with
#'   Projections. Journal of Statistical Software, 40(2), 1-18.
#'   \url{http://www.jstatsoft.org/v40/i02/}.
#' @examples
#' render(flea[, 1:4], grand_tour(), display_xy(), "pdf", "test.pdf")
#' render(flea[, 1:4], grand_tour(), display_xy(), "png", "test-%03d.png")
render <- function(data, tour_path, display, dev, ..., apf = 1/10, frames = 50, rescale = TRUE, sphere = FALSE, start = NULL) {
  if (rescale) data <- rescale(data)
  if (sphere) data  <- sphere(data)

  dev <- match.fun(dev)
  dev(...)
  on.exit(dev.off())

  tour <- new_tour(data, tour_path, start)
  step <- tour(0)

  display$init(data)

  i <- 0
  while(i < frames) {
    display$render_frame()
    display$render_data(data, step$proj, step$target)

    i <- i + 1
    step <- tour(apf)
    if (is.null(step)) return(invisible())
  }
  invisible()
}

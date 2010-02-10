#' Render frames of animation to disk
#' 
#' @param data matrix, or data frame containing numeric columns
#' @param tour_path tour path generator
#' @param display takes the display that is suppose to be used, defaults to
#'   the xy display
#' @param dev name of output device to use (e.g. \code{\link{png}},
#'   \code{\link{pdf}})
#' @param ... other options passed to output device
#' @param apf angle (in radians) per frame
#' @param frames number of frames in output
#' @param rescale if true, rescale all variables to range [0,1]
#' @param sphere if true, sphere all variables
#' @param start starting projection.  If \code{NULL}, uses path default.
#' @keywords hplot
#' @examples
#' render(flea[, 1:4], grand_tour(), display_xy(), "pdf", "test.pdf")
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
  }
}
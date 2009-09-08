#' Animate a tour path.
#'
#' This is the function that powers all of the tour animations.  If you want
#' to write your own tour animation method, the best place to 
#' start is by looking at the code for animation methods that have already 
#' implemented in the package.
#'
#' Animations can be rendered on screen, or saved to disk.  Saving an
#' animation to disk allows you to recreate a movie that is much smoother, but
#' takes considerably more time to generate.
#'
#' @param data matrix, or data frame containing numeric columns
#' @param tour_path tour path generator, defaults to the grand tour
#' @param display takes the display that is suppose to be used, defaults to the xy display
#' @param aps target angular velocity (in radians per second)
#' @param fps target frames per second (defaults to 30)
#' @param max_frames the maximum number of bases to generate.  Defaults to
#'   Inf for interactive use (must use Ctrl + C to terminate), and 1 for 
#'   non-interactive use.  It's recommended that you set this value to a 
#'   finite number if you are saving the animation to disk.
#' @param render_frame function called once at the beginning of the animation
#'   to set up the plotting surface.  This function has no arguments.
#' @param ... other arguments are based on to the tour path generator
#' @param rescale if true, rescale all variables to range [0,1]?
#' @param sphere if true, sphere all variables
#' @param file if specified, will save frames to disk instead of displaying on
#'   screen.  Can be of the format "Rplot\%03d.png"
#' @param dev output device to use (e.g. \code{\link{png}}, \code{\link{pdf}})
#' @param dev.settings a list of other options to use when initialising output
#'   device
#' @examples 
#' f <- flea[, 1:6]
#' animate(f, grand_tour(), display_xy())
#' # or in short
#' animate(f)
#' animate(f, max_frames = 30)
#' animate(f, max_frames = 10, fps = 1, aps = 0.1)
#'
#' animate_xy(f, max_frames = 100, file = "test.pdf", dev = pdf)
animate <- function(data, tour_path = grand_tour(), display = display_xy() , aps = 1, fps = 30, max_frames = Inf, ..., rescale = TRUE, sphere = FALSE) {
  if (rescale) data <- rescale(data)
  if (sphere) data  <- sphere(data)
  
  # Display on screen
  display$init(data)
  display$render_frame()
  
  if (find_platform()$os == "win") {
    step <- function(step, proj, geodesic) {
      display$render_frame()
      display$render_data(data, proj, geodesic)
      Sys.sleep(1 / fps)
    }          
  } else {
    step <- function(step, proj, geodesic) {
      display$render_transition()
      display$render_data(data, proj, geodesic)
      Sys.sleep(1 / fps)
    }      
  }
  
  # By default, only take single step if not interactive
  if (!interactive() && missing(max_frames)) {
    max_frames <- 1
  }
  if (max_frames == Inf) {
    to_stop()
  }
  
  tour(
    data = data, tour_path = tour_path, 
    velocity = aps / fps, total_steps = max_frames,
    step_fun = step, target_fun = display$render_target, ...
  )
}


#' Internal Function
#' 
#' @keywords internal
render <- function(data, tour_path, display, path, apf = 1/30, frames = 100) {
  do.call(dev, c(list(file = file), dev.settings))
  on.exit(dev.off())

  step <- function(step, proj, geodesic) {
    render_frame()
    render_data(data, proj, geodesic)
  }
}
#' Animate a tour path.
#'
#' This is the function that powers all of the tour animations.  Generally
#' you should not have to call this function directly, unless you are creating
#' your own tour method.  However, all of the animation methods have ... 
#' arguments that are passed on to this function, and it is useful to know
#' what these are in order to control many aspects of the animation behaviour.
#'
#' If you want to write your own tour animation method, the best place to 
#' start is by looking at the code for animation methods that have already 
#' implemented in the package.
#'
#' Animations can be rendered on screen, or saved to disk.  Saving an
#' animation to disk allows you to recreate a movie that is much smoother, but
#' takes considerably more time to generate.
#'
#' @param data matrix, or data frame containing numeric columns
#' @param tour_path tour path generator, defaults to the grand tour
#' @param aps target angular velocity (in radians per second)
#' @param fps target frames per second (defaults to 30)
#' @param max_frames the maximum number of bases to generate.  Defaults to
#'   Inf for interactive use (must use Ctrl + C to terminate), and 1 for 
#'   non-interactive use.  It's recommended that you set this value to a 
#'   finite number if you are saving the animation to disk.
#' @param render_frame function called once at the beginning of the animation
#'   to set up the plotting surface.  This function has no arguments.
#' @param render_target function called whenever new target projection is 
#'   generated
#' @param render_data function called after every new projection is generated
#'   to render the data.  The function has three arguments: the data, the
#'   the projection matrix and the geodesic path 
#'  (see \code{\link{geodesic_path}}) for more details.
#' @param render_transition function called before rendering data.  This is
#'   typically used to draw a transparent rectangle over the previous data to
#'   preserve some continuity.  It is not used when saving to disk.
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
#' animate_xy(f)
#' animate_xy(f, max_frames = 30)
#' animate_xy(f, max_frames = 10, fps = 1, aps = 0.1)
#'
#' animate_xy(f, max_frames = 100, file = "test.pdf", dev = pdf)
animate <- function(data, tour_path, aps = 1, fps = 30, max_frames = Inf, render_frame = nul, render_target = nul, render_data = nul, render_transition = nul, ..., rescale = TRUE, sphere = FALSE, file = NULL, dev = NULL, dev.settings = list()) {
  if (rescale) data <- rescale(data)
  if (sphere) data  <- sphere(data)
  
  if (is.null(file)) {
    # Display on screen
    render_frame()
    step <- function(step, proj, geodesic) {
      render_transition()
      render_data(data, proj, geodesic)
      Sys.sleep(1 / fps)
    }  
  } else {
    # Save to disk
    do.call(dev, c(list(file = file), dev.settings))
    on.exit(dev.off())

    step <- function(step, proj, geodesic) {
      render_frame()
      render_data(data, proj, geodesic)
    }
    
    render_target <- nul
  }
  
  # By default, only take single step if not interactive
  if (!interactive() && missing(max_frames)) {
    max_frames <- 1
  }
  if (max_frames == Inf) {
    message("Press Ctrl+C to stop tour runnning\n")
  }
  
  tour(
    data = data, tour_path = tour_path, 
    velocity = aps / fps, total_steps = max_frames,
    step_fun = step, target_fun = render_target, ...
  )
}

animate2 <- function(tour, aps = 1, fps = 30, max_frames = Inf, ..., rescale = TRUE, sphere = FALSE) {
  if (rescale) data <- rescale(data)
  if (sphere) data  <- sphere(data)
  
  # Display on screen
  display$init(data)
  display$render_frame()
  step <- function(step, proj, geodesic) {
    display$render_transition()
    display$render_data(data, proj, geodesic)
    Sys.sleep(1 / fps)
  }  
  
  # By default, only take single step if not interactive
  if (!interactive() && missing(max_frames)) {
    max_frames <- 1
  }
  if (max_frames == Inf) {
    message("Press Ctrl+C to stop tour runnning\n")
  }
  
  tour(
    data = data, tour_path = tour_path, 
    velocity = aps / fps, total_steps = max_frames,
    step_fun = step, target_fun = display$render_target, ...
  )
}

render <- function(data, tour_path, display, path, apf = 1/30, frames = 100) {
  
}
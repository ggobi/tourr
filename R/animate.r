#' Animate a tour path
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
#' @param tourf tour path generator, defaults to the grand tour
#' @param d number of target dimensions
#' @param aps target angular velocity (in radians per second)
#' @param fps target frames per second (defaults to 30)
#' @param max_frames the maximum number of frames to generate.  Defaults to
#'   Inf for interactive use (must use Ctrl + C to terminate), and 1 for 
#'   non-interactive use.  It's recommended that you set this value to a 
#'   finite number if you are saving the animation to disk.
#' @param start starting projection, if omitted will use random projection
#' @param render_frame function called once at the beginning of the animation
#'   to set up the plotting surface.  This function has no arguments.
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
#'   screen.  Can be of the format "Rplot%03d.png"
#' @param dev output device to use (e.g. \code{\link{png}}, \code{\link{pdf}})
#' @param dev.setting a list of other options to use when initialising output
#'   device
#' @examples 
#' f <- flea[, 1:6]
#' animate_xy(f)
#' animate_xy(f, max_frames = 30)
#' animate_xy(f, max_frames = 10, fps = 1, aps = 0.1)
#'
#' animate_xy(f, max_frames = 100, file = "test.pdf", dev = pdf)
animate <- function(data, tourf, d, aps = 1, fps = 30, max_frames = Inf, start = NULL, render_frame, render_target, render_data, render_transition, ..., rescale = TRUE, sphere = FALSE, file = NULL, dev = NULL, dev.settings = list()) {
  if (rescale) data <- rescale(data)
  if (sphere) data  <- sphere(data)
  
  if (is.null(start)) {
    start <- matrix(0, nrow = ncol(data), ncol = d)
    diag(start) <- 1    
  }
  
  if (is.null(file)) {
    render_frame()
    step <- function(step, proj, geodesic) {
      render_transition()
      render_data(data, proj, geodesic)
      Sys.sleep(1 / fps)
    }  
    
  } else {
    do.call(dev, c(list(file = file), dev.settings))
    on.exit(dev.off())

    step <- function(step, proj, geodesic) {
      render_frame()
      render_data(data, proj, geodesic)
    }
  }
  
  if (!interactive() && max_frames == Inf) {
    max_frames <- 1
  }
  if (max_frames == Inf) {
    message("Press Ctrl+C to stop tour runnning\n")
  }
  
  tourf(
    start, velocity = aps / fps, 
    step_fun = step, target_fun = render_target, 
    total_steps = max_frames, ..., data = data
  )
}

# Setting up to be ready to display data projections
blank_plot <- function(...) {
  plot(
    x = NA, y = NA, xlab = "", ylab = "",
    axes = FALSE, frame = TRUE, xaxs = "i", yaxs = "i",
    ...
  )  
}

#' Animate a tour path.
#'
#' This is the function that powers all of the tour animations.  If you want
#' to write your own tour animation method, the best place to
#' start is by looking at the code for animation methods that have already
#' implemented in the package.
#'
#' See \code{\link{render}} to render animations to disk.
#'
#' @param data matrix, or data frame containing numeric columns
#' @param tour_path tour path generator, defaults to 2d grand tour
#' @param start projection to start at, if not specified, uses default
#'   associated with tour path
#' @param display takes the display that is suppose to be used, defaults to
#'   the xy display
#' @param aps target angular velocity (in radians per second)
#' @param fps target frames per second (defaults to 15, to accommodate RStudio graphics device)
#' @param max_frames the maximum number of bases to generate.  Defaults to
#'   Inf for interactive use (must use Ctrl + C to terminate), and 1 for
#'   non-interactive use.
#' @param rescale Default FALSE. If TRUE, rescale all variables to range [0,1]?
#' @param sphere if true, sphere all variables
#' @param ... ignored
#' @return an (invisible) list of bases visited during this tour
#' @export
#' @examples
#' f <- flea[, 1:6]
#' animate(f, grand_tour(), display_xy())
#' # or in short
#' animate(f)
#' animate(f, max_frames = 30)
#' \donttest{
#' animate(f, max_frames = 10, fps = 1, aps = 0.1)
#' }
animate <- function(data, tour_path = grand_tour(), display = display_xy(),
                    start = NULL, aps = 1, fps = 10, max_frames = Inf,
                    rescale = FALSE, sphere = FALSE, ...) {
  record <-
    tibble::tibble(
      basis = list(),
      index_val = numeric(),
      info = character(),
      method = character(),
      alpha = numeric(),
      tries = numeric(),
      loop = numeric()
    )
  if (!is.matrix(data)) {
    message("Converting input data to the required matrix format.")
    data <- as.matrix(data)
  }

  if (rescale) data <- rescale(data)
  if (sphere) data <- sphere_data(data)

  # By default, only take single step if not interactive
  # This is useful for the automated tests run by R CMD check
  if (!interactive() && missing(max_frames)) {
    max_frames <- 1
  }
  if (max_frames == Inf) {
    to_stop()
  }
  plat <- find_platform()
  if (rstudio_gd() && fps > 19) {
    warning("Rstudio graphics device supports maximum fps of 19", call. = FALSE)
    fps <- 19
  }

  tour <- new_tour(data, tour_path, start, ...)
  start <- tour(0, ...)
  bs <- 1
  bases <- array(NA, c(ncol(data), ncol(start$target), bs))

  # Initialise display
  display$init(data)
  display$render_frame()
  display$render_data(data, start$proj)

  b <- 0
  i <- 0

  tryCatch(
    {
      while (i < max_frames) {
        i <- i + 1
        step <- tour(aps / fps, ...)
        if (step$step == 1) {
          b <- b + 1
          if (b > bs) {
            bases <- c(bases, rep(NA, bs * dim(bases)[1] * dim(bases)[2]))
            dim(bases) <- c(ncol(data), ncol(start$target), 2 * bs)
            bs <- 2 * bs
          }
          bases[, , b] <- step$target
        }

        dev.hold()
        on.exit(dev.flush())
        if (plat$os == "win" || plat$iface == "rstudio") {
          display$render_frame()
        } else {
          display$render_transition()
        }
        display$render_data(data, step$proj, step$target)
        dev.flush()
        if (step$step < 0) break # break after rendering final projection
        Sys.sleep(1 / fps)
      }
    },
    interrupt = function(cond) {
      dev.flush()
      return()
    }
  )

  if (b != 0) {
    invisible(bases[, , seq_len(b)])
  }

  invisible(record)

  # Need a better way to clean up global variables than this
  # suppressWarnings(rm(tries, cur_index, current, t0, record, envir = globalenv()))
}

rstudio_gd <- function() identical(names(dev.cur()), "RStudioGD")

# globalVariables("record")

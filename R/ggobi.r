#' Use ggobi to display a tour path.
#'
#' Prompts R to use rggobi as the window for the plotting
#'
#' @param data matrix, or data frame containing numeric columns
#' @param tour_f function sent to perform the tour
#' @param aps 
#' @param fps  how many frames per second should be used
#' @param ... other arguments passed on to \code{\link{animate}}
#' @param rescale boolean as to put standardized data into ggobi (a transformation)
#' @param sphere boolean as to put principle components into ggobi (a transformation)
#' @examples
#'  ggobi_tour(flea[,1:6])
ggobi_tour <- function(data, tour_f = grand_tour, aps = 1, fps = 30, ..., rescale = TRUE, sphere = FALSE) {
  if(!require("rggobi", quiet = TRUE)) {
    stop("rggobi required for ggobi based tour")
  }
  
  # Start with plot of first two variables
  start <- matrix(0, nrow = ncol(data), ncol = 2)
  diag(start) <- runif(2)
  
  if (rescale) data <- rescale(data)
  if (sphere) data  <- sphere(data)

  # Display
  g <- ggobi(data)
  message("Pause the tour in GGobi to allow R control to begin")
  # gd <- display(g$data, "2D Tour")
  gd <- displays(g)[[1]]
  pmode(gd) <- "2D Tour"
  
  update_plot <- function(step, proj, geodesic) {
    Sys.sleep(1 / fps)
    ggobi_display_set_tour_projection(gd, proj)
  }

  cat("Press Ctrl+C to stop tour runnning\n")
  tour_f(start, velocity = aps / fps, total_steps = Inf, step_fun = update_plot, ...)
}


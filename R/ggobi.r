# library(rggobi)
#X  display_ggobi(flea[,1:6])

display_ggobi <- function(data, edges = NULL, tour_f = grand_tour, aps = 1, fps = 30, ..., rescale = TRUE, sphere = FALSE) {
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
  cat("Pause the tour in GGobi to allow R control to begin\n")
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


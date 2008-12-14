# Also needs to be able to save to disk
# This function runs the selected tour and displays with R in the chosen way.
animate <- function(data, tourf, d, aps = 1, fps = 30, start = NULL, render_frame, render_target, render_data, render_transition, ..., rescale = TRUE, sphere = FALSE, file = NULL, dev = NULL, dev.settings = list()) {
  if (rescale) data <- rescale(data)
  if (sphere) data  <- sphere(data)
  
  if (is.null(start)) {
    start <- matrix(0, nrow = ncol(data), ncol = d)
    diag(start) <- 1    
  }
  
  if (is.null(file)) {
    render_frame()
    step <- function(step, proj, geodesic) {
      Sys.sleep(1 / fps)
      render_transition()
      render_data(data, proj, geodesic)
    }  
    
  } else {
    do.call(dev, c(list(file = file), dev.settings))
    on.exit(dev.off())

    step <- function(step, proj, geodesic) {
      render_frame()
      render_data(data, proj, geodesic)
    }
  }
  
  cat("Press Ctrl+C to stop tour runnning\n")
  tourf(
    start, velocity = aps / fps, 
    step_fun = step, target_fun = render_target, 
    total_steps = Inf, ..., data = data
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

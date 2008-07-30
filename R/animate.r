# Also needs to be able to save to disk
animate <- function(data, tourf, d, aps = 1, fps = 30, start = NULL, render_frame, render_target, render_data, render_transition, ..., rescale = TRUE, sphere = FALSE, file = NULL, dev = NULL, dev.settings = list()) {
  if (rescale) data <- rescale(data)
  if (sphere) data  <- sphere(data)
  
  if (is.null(start)) {
    start <- matrix(0, nrow = ncol(data), ncol = d)
    diag(start) <- 1    
  }
  
  if (is.null(file)) {
    render_frame()
    step <- function(step, proj) {
      Sys.sleep(1 / fps)
      render_transition()
      render_data(data, proj)
    }  
    
  } else {
    do.call(dev, c(list(file = file), dev.settings))
    on.exit(dev.off())

    step <- function(step, proj) {
      render_frame()
      render_data(data, proj)
    }
  }
  
  cat("Press Ctrl+C to stop tour runnning\n")
  tourf(
    start, velocity = aps / fps, 
    step_fun = step, target_fun = render_target, 
    total_steps = Inf, ..., data = data
  )
}



rescale <- function(df) {
  apply(df, 2, function(x) (x - min(x)) / diff(range(x)))
}

sphere <- function(df) {
  predict(princomp(df))
}

blank_plot <- function(...) {
  plot(
    x = NA, y = NA, xlab = "", ylab = "",
    axes = FALSE, frame = TRUE, xaxs = "i", yaxs = "i",
    ...
  )  
}
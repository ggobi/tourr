# Code to display the tour using R and GGobi graphics

#X r_tour(mtcars[, 1:5])
#X r_tour(mtcars[, 1:5], little_tour)
r_tour <- function(data, tourf = grand_tour, d=2, aps = 1, fps = 30, ...) {
  # Standardise data
  data <- apply(data, 2, function(x) (x - min(x)) / diff(range(x)))
  labels <- abbreviate(colnames(data), 2)
  
  # Start with plot of first two variables
  start <- matrix(0, nrow = ncol(data), ncol = 2)
  diag(start) <- 1
  
  # Display 
  range <- c(-2, 2)
  par(pch = "s")
  plot(NA, NA,xlim=range, ylim=range, xlab="", ylab="", axes=FALSE, frame=TRUE, xaxs="i", yaxs="i")
  step <- function(step, proj) {
    Sys.sleep(1 / fps)
    rect(-1.99, -1.99, 1.99, 1.99, col="#FFFFFFE6", border=NA)
    
    # Draw tour axes
    segments(0, 0, proj[, 1], proj[, 2], col="grey50")
    theta <- seq(0, 2 * pi, length = 50)
    lines(cos(theta), sin(theta), col="grey50")
    text(proj, label = labels, col="grey50")

    # Draw projected points
    points(data %*% proj, pch=20)
  }
  target <- function(target) {
    rect(-1.99, -1.99, 1.99, 1.99, col="#7F7F7F33", border=NA)
  }

  cat("Press Ctrl+C to stop tour runnning\n")
  tourf(start, velocity = aps / fps, step_fun = step, target_fun = target, total_steps = Inf, ..., data=data)
}



ggobi_tour <- function(data, tour = grand_tour, aps = 1, fps = 100, ...) {
  if(!require("rggobi", quiet = TRUE)) {
    stop("rggobi required for ggobi based tour")
  }
  
  # Start with plot of first two variables
  start <- matrix(0, nrow = ncol(data), ncol = 2)
  diag(start) <- runif(2)
    
  # Display
  data <- apply(data, 2, function(x) (x - min(x)) / diff(range(x)))
  g <- ggobi(data)
  gd <- display(g$data, "2D Tour")
  
  update_plot <- function(step, proj) {
    Sys.sleep(1 / fps)
    ggobi_display_set_tour_projection(gd, proj)
  }

  cat("Pause the tour in GGobi to allow R control to begin\n")
  cat("Press Ctrl+C to stop tour runnning\n")
  tour(start, velocity = aps / fps, total_steps = Inf, step_fun = update_plot, ...)
}

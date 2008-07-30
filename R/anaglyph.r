# Math from http://dogfeathers.com/java/3dproj.html
# z0 = distance from eye to screen, mm
# d = half interpupilary distance, mm
anaglyph <- function(d3, length = par("din")[1] * 25.4, z0 = 300, d = 30) {
  length <- length * 0.3
  x <- d3[, 1] * length
  y <- d3[, 2] * length
  # Squash z dimension a bit more
  z <- (1.5 + d3[, 3]) * length / 2
    
  d2 <- data.frame(
    left =  (z0 * x - z * d) / (z0 - z), 
    right = (z0 * x + z * d) / (z0 - z),
    y =     (z0 * y)         / (z0 - z)
  ) / length * 0.5

  with(d2, points(right, y, pch=20, col="green2"))
  with(d2, points(left, y, pch=20, col="red"))
}

stereo_tour <- function(data, tourf = grand_tour, aps = 1, fps = 30, ...) {
  # Standardise data
  data <- apply(data, 2, function(x) (x - min(x)) / diff(range(x)))
  labels <- abbreviate(colnames(data), 2)
  
  # Start with plot of first two variables
  start <- matrix(0, nrow = ncol(data), ncol = 3)
  diag(start) <- 1
  
  # Display 
  range <- c(-1, 1)
  par(pch = "s", bg = "grey80")
  plot(NA, NA,xlim=range, ylim=range, xlab="", ylab="", axes=FALSE, frame=TRUE, xaxs = "i", yaxs = "i")
  step <- function(step, proj) {
    Sys.sleep(1 / fps)
    
    rect(-1, -1, 1, 1, col="#CCCCCCE6", border=NA)
    anaglyph(data %*% proj)
  }
  target <- function(target) {
    # rect(-1, -1, 1, 1, col="#7F7F7F33", border=NA)
  }

  cat("Press Ctrl+C to stop tour runnning\n")
  tourf(start, velocity = aps / fps, step_fun = step, target_fun = target, total_steps = Inf, ..., data=data)
}


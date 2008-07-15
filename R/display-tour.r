#X r_tour(mtcars[, 1:5])
#X r_tour(mtcars[, 1:5], little_tour)
r_tour <- function(data, tour = grand_tour, ...) {
  # Standardise data
  data <- t(apply(data, 1, function(x) (x - min(x)) / diff(range(x))))
  
  # Start with plot of first two variables
  # start <- matrix(0, nrow = ncol(data), ncol = 2)
  # diag(start) <- runif(2)
  
  # Start with a random basis
  start <- basis_random(ncol(data), 2)
  
  # Display 
  plot(NA, NA,xlim=c(-2,2), ylim=c(-2,2), xlab="", ylab="")
  update_plot <- function(step, proj) {
    Sys.sleep(0.05)
    rect(-2, -2, 2, 2, col="#FFFFFFE6", border=NA)
    points(data %*% proj, pch=20)
  }

  cat("Press Ctrl+C to stop tour runnning\n")
  tour(start, step_fun = update_plot, total_steps = Inf)
}



ggobi_tour <- function(data, tour = grand_tour, ...) {
  if(!require("rggobi", quiet = TRUE)) {
    stop("rggobi required for ggobi based tour")
  }

  
  # Start with plot of first two variables
  start <- matrix(0, nrow = ncol(data), ncol = 2)
  diag(start) <- 1
  
  # Display
  g <- ggobi(data)
  gd <- g$data
  update_plot <- function(step, proj) {
    Sys.sleep(0.05)
    rect(-2, -2, 2, 2, col="#FFFFFFE6", border=NA)
    points(data %*% proj, pch=20)
  }

  cat("Press Ctrl+C to stop tour runnning\n")
  tour(start, step_fun = update_plot, total_steps = Inf)
}



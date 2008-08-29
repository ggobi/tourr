#X animate_density(flea[, 1:6])
#X animate_density(flea[, 1:6], method = "density")
#X animate_density(flea[, 1:6], tourf = planned_tour, basis_set = t1, method = "density")

animate_density <- function(data, tourf = grand_tour, method="hist", center = TRUE, ...) {
  labels <- abbreviate(colnames(data), 2)
  
  # Display 
  range <- c(-2, 2)
  render_frame <- function() {
    par(pty="m")
    plot(
      x = NA, y = NA, xlim = range, ylim = c(-1.1,4), xaxs="i", yaxs="i",
      xlab = "Data Projection", ylab = "Density"
    )
  }
  render_transition <- function() {
    rect(-1.99, -1.99, 1.99, 5, col="grey80", border=NA)
  }
  render_data <- function(data, proj, geodesic) {
    abline(h = seq(0.5, 3.5, by=0.5), col="white")
    lines(c(0,0), c(-1,0), col="white")
    lines(c(-1,-1), c(-1,0), col="white")
    lines(c(1,1), c(-1,0), col="white")

    x <- data%*%proj
    if (center) x <- scale(x, center = TRUE, scale = FALSE)
    
    # Render projection data
    if (method=="hist") {
      bins <- hist(x, breaks = seq(-2, 2, 0.2), plot = FALSE)
      with(bins, rect(mids - 0.1, rep(0, length(mids)), mids + 0.1, density, col="black"))
    }
    else if (method=="density") {
      polygon(density(x), lwd = 2, col="black")
      abline(h = 0)
    }
    
    # Render tour axes
    for (i in 1:length(proj)) {
      x <- i / length(proj)
      lines(c(0, proj[i]), c(-x, -x), col="black", lwd=3)
      text(1, -x, labels[i], pos=4)
    }
  }
  render_target <- function(target) {
    rect(-1.99, -1.99, 1.99, 5, col="#7F7F7F33", border=NA)
  }

  animate(
    data = data, tourf = tourf, d = 1, 
    render_frame = render_frame, render_data = render_data,
    render_transition = render_transition, render_target = render_target, 
    ...
  )
}

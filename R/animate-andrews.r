andrews <- function(x) {
  n <- length(x)
  y <- rep(x[1] / sqrt(2), length(t))

  function(t) {
    for(i in seq(2, n, by = 1)) {
      val <- i %/% 2 * t
      y <- y + x[i] * (if(i %% 2 == 0) sin(val) else cos(val))
    }
    y / n
  }
}

#X animate_andrews(flea[, 1:6])

animate_andrews <- function(data, tourf = grand_tour, d = 2, ...) {
  grid <- seq(-pi, pi, length = 50)
  data <- rescale(data)
  
  render_frame <- function() {
    blank_plot(xlim = c(-pi, pi), ylim = c(-1, 1))
  }
  render_transition <- function() {
    rect(-pi, -1, pi, 1, col="#FFFFFF", border=NA)
  }
  render_data <- function(data, proj) {    
    xd <- data %*% proj
    xd <- rescale(xd)
    
    values <- lapply(seq_len(nrow(xd)), function(i) {
      rbind(
        cbind(grid, andrews(xd[i, ])(grid)),
        NA, NA
      )
    })
    segments(-pi, 0, pi, 0)
    lines(do.call("rbind", values))
  }

  animate(
    data = data, tourf = tourf, d = d, 
    render_frame = render_frame, render_data = render_data,
    render_transition = render_transition, render_target = nul, 
    ...
  )
}
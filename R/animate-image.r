# image_tour(pict_samp)
# This is too slow
animate_image<-function(data, tourf = grand_tour, ...) {
  xs <- dim(data)[1]
  ys <- dim(data)[2]
  
  start <- matrix(0, nrow = dim(data)[3], ncol = 1)
  diag(start) <- 1
  
  render_frame <- function() { 
    blank_plot(xlim = c(1, xs), ylim = c(1, xs))
  }
  
  render_data <- function(data, proj) {
    image(
      data[,,1]*proj[1]+data[,,2]*proj[2]+data[,,3]*proj[3], 
      zlim = c(-2, 2), add = TRUE
    )
  }

  animate(
    data = data, tourf = tourf, start = start, 
    render_frame = render_frame, render_data = render_data,
    render_transition = nul, render_target = nul, 
    ...
  )
}
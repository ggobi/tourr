# Image tour uses a 3d array:
#   * first two dimensions are location
#   * third is variables
#
#X animate_image(ozone)
animate_image <- function(data, tourf = grand_tour, ...) {
  
  xs <- dim(data)[1]
  ys <- dim(data)[2]
  zs <- dim(data)[3]

  # Collapse 3d array into 2d matrix with 
  # rows and columns in first dimension
  dim(data) <- c(xs * ys, zs)

  render_frame <- function() { 
    blank_plot(xlim = c(1, xs), ylim = c(1, xs))
  }
  
  render_data <- function(data, proj, geodesic) {
    z <- data %*% proj
    dim(z) <- c(xs, ys)
    image(
      x = seq_len(xs), y = seq_len(ys), 
      z = t(z), 
      zlim = c(-2, 2), add = TRUE
    )
  }

  animate(
    data = data, tourf = tourf, d = 1, 
    render_frame = render_frame, render_data = render_data,
    render_transition = nul, render_target = nul, 
    ...
  )
}
#X animate_scatmat(flea[, 1:6], d = 3)
animate_scatmat <- function(data, tourf = grand_tour, d = 3, ...) {
  render_data <- function(data, proj) {
    pairs(data %*% proj, pch = 20)
  }

  animate(
    d = d, data = data, tourf = tourf, 
    render_frame = nul, render_data = render_data,
    render_transition = nul, render_target = nul, 
    ...
  )
}

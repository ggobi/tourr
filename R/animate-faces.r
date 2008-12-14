# animate_faces(flea[, 1:6], d = 5)

animate_faces <- function(data, tourf = grand_tour, d = 3, ...) {
  require("     ")
  
  render_data <- function(data, proj, geodesic) {
    x <- data %*% proj
    x <- (x + 2) / 4
    faces2(x, scale = "none")
  }

  animate(
    d = d, data = data, tourf = tourf, 
    render_frame = nul, render_data = render_data,
    render_transition = nul, render_target = nul, 
    ...
  )
  
}

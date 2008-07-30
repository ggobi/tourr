# animate_stars(mtcars[1:4, ], d = 5)
# animate_stars(mtcars[1:4, ], d = 5, col.stars = rep("grey50", 4), radius = FALSE)

# Scaling doesn't seem to be quite right as centres move around as
# well as outside points.

animate_stars <- function(data, tourf = grand_tour, d = 3, ...) {
  
  render_data <- function(data, proj) {
    x <- data %*% proj
    x <- (x + 2) / 4
    stars(x, scale = FALSE, ...)
  }

  animate(
    d = d, data = data, tourf = tourf, 
    render_frame = nul, render_data = render_data,
    render_transition = nul, render_target = nul, 
    ...
  )
  
}

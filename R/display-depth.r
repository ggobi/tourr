#' Display 3d projection with depth cues
#' animate_depth(flea[, 1:6])
animate_depth <- function(data, tour_path = grand_tour(3), ...) {  

  animate(
    data = data, tour_path = tour_path,
    display = display_depth(data, ...),
    ...
  )
}

display_depth <- function(data, limit = NULL,  ...)
{
  greys <- rev(grey.colors(100, start = 0))
  rng <- limit <- NULL
  init <- function(data) {
    if (is.null(limit)) {
      first_eigen <- sqrt(eigen(var(data[, 1:2]))$values[1])
      limit <<- 3 * first_eigen
    }
    rng <<- c(-limit, limit)    
  }
  
  render_frame <- function() {
    par(pty = "s", mar = rep(1,4))
    blank_plot(xlim = rng, ylim = rng)
  }
  render_transition <- function() {
    rect(-1.99, -1.99, 1.99, 1.99, col="#FFFFFF", border=NA)
  }
  render_data <- function(data, proj, geodesic) {
    x <- data %*% proj
    x <- scale(x, center = TRUE, scale = FALSE)

    depth <- x[, 3]
    # depth ranges mostly between -1 and 1
    depth_std <- (depth - min(depth)) / diff(range(depth))
    size <- 0.5 + depth_std * 3
    shade <- greys[round(depth_std * 100)]
    print(siz[1])
    
    points(x[order(-depth), 1:2], pch = 20, cex = size, col = shade, ...)
  }
  render_target <- function(target, geodesic) {
    rect(-1.99, -1.99, 1.99, 1.99, col="#7F7F7F33", border=NA)
  }
  
  list(
    init = init,
    render_frame = render_frame,
    render_transition = render_transition,
    render_data = render_data,
    render_target = render_target
  )
}

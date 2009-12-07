save <- function(data, tour_path = grand_tour(), display = display_xy() , apf = 1 / 30, max_frames = 10, ..., rescale = TRUE, sphere = FALSE) {
  if (rescale) data <- rescale(data)
  if (sphere) data  <- sphere(data)
  
  # Display on screen
  display$init(data)
  
  step <- function(step, proj, geodesic) {
    display$render_frame()
    display$render_data(data, proj, geodesic)
  }          
  
  pdf()  
  tour(
    data = data, tour_path = tour_path, 
    velocity = apf, total_steps = max_frames,
    step_fun = step, target_fun = display$render_target, ...
  )
}
# Compute maximum distance from data to origin.
#
# @keywords internal
max_dist <- function(data, center = FALSE) {
  max(sqrt(rowSums(data^2)))
}

compute_half_range <- function(half_range, data, center) {
  if (!is.null(half_range)) {
    return(half_range)
  }

  if (center) {
    data <- center(data)
  }
  half_range <- max_dist(data, center)
  message("Using half_range ", format(half_range, digits = 2))
  half_range
}

# Compute maximum distance from data to origin.
#
# @keywords internal
max_dist <- function(data, center = FALSE) {
  max(sqrt(rowSums(data ^ 2)))
}

# Calculate distribution of ranges of projected data
#
# @keywords internal
# @examples
# data <- rescale(as.matrix(flea[, 1:5]))
# ranges <- range_dist(data, 10000)
# qplot(c(scaled_limits), geom = "freqpoly", binwidth = 0.1) + 
#  expand_limits(x = c(-1,1))
range_dist <- function(data, n = 1000) {
  proj <- save_history(data, grand_tour(), max_bases = n)
  limits <- t(apply(proj, 3, function(proj) range(data %*% proj)))
  limits / xy_limits(data)
}

compute_half_range <- function(half_range, data, center) {
  if (!is.null(half_range)) return(half_range)
  
  if (center) {
    data <- center(data)
  }
  half_range <- max_dist(data, center)
  message("Using half_range ", format(half_range, digits = 2))
  half_range
}
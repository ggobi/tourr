#' Compute maximum distance from data to origin.
#' @keywords internal
#' @examples
max_dist <- function(data, center = FALSE) {
  max(sqrt(rowSums(data ^ 2)))
}

#' Calculate distribution of ranges of projected data
#' @keyword internal
#' data <- rescale(as.matrix(flea[, 1:5]))
#' ranges <- range_dist(data, 10000)
#' qplot(c(scaled_limits), geom = "freqpoly", binwidth = 0.1) + expand_limits(x = c(-1,1))
range_dist <- function(data, n = 1000) {
  proj <- save_history(data, grand_tour(), max_bases = n)
  limits <- t(apply(proj, 3, function(proj) range(data %*% proj)))
  limits / xy_limits(data)
}



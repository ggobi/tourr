context("Interpolation")

test_that("correct number of bases are returned", {
  a <- basis_init(3, 2)
  b <- a[c(3, 1, 2), ]
  
  dist <- sqrt(2)
  expect_that(proj_dist(a, b), equals(dist))
  
  path <- structure(c(a, b), dim = c(3, 2, 2), class = "history_array")
  steps <- function(angle) dim(interpolate(path, angle = angle))[3]
  
  expect_that(steps(0.01), equals(ceiling(dist / 0.01)))
  expect_that(steps(0.05), equals(ceiling(dist / 0.05)))
  expect_that(steps(0.1), equals(ceiling(dist / 0.1)))
  expect_that(steps(1), equals(ceiling(dist)))
  
})
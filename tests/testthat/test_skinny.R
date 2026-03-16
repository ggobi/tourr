test_that("skinny returns a value between 0 and 1", {
  set.seed(1033)
  x <- cbind(rnorm(100), rnorm(100))
  idx <- skinny()
  val <- idx(x)

  expect_true(val >= 0 && val <= 1)
})

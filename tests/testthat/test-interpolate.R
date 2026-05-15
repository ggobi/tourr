test_that("interpolate works for 1D tours (grand_tour(1))", {
  # Regression test for https://github.com/ggobi/tourr/issues/XXX
  # On Windows/Linux, apply() drops singleton dimensions in a p x 1 x n
  # history_array, causing [.history_array to be called with mismatched
  # indices and .subset() to throw "incorrect number of dimensions".

  t1 <- save_history(flea[, 1:6], grand_tour(1), max = 3)

  # Confirm the array has the expected singleton-column shape
  expect_equal(dim(t1), c(6L, 1L, 3L))

  # This is the line that failed on Windows/Linux: interpolate() calls
  # as.list.history_array() and indexes slices of the 3D array.
  # It must not error regardless of platform apply() dimension-drop behaviour.
  result <- interpolate(t1, 0.01)

  # Result should be a 3D history_array with correct row/col dimensions
  expect_equal(dim(result)[1], 6L)   # p (number of variables)
  expect_equal(dim(result)[2], 1L)   # d (tour dimension, must stay 1)
  expect_gt(dim(result)[3], 3L)      # more frames after interpolation

  # Each interpolated basis should be a valid unit-norm column
  for (k in seq_len(dim(result)[3])) {
    basis <- matrix(unclass(result)[, , k], nrow = 6)
    expect_equal(
      crossprod(basis)[1, 1], 1,
      tolerance = 1e-6,
      label = sprintf("basis at frame %d has unit norm", k)
    )
  }
})

test_that("as.list.history_array preserves matrix shape for 1D and 2D tours", {
  # as.list() is called internally by interpolate() via planned_tour().
  # For a p x 1 x n array, apply(x, 3, list) can silently drop the
  # singleton second dimension on some platforms, returning vectors
  # instead of matrices. Every element of the list must be a p x d matrix.

  for (d in c(1L, 2L)) {
    t1 <- save_history(flea[, 1:6], grand_tour(d), max = 4)
    lst <- as.list(t1)

    expect_equal(
      length(lst), dim(t1)[3],
      label = sprintf("list length matches n_frames for d=%d", d)
    )

    for (i in seq_along(lst)) {
      m <- lst[[i]]
      expect_true(
        is.matrix(m),
        label = sprintf("element %d is a matrix for d=%d", i, d)
      )
      expect_equal(
        dim(m), c(6L, d),
        label = sprintf("element %d has dim c(6,%d)", i, d)
      )
    }
  }
})

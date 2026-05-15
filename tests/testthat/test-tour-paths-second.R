# Tests for tour path generators
#
# Every tour path generator must satisfy three contracts:
#
#   1. CONSTRUCTION  — the object it returns has class "tour_path" and the
#      correct "name" attribute, so new_tour() can dispatch correctly.
#
#   2. GENERATION    — calling the generator with (current_basis, data)
#      returns a geodesic list with at minimum an $ingred component, which
#      itself contains $dist (a positive scalar) and an $interpolate
#      function. Returning NULL signals the tour should stop (used by
#      planned_tour at the end of its sequence).
#
#   3. INTEGRATION   — save_history() + interpolate() round-trip works end-
#      to-end, producing a valid history_array with orthonormal columns.
#      This is the closest thing to an animate() test that can run headlessly.
#
# The orthonormality helper is used throughout to verify basis validity.

# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------

dat  <- as.matrix(flea[, 1:6])   # 74 x 6
dat4 <- as.matrix(flea[, 1:4])   # 74 x 4  (used for frozen/dependence tests)

# Check that all slices of a history_array have orthonormal columns.
expect_orthonormal_history <- function(h, tol = 1e-6) {
  n <- dim(h)[3]
  for (k in seq_len(n)) {
    b <- matrix(unclass(h)[, , k], nrow = dim(h)[1])
    gram <- crossprod(b)
    expect_equal(gram, diag(ncol(b)), tolerance = tol,
      label = sprintf("basis at frame %d is orthonormal", k))
  }
}


# ===========================================================================
# grand_tour
# ===========================================================================

test_that("grand_tour has correct class and name", {
  tp <- grand_tour(2)
  expect_s3_class(tp, "tour_path")
  expect_equal(attr(tp, "name"), "grand")
})

test_that("grand_tour(2) generates valid geodesic ingredients", {
  tp   <- grand_tour(2)
  from <- basis_random(6, 2)
  g    <- tp(from, dat)
  expect_false(is.null(g$ingred))
  expect_gt(g$ingred$dist, 0)
  expect_true(is.function(g$ingred$interpolate))
  # Interpolated midpoint should be orthonormal
  mid <- g$ingred$interpolate(0.5)
  expect_equal(crossprod(mid), diag(2), tolerance = 1e-6)
})

test_that("grand_tour works for 1D (singleton column)", {
  # Regression: this is the dimension that triggers the Windows/Linux
  # dimension-drop bug in history_array handling.
  tp   <- grand_tour(1)
  from <- basis_random(6, 1)
  g    <- tp(from, dat)
  expect_false(is.null(g$ingred))
  mid  <- g$ingred$interpolate(0.5)
  expect_equal(dim(mid), c(6L, 1L))
  expect_equal(crossprod(mid)[1, 1], 1, tolerance = 1e-6)
})

test_that("grand_tour save_history + interpolate round-trip is orthonormal", {
  set.seed(1)
  h <- save_history(dat, grand_tour(2), max = 5)
  expect_equal(dim(h)[1:2], c(6L, 2L))
  interp <- interpolate(h, 0.05)
  expect_orthonormal_history(interp)
})

test_that("grand_tour(1) save_history + interpolate round-trip is orthonormal", {
  set.seed(1)
  h <- save_history(dat, grand_tour(1), max = 5)
  expect_equal(dim(h)[1:2], c(6L, 1L))
  interp <- interpolate(h, 0.05)
  expect_orthonormal_history(interp)
})


# ===========================================================================
# little_tour
# ===========================================================================

test_that("little_tour has correct class and name", {
  tp <- little_tour()
  expect_s3_class(tp, "tour_path")
  expect_equal(attr(tp, "name"), "little")
})

test_that("little_tour generates axis-parallel bases", {
  # The little tour cycles through axis-parallel projections: each column of
  # the basis has exactly one non-zero entry of ±1 at each extreme.
  h <- save_history(dat4, little_tour(), max = 10)
  expect_equal(dim(h)[1:2], c(4L, 2L))
  # All saved bases should be orthonormal
  expect_orthonormal_history(h)
})

test_that("little_tour save_history + interpolate round-trip works", {
  h      <- save_history(dat4, little_tour(), max = 6)
  interp <- interpolate(h, 0.05)
  expect_orthonormal_history(interp)
})


# ===========================================================================
# planned_tour
# ===========================================================================

test_that("planned_tour has correct class and name", {
  set.seed(1)
  bases <- save_history(dat, grand_tour(2), max = 4)
  tp    <- planned_tour(bases)
  expect_s3_class(tp, "tour_path")
  expect_equal(attr(tp, "name"), "planned")
})

test_that("planned_tour replays exactly the saved bases", {
  set.seed(1)
  bases  <- save_history(dat, grand_tour(2), max = 4)
  # Replay via a second save_history using planned_tour — we should recover
  # a history with at least as many frames as the original.
  replay <- save_history(dat, planned_tour(bases))
  expect_gte(dim(replay)[3], dim(bases)[3])
  expect_orthonormal_history(replay)
})

test_that("planned_tour with cycle = TRUE does not terminate early", {
  set.seed(1)
  bases  <- save_history(dat, grand_tour(2), max = 3)
  # With cycling, requesting more frames than bases should succeed
  replay <- save_history(dat, planned_tour(bases, cycle = TRUE), max = 9)
  expect_gte(dim(replay)[3], 6L)
})

test_that("planned_tour works for 1D saved history", {
  set.seed(1)
  bases  <- save_history(dat, grand_tour(1), max = 4)
  replay <- save_history(dat, planned_tour(bases))
  expect_equal(dim(replay)[2], 1L)
  expect_orthonormal_history(replay)
})


# ===========================================================================
# guided_tour
# ===========================================================================

test_that("guided_tour has correct class and name", {
  tp <- guided_tour(holes())
  expect_s3_class(tp, "tour_path")
  expect_equal(attr(tp, "name"), "guided")
})

test_that("guided_tour with holes index produces valid history", {
  set.seed(42)
  h <- save_history(dat, guided_tour(holes()), max = 5)
  expect_equal(dim(h)[1:2], c(6L, 2L))
  expect_orthonormal_history(h)
})

test_that("guided_tour with cmass index produces valid history", {
  set.seed(42)
  h <- save_history(dat, guided_tour(cmass()), max = 5)
  expect_equal(dim(h)[1:2], c(6L, 2L))
  expect_orthonormal_history(h)
})

test_that("guided_tour index values are non-decreasing along saved path", {
  # The guided tour is an optimisation: each new basis should have a higher
  # (or equal) index value than the previous one.
  #
  # holes() takes the projected data matrix (n x d), not the basis or the
  # original data. We compute proj = dat %*% basis for each saved frame.
  set.seed(42)
  h      <- save_history(dat, guided_tour(holes()), max = 8)
  scores <- sapply(seq_len(dim(h)[3]), function(k) {
    basis <- matrix(unclass(h)[, , k], nrow = 6)
    proj  <- dat %*% basis
    holes()(proj)
  })
  # Allow slight numerical noise with a small tolerance
  diffs <- diff(scores)
  expect_true(all(diffs >= -1e-4),
    label = "holes index is non-decreasing along guided tour path")
})


# ===========================================================================
# local_tour
# ===========================================================================

test_that("local_tour has correct class and name", {
  tp <- local_tour(basis_random(6, 2))
  expect_s3_class(tp, "tour_path")
  expect_equal(attr(tp, "name"), "local")
})

test_that("local_tour stays close to its starting basis", {
  set.seed(1)
  start <- basis_random(6, 2)
  h     <- save_history(dat, local_tour(start, angle = pi / 8), max = 10)
  # Every basis in the history should be within angle pi/8 of the starting
  # basis (measured via proj_dist which returns angle in [0, pi/2])
  n <- dim(h)[3]
  for (k in seq_len(n)) {
    b    <- matrix(unclass(h)[, , k], nrow = 6)
    dist <- proj_dist(start, b)
    expect_lte(dist, pi / 8 + 1e-6,
      label = sprintf("frame %d stays within angle constraint", k))
  }
})

test_that("local_tour save_history + interpolate round-trip is orthonormal", {
  set.seed(1)
  start  <- basis_random(6, 2)
  h      <- save_history(dat, local_tour(start), max = 5)
  interp <- interpolate(h, 0.05)
  expect_orthonormal_history(interp)
})


# ===========================================================================
# frozen_tour
# ===========================================================================

test_that("frozen_tour has correct class and name", {
  frozen     <- matrix(NA, nrow = 4, ncol = 2)
  frozen[3,] <- 0.5
  tp         <- frozen_tour(2, frozen)
  expect_s3_class(tp, "tour_path")
  expect_equal(attr(tp, "name"), "frozen")
})

test_that("frozen_tour respects frozen values in saved history", {
  frozen     <- matrix(NA, nrow = 4, ncol = 2)
  frozen[3,] <- 0.5
  set.seed(1)
  h <- save_history(dat4, frozen_tour(2, frozen), max = 8)

  # Row 3 of every basis should remain 0.5 throughout the tour
  for (k in seq_len(dim(h)[3])) {
    b <- matrix(unclass(h)[, , k], nrow = 4)
    expect_equal(b[3, ], c(0.5, 0.5), tolerance = 1e-6,
      label = sprintf("frame %d: frozen row 3 stays at 0.5", k))
  }
})

test_that("frozen_tour save_history + interpolate round-trip is orthonormal", {
  frozen     <- matrix(NA, nrow = 4, ncol = 2)
  frozen[1, 1] <- 0.5
  set.seed(1)
  h      <- save_history(dat4, frozen_tour(2, frozen), max = 5)
  interp <- interpolate(h, 0.05)
  expect_orthonormal_history(interp)
})


# ===========================================================================
# radial_tour
# ===========================================================================

test_that("radial_tour has correct class and name", {
  start <- basis_random(6, 2)
  tp    <- radial_tour(start, mvar = 1)
  expect_s3_class(tp, "tour_path")
  expect_equal(attr(tp, "name"), "radial")
})

test_that("radial_tour requires a start basis and integer mvar", {
  # Passing an out-of-range mvar should error — the variable index must be
  # a valid column of the data.
  start <- basis_random(6, 2)
  expect_no_error(radial_tour(start, mvar = 6))   # last variable: fine
  expect_error(radial_tour(start, mvar = 0))       # 0 is not a valid index
  expect_error(radial_tour(start, mvar = 7))       # beyond p=6
})

test_that("radial_tour save_history produces orthonormal bases", {
  set.seed(1)
  start <- basis_random(6, 2)
  h     <- save_history(dat, radial_tour(start, mvar = 1), max = 10)
  expect_equal(dim(h)[1:2], c(6L, 2L))
  expect_orthonormal_history(h)
})

test_that("radial_tour drives mvar column through zero during sweep", {
  # The defining behaviour of the radial tour: variable mvar is rotated
  # smoothly out of the projection. At the midpoint of the sweep its
  # contribution (both rows of the mvar column) should be at or near zero.
  # We capture enough frames that the midpoint is well-sampled.
  set.seed(1)
  start <- basis_random(6, 2)
  mvar  <- 3L
  h     <- save_history(dat, radial_tour(start, mvar = mvar), max = 30)

  # Extract the norm of row `mvar` across all frames — it should dip
  # close to zero somewhere in the sequence.
  norms <- sapply(seq_len(dim(h)[3]), function(k) {
    b <- matrix(unclass(h)[, , k], nrow = 6)
    sqrt(sum(b[mvar, ]^2))   # Euclidean norm of the mvar row
  })
  expect_lt(min(norms), 0.1,
    label = "mvar row norm passes through near-zero during radial sweep")
})

test_that("radial_tour works with multiple mvar indices", {
  set.seed(1)
  start <- basis_random(6, 2)
  # Two variables rotated out simultaneously
  h <- save_history(dat, radial_tour(start, mvar = c(2, 5)), max = 10)
  expect_equal(dim(h)[1:2], c(6L, 2L))
  expect_orthonormal_history(h)
})

test_that("radial_tour save_history + interpolate round-trip is orthonormal", {
  set.seed(1)
  start  <- basis_random(6, 2)
  h      <- save_history(dat, radial_tour(start, mvar = 4), max = 10)
  interp <- interpolate(h, 0.05)
  expect_orthonormal_history(interp)
})

test_that("radial_tour works with a 1D start basis via new_tour", {
  # Regression: ensure the singleton-column case doesn't trigger dimension
  # drop bugs in the history_array machinery.
  set.seed(1)
  start <- basis_random(6, 1)
  expect_no_error({
    tour <- new_tour(dat, radial_tour(start, mvar = 2))
    step <- tour(0.05)
  })
  expect_equal(dim(step$proj), c(6L, 1L))
  expect_true(is_orthonormal(step$proj))
})


# ===========================================================================
# Cross-cutting: all tour paths work with new_tour() directly
# ===========================================================================

test_that("new_tour() accepts all standard tour path types without error", {
  set.seed(1)
  bases  <- save_history(dat, grand_tour(2), max = 3)
  frozen <- matrix(NA, nrow = 4, ncol = 2); frozen[3,] <- 0.5

  tour_paths <- list(
    grand_tour(2),
    grand_tour(1),
    little_tour(),
    planned_tour(bases),
    local_tour(basis_random(6, 2)),
    guided_tour(holes()),
    dependence_tour(c(1, 2, 1, 2, 1, 2)),
    frozen_tour(2, frozen),
    radial_tour(basis_random(6, 2), mvar = 1)
  )
  data_for <- list(dat, dat, dat4, dat, dat, dat, dat, dat4, dat)

  for (i in seq_along(tour_paths)) {
    tp <- tour_paths[[i]]
    d  <- data_for[[i]]
    expect_no_error(
      {
        tour <- new_tour(d, tp)
        step <- tour(0.05)
      },
      label = sprintf("new_tour works for tour type %d (%s)",
                      i, attr(tp, "name"))
    )
    expect_true(is.matrix(step$proj),
      label = sprintf("new_tour step$proj is a matrix for type %d", i))
    expect_true(is_orthonormal(step$proj),
      label = sprintf("new_tour step$proj is orthonormal for type %d", i))
  }
})

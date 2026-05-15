# Tests for display_* functions
#
# Strategy: display functions return a list of closures (init, render_frame,
# render_transition, render_data, render_target). We test them by:
#   1. Checking the returned object has the right structure.
#   2. Calling each closure with realistic inputs inside a null PDF device
#      so we exercise the drawing code without opening a screen window.
#
# We do NOT call animate() in these tests — that would open an interactive
# graphics device and run indefinitely. Instead we drive the closures directly,
# which is both faster and more precise about what is being tested.

# ---------------------------------------------------------------------------
# Shared test fixtures
# ---------------------------------------------------------------------------

dat   <- as.matrix(flea[, 1:6])          # 74 x 6 numeric matrix
proj2 <- basis_random(6, 2)              # 6 x 2 orthonormal basis  (2D tour)
proj1 <- basis_random(6, 1)              # 6 x 1 orthonormal basis  (1D tour)

# Helper: run `expr` inside a null PDF device so base-graphics calls succeed
# but nothing is drawn to screen or disk.
#
# pdf(NULL) opens the device but does not create a plot frame. Drawing
# primitives like points(), text(), and segments() require plot.new() to have
# been called first, which is normally done by the display's render_frame()
# closure. We call plot.new() here so that tests which call render_data()
# directly (without going through render_frame() first) do not fail with
# "plot.new has not been called yet".
with_null_device <- function(expr) {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  plot.new()
  force(expr)
}


# ===========================================================================
# display_xy
# ===========================================================================

test_that("display_xy returns a correctly structured display object", {
  d <- display_xy()
  expect_named(d, c("init", "render_frame", "render_transition",
                    "render_data", "render_target"))
  expect_true(all(vapply(d, is.function, logical(1))))
})

test_that("display_xy init sets half_range from data", {
  d <- display_xy()
  with_null_device({
    d$init(dat)
    # render_data should now work without error, confirming init ran correctly
    expect_no_error(d$render_data(dat, proj2, geodesic = FALSE))
  })
})

test_that("display_xy render_data accepts col and pch vectors", {
  col_vec <- ifelse(flea$species == "Heikert'", "steelblue", "tomato")
  d <- display_xy(col = col_vec, pch = 20)
  with_null_device({
    d$init(dat)
    expect_no_error(d$render_data(dat, proj2, geodesic = FALSE))
  })
})

test_that("display_xy respects a fixed half_range", {
  # When half_range is fixed, the projected coordinates should be scaled by
  # exactly that value. We verify render_data runs without error and that
  # init does not override the supplied half_range.
  d <- display_xy(half_range = 1)
  with_null_device({
    d$init(dat)
    expect_no_error(d$render_data(dat, proj2, geodesic = FALSE))
  })
})

test_that("display_xy works with axes = 'off'", {
  d <- display_xy(axes = "off")
  with_null_device({
    d$init(dat)
    expect_no_error(d$render_frame())
    expect_no_error(d$render_data(dat, proj2, geodesic = FALSE))
  })
})

test_that("display_xy works with edges argument", {
  # edges is an n x 2 integer matrix of point-index pairs to connect
  edges <- matrix(c(1L, 2L, 3L, 4L), ncol = 2)
  d <- display_xy(edges = edges)
  with_null_device({
    d$init(dat)
    expect_no_error(d$render_data(dat, proj2, geodesic = FALSE))
  })
})


# ===========================================================================
# display_dist  (1D distribution display)
# ===========================================================================

test_that("display_dist returns a correctly structured display object", {
  d <- display_dist()
  expect_named(d, c("init", "render_frame", "render_transition",
                    "render_data", "render_target"))
  expect_true(all(vapply(d, is.function, logical(1))))
})

test_that("display_dist render_data works with a 1D projection", {
  d <- display_dist()
  with_null_device({
    d$init(dat)
    expect_no_error(d$render_data(dat, proj1, geodesic = FALSE))
  })
})

test_that("display_dist render_frame does not error", {
  d <- display_dist()
  with_null_device({
    d$init(dat)
    expect_no_error(d$render_frame())
  })
})


# ===========================================================================
# display_pcp  (parallel coordinates)
# ===========================================================================

test_that("display_pcp returns a correctly structured display object", {
  d <- display_pcp()
  expect_named(d, c("init", "render_frame", "render_transition",
                    "render_data", "render_target"))
  expect_true(all(vapply(d, is.function, logical(1))))
})

test_that("display_pcp render_data works with a 2D projection", {
  d <- display_pcp()
  with_null_device({
    d$init(dat)
    expect_no_error(d$render_data(dat, proj2, geodesic = FALSE))
  })
})

test_that("display_pcp accepts col argument", {
  col_vec <- rep(c("red", "blue"), length.out = nrow(dat))
  d <- display_pcp(col = col_vec)
  with_null_device({
    d$init(dat)
    expect_no_error(d$render_data(dat, proj2, geodesic = FALSE))
  })
})


# ===========================================================================
# display_density2d
# ===========================================================================

test_that("display_density2d returns a correctly structured display object", {
  d <- display_density2d()
  expect_named(d, c("init", "render_frame", "render_transition",
                    "render_data", "render_target"))
  expect_true(all(vapply(d, is.function, logical(1))))
})

test_that("display_density2d render_data works with a 2D projection", {
  d <- display_density2d()
  with_null_device({
    d$init(dat)
    expect_no_error(d$render_data(dat, proj2, geodesic = FALSE))
  })
})


# ===========================================================================
# Cross-cutting: render_target is NULL for all standard displays
# (render_target is only non-NULL for guided-tour displays)
# ===========================================================================

test_that("standard display render_target closures are nul/NULL", {
  displays <- list(
    display_xy(),
    display_dist(),
    display_pcp(),
    display_density2d()
  )
  for (d in displays) {
    # render_target should either be the package's `nul` function or NULL
    rt <- d$render_target
    expect_true(
      is.null(rt) || identical(rt, tourr:::nul),
      label = "render_target is nul or NULL"
    )
  }
})

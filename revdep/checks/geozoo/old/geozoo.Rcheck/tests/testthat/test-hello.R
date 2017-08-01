context("run everything with defaults")

test_that("base functions", {

  w <- (function(){
    fp <- switch(.Platform$OS.type,
      windows = "nul",
      file.path("", "dev", "null")
    )
    function(x) {
      write.xml(x, fp, "TITLE!")
    }
  })()

  w(cross.polytope())

  w(cube.iterate())
  w(cube.face())
  w(cube.solid.random())
  w(cube.solid.grid())
  w(cube.dotline())
  w(sphere.hollow())
  w(sphere.solid.random())
  w(sphere.solid.grid())
  w(simplex())
  w(torus())
  w(torus.flat())

  w(boy.surface())
  w(conic.spiral())
  w(conic.spiral.nautilus())
  w(cross.cap())
  w(dini.surface())
  w(ellipsoid())
  w(enneper.surface())
  w(klein.fig.eight())
  w(roman.surface())

  w(mobius())
  w(mobius.experiment())

  expect_true(TRUE)
})

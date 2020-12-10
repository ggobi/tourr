### Name: guided_section_tour
### Title: A guided section tour path.
### Aliases: guided_section_tour

### ** Examples

# Generate samples on a 3d hollow sphere using the geozoo package
set.seed(12345)
sphere3 <- geozoo::sphere.hollow(3)$points
# Columns need to be named before launching the tour
colnames(sphere3) <- c("x1", "x2", "x3")
# Off-center anchoring
anchor3 <- rep(0.75, 3)
# Index setup
r_breaks <- linear_breaks(5, 0, 1)
a_breaks <- angular_breaks(10)
eps <- estimate_eps(nrow(sphere3), ncol(sphere3), 0.1 / 1, 5 * 10, 10, r_breaks)
idx <- slice_index(r_breaks, a_breaks, eps, bintype = "polar", power = 1, reweight = TRUE, p = 3)
# Running the guided section tour select sections showing a big hole in the center
animate_slice(sphere3, guided_section_tour(idx, v_rel = 0.1, anchor = anchor3, max.tries = 5),
  v_rel = 0.1, anchor = anchor3
)




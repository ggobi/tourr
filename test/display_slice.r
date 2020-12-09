### Name: display_slice
### Title: Display tour path with a sliced scatterplot
### Aliases: display_slice animate_slice

### ** Examples

# Generate samples on a 3d and 5d hollow sphere using the geozoo package
sphere3 <- geozoo::sphere.hollow(3)$points
sphere5 <- geozoo::sphere.hollow(5)$points

# Columns need to be named before launching the tour
colnames(sphere3) <- c("x1", "x2", "x3")
colnames(sphere5) <- c("x1", "x2", "x3", "x4", "x5")

# Animate with the slice display using the default parameters
animate_slice(sphere3)
animate_slice(sphere5)

# Animate with off-center anchoring
anchor3 <- rep(0.7, 3)
anchor5 <- rep(0.3, 5)
animate_slice(sphere3, anchor = anchor3)
# Animate with thicker slice to capture more points in each view
animate_slice(sphere5, anchor = anchor5, v_rel = 0.02)




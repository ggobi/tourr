### Name: display_sage
### Title: Display tour path with a sage scatterplot
### Aliases: display_sage animate_sage

### ** Examples

# Generate uniform samples in a 10d sphere using the geozoo package
sphere10 <- geozoo::sphere.solid.random(10)$points
# Columns need to be named before launching the tour
colnames(sphere10) <- paste0("x", 1:10)
# Standard grand tour display, points cluster near center
animate_xy(sphere10)
# Sage display, points are uniformly distributed across the disk
animate_sage(sphere10)




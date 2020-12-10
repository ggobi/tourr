### Name: save_history
### Title: Save tour history.
### Aliases: save_history

### ** Examples

# You can use a saved history to replay tours with different visualisations

t1 <- save_history(flea[, 1:6], max = 3)
animate_xy(flea[, 1:6], planned_tour(t1))
## andrews_history(t1)
## andrews_history(interpolate(t1))

t1 <- save_history(flea[, 1:6], grand_tour(4), max = 3)
animate_pcp(flea[, 1:6], planned_tour(t1))
animate_scatmat(flea[, 1:6], planned_tour(t1))

t1 <- save_history(flea[, 1:6], grand_tour(1), max = 3)
animate_dist(flea[, 1:6], planned_tour(t1))

testdata <- matrix(rnorm(100 * 3), ncol = 3)
testdata[1:50, 1] <- testdata[1:50, 1] + 10
testdata <- sphere_data(testdata)
t2 <- save_history(testdata, guided_tour(holes(), max.tries = 100),
  max = 5, rescale = FALSE
)
animate_xy(testdata, planned_tour(t2))

# Or you can use saved histories to visualise the path that the tour took.
plot(path_index(interpolate(t2), holes()))
plot(path_curves(interpolate(t2)))




### Name: display_dist
### Title: 1d distribution tour path animation.
### Aliases: display_dist animate_dist
### Keywords: hplot

### ** Examples

animate_dist(flea[, 1:6])

# When the distribution is not centred, it tends to wander around in a
# distracting manner
animate_dist(flea[, 1:6], center = FALSE)

# Alternatively, you can display the distribution with a histogram
animate_dist(flea[, 1:6], method = "hist")




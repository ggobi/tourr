### Name: grand_tour
### Title: A grand tour path.
### Aliases: grand_tour

### ** Examples

# All animation methods use the grand tour path by default
animate_dist(flea[, 1:6])
animate_xy(flea[, 1:6])
animate_pcp(flea[, 1:6])
animate_pcp(flea[, 1:6], grand_tour(4))

# The grand tour is a function:
tour2d <- grand_tour(2)
is.function(tour2d)

# with two parameters, the previous projection and the data set
args(tour2d)
# if the previous projection is null, it will generate a starting
# basis, otherwise the argument is ignored
tour2d(NULL, mtcars)
# the data argument is just used to determine the correct dimensionality
# of the output matrix
tour2d(NULL, mtcars[, 1:2])




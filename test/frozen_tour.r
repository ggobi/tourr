### Name: frozen_tour
### Title: A frozen tour path.
### Aliases: frozen_tour

### ** Examples

frozen <- matrix(NA, nrow = 4, ncol = 2)
frozen[3, ] <- .5
animate_xy(flea[, 1:4], frozen_tour(2, frozen))
## Not run: 
##D # Doesn't work - a bug?
##D frozen <- matrix(NA, nrow = 4, ncol = 2)
##D frozen[1, 1] <- 0.5
##D animate_xy(flea[, 1:4], frozen_tour(2, frozen))
##D 
##D # Doesn't work - a bug?
##D frozen <- matrix(NA, nrow = 4, ncol = 2)
##D frozen[, 1] <- 1 / 2
##D animate_xy(flea[, 1:4], frozen_tour(2, frozen))
##D 
##D # Doesn't work - a bug?
##D frozen[3, ] <- c(0, 1)
##D animate_xy(flea[, 1:4], frozen_tour(2, frozen))
##D 
##D # Doesn't move, which is correct - no free variables
##D frozen[4, ] <- .2
##D animate_xy(flea[, 1:4], frozen_tour(2, frozen))
##D 
##D # Doesn't work - a bug?
##D frozen <- matrix(NA, nrow = 4, ncol = 2)
##D frozen[, 1] <- 1 / 2
##D animate_xy(flea[, 1:4], frozen_tour(2, frozen))
## End(Not run)
# Two frozen variables in five 5.
frozen <- matrix(NA, nrow = 5, ncol = 2)
frozen[3, ] <- .5
frozen[4, ] <- c(-.2, .2)
animate_xy(flea[, 1:5], frozen_tour(2, frozen))




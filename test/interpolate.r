### Name: interpolate
### Title: Interpolate geodesically between bases.
### Aliases: interpolate
### Keywords: hplot

### ** Examples

t1 <- save_history(flea[, 1:6], grand_tour(1), max = 10)
dim(t1)
dim(interpolate(t1, 0.01))
dim(interpolate(t1, 0.05))
dim(interpolate(t1, 0.1))




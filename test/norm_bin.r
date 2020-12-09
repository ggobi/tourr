### Name: norm_bin
### Title: Normality index.
### Aliases: norm_bin norm_kol
### Keywords: hplot

### ** Examples

# manually compute the norm_kol index
# create the index function
set.seed(123)
index <- norm_kol(nrow(flea[, 1:3]))
# create the projection
proj <- matrix(c(1, 0, 0), nrow = 3)
# pre-process the example data
flea_s <- sphere_data(flea[, 1:3])
# produce the index value
index(flea_s %*% proj)




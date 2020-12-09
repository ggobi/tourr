### Name: paths_index
### Title: Compute index value for many histories.
### Aliases: paths_index
### Keywords: internal

### ** Examples

holes1d <- guided_tour(holes(), 1)
# Perform guided tour 5 times, saving results
tries <- replicate(5, save_history(flea[, 1:6], holes1d), simplify = FALSE)
# Interpolate between target bases
itries <- lapply(tries, interpolate)

paths <- paths_index(itries, holes())
head(paths)

if (require(ggplot2)) {
  qplot(step, value, data = paths, group = try, geom = "line")
  qplot(step, improvement, data = paths, group = try, geom = "line")
}




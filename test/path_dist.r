### Name: path_dist
### Title: Compute distance matrix from bases.
### Aliases: path_dist

### ** Examples

## Not run: 
##D grand <- interpolate(save_history(flea[, 1:6], max = 50), 0.2)
##D # The grand tour  -----------------------------
##D # Look at the tour path in a tour, how well does it cover a sphere
##D # Using MDS
##D d <- path_dist(grand)
##D ord <- as.data.frame(MASS::isoMDS(d)$points)
##D qplot(V1, V2, data = ord, geom = "path") +
##D   coord_equal() + labs(x = NULL, y = NULL)
## End(Not run)

# 5 guided tours  -----------------------------
holes1d <- guided_tour(holes(), 1)
tour_reps <- replicate(5, save_history(flea[, 1:6], holes1d, max = 10),
  simplify = FALSE
)
tour_reps2 <- lapply(tour_reps, interpolate, 0.2)

bases <- unlist(lapply(tour_reps2, as.list), recursive = FALSE)
class(bases) <- "history_list"
index_values <- paths_index(tour_reps2, holes())
d <- path_dist(bases)
ord <- as.data.frame(cmdscale(d, 2))

info <- cbind(ord, index_values)
if (require("ggplot2")) {
  ggplot(data = info, aes(x = step, y = value, group = try)) +
    geom_line()
  ggplot(data = info, aes(x = V1, y = V2, group = try)) +
    geom_path() +
    geom_point(aes(size = value)) +
    coord_equal()
  last_plot() + facet_wrap(~try)
}




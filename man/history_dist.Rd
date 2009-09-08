\name{history_dist}
\alias{history_dist}
\title{Compute distance matrix from bases.}
\usage{history_dist(history)}
\description{Compute distance matrix from bases.}
\examples{grand <- interpolate(save_history(flea[, 1:6], max = 50), 0.2)
# The grand tour  -----------------------------
d <- history_dist(grand)
ord <- as.data.frame(MASS::isoMDS(d)$points)
qplot(V1, V2, data = ord, geom="path") + 
coord_equal() + labs(x = NULL, y = NULL)

# 25 guided tours  -----------------------------
holes1d <- guided_tour(holes, 1)
tries <- replicate(25, save_history(flea[, 1:6], holes1d), simplify = FALSE)
tries2 <- lapply(tries, interpolate, 0.2)

bases <- unlist(lapply(tries2, as.list), recursive = F)
class(bases) <- "history_list"
index_values <- histories_index(tries2, holes)
d <- history_dist(bases)
ord <- as.data.frame(cmdscale(d, 2))

info <- cbind(ord, index_values)
qplot(step, value, data = info, geom="line", group = try)
qplot(V1, V2, data = info, geom="path", group = try) + 
geom_point(aes(size = value)) + 
coord_equal()
last_plot() + facet_wrap(~ try)}

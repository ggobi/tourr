history_index <- function(bases, index_f, data = attr(bases, "data")) {
  index <- function(proj) {
    index_f(as.matrix(data) %*% proj)
  }
  
  apply(bases, 3, index)
}

histories_index <- function(bases_list, index_f) {
  indices <- lapply(bases_list, history_index, index_f)
  data.frame(
    try = rep(seq_along(indices), sapply(indices, length)),
    step = unlist(sapply(indices, seq_along)), 
    value = unlist(indices)
  )  
}

# # Perform guided tour 10 times, saving results
# tries <- replicate(100, save_history(flea[, 1:6], d=1, guided_tour, index_f = holes, basis_f = basis_geodesic_search, sphere = T), simplify = F)


# tries <- replicate(10, save_history(flea[, 1:6], guided_tour, index_f = holes, basis_f = basis_geodesic_search, sphere = T), simplify = F)

# old <- replicate(10, save_history(flea[, 1:6], guided_tour, index_f = holes, basis_f = basis_better, sphere = T), simplify = F)


# 
# # Interpolate between target bases 
# itries <- lapply(tries, interpolate)
# iold <- lapply(old, interpolate)
# 
# histories_index(tries, holes)
# 
# library(ggplot2)
# qplot(step, value, data=histories_index(itries, holes), group=try, geom="line")
# qplot(step, value, data=histories_index(iold, holes), group=try, geom="line")
# 
# # Experiment with MDS ------
# 
# tries2 <- lapply(tries, interpolate, 0.2)
# bases <- unlist(tries2)
# dim(bases) <- c(6, 2, length(bases) / 12)
# 
# n <- dim(bases)[3]
# d <- matrix(NA, nrow = n, ncol = n)
# 
# for(i in seq_len(n)) {
#   for (j in seq_len(i - 1)) {
#     d[i, j] <- proj_dist(bases[, , i], bases[, , j])
#   }
# }
# d <- as.dist(d)
# 
# ord <- isoMDS(d)$points
# mds <- data.frame(ord, histories_index(tries2, holes))
# qplot(X1, X2, data=mds, geom="path", group=try) + geom_point(aes(size = value, colour=value)) + coord_equal()

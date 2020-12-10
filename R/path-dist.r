#' Compute distance matrix from bases.
#'
#' @param history history of the plots
#' @export
#' @examples
#' \dontrun{
#' grand <- interpolate(save_history(flea[, 1:6], max = 50), 0.2)
#' # The grand tour  -----------------------------
#' # Look at the tour path in a tour, how well does it cover a sphere
#' # Using MDS
#' d <- path_dist(grand)
#' ord <- as.data.frame(MASS::isoMDS(d)$points)
#' ggplot(data = ord, aes(x=V1, y=V2)) +
#'   geom_path() +
#'   coord_equal() +
#'   labs(x = NULL, y = NULL)
#' }
#'
#' # 5 guided tours  -----------------------------
#' holes1d <- guided_tour(holes(), 1)
#' tour_reps <- replicate(5, save_history(flea[, 1:6], holes1d, max = 10),
#'   simplify = FALSE
#' )
#' tour_reps2 <- lapply(tour_reps, interpolate, 0.2)
#'
#' bases <- unlist(lapply(tour_reps2, as.list), recursive = FALSE)
#' class(bases) <- "history_list"
#' index_values <- paths_index(tour_reps2, holes())
#' d <- path_dist(bases)
#' ord <- as.data.frame(cmdscale(d, 2))
#'
#' info <- cbind(ord, index_values)
#' if (require("ggplot2")) {
#'   ggplot(data = info, aes(x = step, y = value, group = try)) +
#'     geom_line()
#'   ggplot(data = info, aes(x = V1, y = V2, group = try)) +
#'     geom_path() +
#'     geom_point(aes(size = value)) +
#'     coord_equal()
#'   last_plot() + facet_wrap(~try)
#' }
path_dist <- function(history) {
  history <- as.array(history)
  n <- dim(history)[3]
  d <- matrix(NA, nrow = n, ncol = n)

  for (i in seq_len(n)) {
    for (j in seq_len(i - 1)) {
      d[i, j] <- proj_dist(history[[i]], history[[j]])
    }
  }
  stats::as.dist(d)
}

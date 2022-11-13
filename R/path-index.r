#' Compute index values for a tour history.
#'
#' @param history list of bases produced by \code{\link{save_history}}
#'   (or otherwise)
#' @param index_f index function to apply to each basis
#' @param data dataset to be projected on to bases
#' @keywords hplot
#' @seealso \code{\link{save_history}} for options to save history
#' @export
#' @examples
#' fl_holes <- save_history(flea[, 1:6], guided_tour(holes()), sphere = TRUE)
#' path_index(fl_holes, holes())
#' ## path_index(fl_holes, cmass())
#'
#' plot(path_index(fl_holes, holes()), type = "l")
#' ## plot(path_index(fl_holes, cmass()), type = "l")
#'
#' \donttest{
#' # Use interpolate to show all intermediate bases as well
#' hi <- path_index(interpolate(fl_holes), holes())
#' hi
#' plot(hi)
#' }
path_index <- function(history, index_f, data = attr(history, "data")) {
  index <- function(proj) {
    index_f(as.matrix(data) %*% proj)
  }

  structure(
    apply(history, 3, index),
    class = "path_index"
  )
}

#' Plot history index with ggplot2.
#'
#' @keywords internal hplot
#' @export
plot.path_index <- function(x, ...) {
  ggplot2::qplot(seq_along(x), unclass(x), geom = "line") +
    ggplot2::labs(x = "step", y = "index")
}

#' Compute index value for many histories.
#'
#' This is a convenience method that returns a data frame summarising the
#' index values for multiple tour paths.
#'
#' @keywords internal
#' @param bases_list list of histories produced by \code{\link{save_history}}
#' @param index_f index function to apply to each projection
#' @export
#' @examples
#' # The max.tries is low for satisfying CRAN checks
#' # Increase it for using in practice
#' holes1d <- guided_tour(holes(), 1, max.tries=2)
#' # Perform guided tour 5 times, saving results
#' tries <- replicate(5, save_history(flea[, 1:6], holes1d), simplify = FALSE)
#' # Interpolate between target bases
#' itries <- lapply(tries, interpolate)
#'
#' paths <- paths_index(itries, holes())
#' head(paths)
#'
#' if (require(ggplot2)) {
#'   ggplot(data = paths, aes(x=step, y=value, group = try)) + geom_line()
#'   ## ggplot(data = paths, aes(x=step, y=improvement, group = try)) + geom_line()
#' }
paths_index <- function(bases_list, index_f) {
  indices <- lapply(bases_list, path_index, index_f)
  data.frame(
    try = rep(seq_along(indices), sapply(indices, length)),
    step = unlist(sapply(indices, seq_along)),
    value = unlist(indices),
    improvement = unlist(lapply(indices, function(x) c(0, diff(x))))
  )
}

#' Draw the path that the geodesics took.
#'
#' This computes the projected values of each observation at each step, and
#' allows you to recreate static views of the animated plots.
#'
#' @param history list of bases produced by \code{\link{save_history}}
#'   (or otherwise)
#' @param data dataset to be projected on to bases
#' @export
#' @examples
#' path1d <- save_history(flea[, 1:6], grand_tour(1), 10)
#' path2d <- save_history(flea[, 1:6], grand_tour(2), 10)
#'
#' if (require("ggplot2")) {
#' plot(path_curves(path1d))
#' plot(path_curves(interpolate(path1d)))
#'
#' plot(path_curves(path2d))
#' plot(path_curves(interpolate(path2d)))
#'
#' # Instead of relying on the built in plot method, you might want to
#' # generate your own.  Here are few examples of alternative displays:
#'
#' df <- path_curves(path2d)
#' qplot(step, value, data = df, group = obs:var, geom = "line", colour=var) + facet_wrap( ~ obs)
#'
#' library(reshape2)
#' qplot(`1`, `2`, data = dcast(df, ... ~ var)) +
#'   facet_wrap( ~ step) +
#'   coord_equal()
#' }
path_curves <- function(history, data = attr(history, "data")) {
  history <- as.list(history)
  n <- length(history)

  project <- function(basis) {
    proj <- data %*% basis
    data.frame(
      obs = factor(row(proj)),
      var = factor(col(proj)),
      value = as.vector(proj)
    )
  }
  projections <- do.call("rbind", lapply(history, project))
  projections$step <- rep(seq_len(n), each = nrow(data) * ncol(history[[1]]))
  class(projections) <- c("path_curve", class(projections))

  projections
}

#' Plot history curves.
#'
#' The default plot method is a line plot with step on the x axis and
#' value on the y axis.  Each observation is drawn with a different line
#' line and the plot is facetted by variable.  This is rather similar in
#' spirit to a parallel coordinates plot or Andrews curves.
#'
#' For alternative ways of plotting this data, see
#'  \code{\link{path_curves}}
#' @keywords internal
#' @export
plot.path_curve <- function(x, ...) {
  ggplot2::qplot(step, value, data = x, group = obs, geom = "line") +
    ggplot2::facet_grid(var ~ .)
}
globalVariables(c("value", "obs"))

#' Draw the path that the geodesics took.
#'
#' This computes the projected values of each observation at each step, and 
#' allows you to recreate static views of the animated plots.
#'
#' @param history list of bases produced by \code{\link{save_history}} 
#'   (or otherwise)
#' @param data dataset to be projected on to bases
#' @examples
#' path1d <- save_history(flea[, 1:6], grand_tour(1), 10)
#' path2d <- save_history(flea[, 1:6], grand_tour(2), 10)
#'
#' plot(history_curves(path1d))
#' plot(history_curves(interpolate(path1d)))
#'
#' plot(history_curves(path2d))
#' plot(history_curves(interpolate(path2d)))
#'
#' # Instead of relying on the built in plot method, you might want to 
#' # generate your own.  Here are few examples of alternative displays:
#'
#' df <- history_curves(path2d)
#' qplot(step, value, data = df, group = obs:var, geom = "line", colour=var)
#' 
#' qplot(`1`, `2`, data = cast(df, ... ~ var)) + 
#'   facet_wrap( ~ step) + 
#'   coord_equal()
#' qplot(step, value, data = df, colour = var, geom="line") + 
#'   facet_wrap( ~ obs) 
history_curves <- function(history, data = attr(history, "data")) {
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
  class(projections) <- c("history_curve", class(projections))
  
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
#'  \code{\link{history_curves}}
#' @keywords internal
plot.history_curve <- function(x, ...) {
  ggplot2::qplot(step, value, data = x, group = obs, geom = "line") + 
    facet_grid(var ~ .) 
}






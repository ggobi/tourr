#' A guided tour path.
#'
#' Instead of choosing new projections at random like the grand tour, the
#' guided tour always tries to find a projection that is more interesting
#' than the current projection.
#'
#' Currently the index functions only work in 2d.
#'
#' Usually, you will not call this function directly, but will pass it to
#' a method that works with tour paths like \code{\link{animate}},
#' \code{\link{save_history}} or \code{\link{render}}.
#'
#' @param index_f the index function to optimise.
#' @param d target dimensionality
#' @param alpha the initial size of the search window, in radians
#' @param cooling the amount the size of the search window should be adjusted
#'   by after each step
#' @param search_f the search strategy to use
#' @param max.tries the maximum number of unsuccessful attempts to find
#'   a better projection before giving up
#' @param max.i the maximum index value, stop search if a larger value is found
#' @param ... arguments sent to the search_f
#' @seealso \code{\link{cmass}}, \code{\link{holes}} and \code{\link{lda_pp}}
#'   for examples of index functions.  The function should take a numeric
#'   matrix and return a single number, preferrably between 0 and 1.
#' \code{\link{search_geodesic}}, \code{\link{search_better}},
#'   \code{\link{search_better_random}} for different search strategies
#' @export
#' @examples
#' animate_xy(flea[, 1:3], guided_tour(holes()), sphere = TRUE)
#' animate_xy(flea[, 1:6], guided_tour(holes()), sphere = TRUE)
#' animate_dist(flea[, 1:6], guided_tour(holes(), 1), sphere = TRUE)
#' clrs <- c("#486030", "#c03018", "#f0a800")
#' col <- clrs[as.numeric(flea$species)]
#' animate_xy(flea[, 1:6], guided_tour(lda_pp(flea[,7])), sphere = TRUE, col=col)
#'
#' # save_history is particularly useful in conjunction with the
#' # guided tour as it allows us to look at the tour path in many different
#' # ways
#' f <- flea[, 1:3]
#' tries <- replicate(5, save_history(f, guided_tour(holes())), simplify = FALSE)
guided_tour <- function(index_f, d = 2, alpha = 0.5, cooling = 0.99, max.tries = 25, max.i = Inf, search_f = search_geodesic, ...) {

  generator <- function(current, data) {
    if (is.null(current)) return(basis_init(ncol(data), d))

    index <- function(proj) {
      index_f(as.matrix(data) %*% proj)
    }

    cur_index <- index(current)

    if (cur_index > max.i){
      cat("Found index ", cur_index, ", larger than selected maximum ", max.i, ". Stopping search.\n",
          sep="")
      cat("Final projection: \n")
      if (ncol(current)==1) {
        for (i in 1:length(current))
          cat(sprintf("%.3f",current[i])," ")
        cat("\n")
      }
      else {
        for (i in 1:nrow(current)) {
          for (j in 1:ncol(current))
            cat(sprintf("%.3f",current[i,j])," ")
          cat("\n")
        }
      }
      return(NULL)
    }

    basis <- search_f(current, alpha, index, max.tries, cur_index=cur_index, ...)
    alpha <<- alpha * cooling

    basis
  }

  new_geodesic_path("guided", generator)
}

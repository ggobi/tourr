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
#' @param search_f the search strategy to use: \code{\link{search_geodesic}}, \code{\link{search_better}},
#'   \code{\link{search_better_random}}, \code{\link{search_polish}}. Default is \code{\link{search_geodesic}}.
#' @param max.tries the maximum number of unsuccessful attempts to find
#'   a better projection before giving up
#' @param max.i the maximum index value, stop search if a larger value is found
#' @param n_sample number of samples to generate if \code{search_f} is \code{\link{search_polish}}
#' @param ... arguments sent to the search_f
#' @seealso \code{\link{cmass}}, \code{\link{holes}} and \code{\link{lda_pp}}
#'   for examples of index functions.  The function should take a numeric
#'   matrix and return a single number, preferably between 0 and 1.
#' \code{\link{search_geodesic}}, \code{\link{search_better}},
#'   \code{\link{search_better_random}} for different search strategies
#' @export
#' @examples
#' flea_std <- apply(flea[,1:6], 2, function(x) (x-mean(x))/sd(x))
#' animate_xy(flea_std, guided_tour(holes()), sphere = TRUE)
#' \donttest{
#' animate_xy(flea_std, guided_tour(holes(), search_f = search_better_random), sphere = TRUE)
#' animate_dist(flea_std, guided_tour(holes(), 1), sphere = TRUE)
#' animate_xy(flea_std, guided_tour(lda_pp(flea$species)), sphere = TRUE, col = flea$species)
#'
#' # save_history is particularly useful in conjunction with the
#' # guided tour as it allows us to look at the tour path in many different
#' # ways
#' f <- flea_std[, 1:3]
#' tries <- replicate(5, save_history(f, guided_tour(holes())), simplify = FALSE)
#' }
guided_tour <- function(index_f, d = 2, alpha = 0.5, cooling = 0.99, max.tries = 25,
                        max.i = Inf, search_f = search_geodesic, n_sample = 100, ...) {
  generator <- function(current, data, tries, ...) {
    index <- function(proj) {
      index_f(as.matrix(data) %*% proj)
    }

    valid_fun <- c(
      "search_geodesic", "search_better", "search_better_random",
      "search_polish", "search_posse"
    )
    method <- valid_fun[vapply(valid_fun, function(x) {
      identical(get(x), search_f)
    }, logical(1))]

    if (is.null(current)) {
      current <- basis_random(ncol(data), d)

      cur_index <- index(current)

      tryCatch({
        rcd_env <- parent.frame(n = 3)
        rcd_env[["record"]] <- dplyr::add_row(
          rcd_env[["record"]],
          basis = list(current),
          index_val = cur_index,
          info = "new_basis",
          method = method,
          alpha = formals(guided_tour)$alpha,
          tries = 1,
          loop = 1
        )
      },
      error = function(e){
        assign("record",
               tibble::tibble(basis = list(),
                             index_val = numeric(),
                             info = character(),
                             method = character(),
                             alpha = numeric(),
                             tries = numeric(),
                             loop = numeric()),
               envir = parent.frame())
        rcd_env[["record"]] <- tibble::tibble(
          basis = list(current),
          index_val = cur_index,
          info = "new_basis",
          method = method,
          alpha = formals(guided_tour)$alpha,
          tries = 1,
          loop = 1)
      }
      )

      return(current)
    }

    cur_index <- index(current)

    if (cur_index > max.i) {
      cat("Found index ", cur_index, ", larger than selected maximum ", max.i, ". Stopping search.\n",
        sep = ""
      )
      cat("Final projection: \n")
      if (ncol(current) == 1) {
        for (i in 1:length(current)) {
          cat(sprintf("%.3f", current[i]), " ")
        }
        cat("\n")
      }
      else {
        for (i in 1:nrow(current)) {
          for (j in 1:ncol(current)) {
            cat(sprintf("%.3f", current[i, j]), " ")
          }
          cat("\n")
        }
      }
      return(NULL)
    }

    # current, alpha = 1, index, max.tries = 5, n = 5, delta = 0.01, cur_index = NA, ..
    basis <- search_f(current, alpha, index, tries, max.tries, cur_index = cur_index, frozen = frozen, n_sample = n_sample, ...)

    if (method == "search_posse") {
      if (!is.null(basis$h)) {
        if (basis$h > 30) {
          alpha <<- alpha * cooling
        }
      }
    } else {
      alpha <<- alpha * cooling
    }

    list(target = basis$target, index = index)
  }

  new_geodesic_path("guided", generator)
}
# globalVariables(c("cur_index", "index"))

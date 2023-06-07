#' Save tour history.
#'
#' Save a tour path so it can later be displayed in many different ways.
#'
#' @param data matrix, or data frame containing numeric columns
#' @param tour_path tour path generator
#' @param max_bases maximum number of new bases to generate.  Some tour paths
#'  (like the guided tour) may generate less than the maximum.
#' @param start starting projection, if you want to specify one
#' @param rescale Default FALSE. If TRUE, rescale all variables to range [0,1]?
#' @param sphere if true, sphere all variables
#' @param step_size distance between each step - defaults to \code{Inf} which
#'   forces new basis generation at each step.
#' @param ... additional arguments passed to tour path
#' @export
#' @examples
#' # You can use a saved history to replay tours with different visualisations
#'
#' t1 <- save_history(flea[, 1:6], max = 3)
#' animate_xy(flea[, 1:6], planned_tour(t1))
#' ## andrews_history(t1)
#' ## andrews_history(interpolate(t1))
#'
#' ## t1 <- save_history(flea[, 1:6], grand_tour(4), max = 3)
#' ## animate_pcp(flea[, 1:6], planned_tour(t1))
#' ## animate_scatmat(flea[, 1:6], planned_tour(t1))
#'
#' ## t1 <- save_history(flea[, 1:6], grand_tour(1), max = 3)
#' ## animate_dist(flea[, 1:6], planned_tour(t1))
#'
#' testdata <- matrix(rnorm(100 * 3), ncol = 3)
#' testdata[1:50, 1] <- testdata[1:50, 1] + 10
#' testdata <- sphere_data(testdata)
#' t2 <- save_history(testdata, guided_tour(holes(), max.tries = 10),
#'   max = 5
#' )
#' animate_xy(testdata, planned_tour(t2))
#'
#' # Or you can use saved histories to visualise the path that the tour took.
#' plot(path_index(interpolate(t2), holes()))
save_history <- function(data, tour_path = grand_tour(), max_bases = 100, start = NULL,
                         rescale = FALSE, sphere = FALSE, step_size = Inf, ...) {
  if (!is.matrix(data)) {
    message("Converting input data to the required matrix format.")
    data <- as.matrix(data)
  }
  if (rescale) data <- rescale(data)
  if (sphere) data <- sphere_data(data)

  record <-
    tibble::tibble(
      basis = list(),
      index_val = numeric(),
      info = character(),
      method = character(),
      alpha = numeric(),
      tries = numeric(),
      loop = numeric()
    )

  if (is.null(start)) {
    start <- tour_path(NULL, data, ...)
  }
  tour <- new_tour(data, tour_path, start, ...)

  projs <- array(NA, c(ncol(data), ncol(start), max_bases + 1))
  projs[,,1] <- start
  princ_dirs <- projs

  i <- 1
  while (i < max_bases) {
    i <- i + 1
    # An infinite step size forces the tour path to generate a new basis
    # every time, so no interpolation occurs.
    step <- tour(step_size)
    if (is.null(step$target)) break # this can happen if no better basis is found in guided tour

    projs[, , i] <- step$target
    if (step$step < 0) break # already appended final projection, break directly
  }

  # Remove empty matrices for tours that terminated early
  # (e.g. guided tour)
  empty <- apply(projs, 3, function(x) all(is.na(x)))
  projs <- projs[, , !empty, drop = FALSE]
  if (length(projs) == 0) {
    return(NULL)
  }

  attr(projs, "data") <- data
  structure(projs, class = "history_array")
}

#' @export
"[.history_array" <- function(x, i = TRUE, j = TRUE, k = TRUE, ...) {
  piece <- .subset(x, i, j, k, drop = FALSE)
  structure(piece,
    data = attr(x, "data"),
    class = class(x)
  )
}

#' @export
"[[.history_array" <- function(x, i, ...) {
  as.matrix(.subset(x, TRUE, TRUE, i, drop = FALSE))
}

#' @export
length.history_array <- function(x) dim(x)[3]

#' @export
str.history_array <- function(object, ...) utils::str(unclass(object))

#' @export
print.history_array <- function(x, ...) {
  attr(x, "data") <- NULL
  NextMethod()
}

#' @export
as.list.history_list <- function(x, ...) x

#' @export
as.list.history_array <- function(x, ...) {
  projs <- do.call("c", apply(x, 3, list))
  structure(projs, class = "history_list", data = attr(x, "data"))
}

#' @export
as.array.history_array <- function(x, ...) x

#' @export
as.array.history_list <- function(x, ...) {
  dims <- c(nrow(x[[1]]), ncol(x[[1]]), length(x))
  projs <- array(NA, dims)
  for (i in seq_along(x)) {
    projs[, , i] <- x[[i]]
  }
  structure(projs, class = "history_array", data = attr(x, "data"))
}

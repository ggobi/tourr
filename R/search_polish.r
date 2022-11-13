#' Search very locally to find slightly better projections to polish a broader search.

#' @param current the current projection basis
#' @param alpha the angle used to search the target basis from the current basis
#' @param index index function
#' @param tries the counter of the outer loop of the opotimiser
#' @param polish_max_tries maximum number of iteration before giving up
#' @param cur_index the index value of the current basis
#' @param n_sample number of samples to generate
#' @param polish_cooling percentage of reduction in polish_alpha when no better basis is found
#' @param ... other arguments being passed into the \code{search_polish()}
#' @keywords optimize
#' @export
#' @examples
#' data(t1)
#' best_proj <- t1[, , dim(t1)[3]]
#' attr(best_proj, "data") <- NULL
#' best_proj <- unclass(drop(best_proj))
#' animate_xy(
#'   flea[, 1:6],
#'   guided_tour(holes()),
#'     search_f = search_polish(
#'          polish_max_tries = 5),
#'   start = best_proj
#' )
search_polish <- function(current, alpha = 0.5, index, tries, polish_max_tries = 30,
                          cur_index = NA, n_sample = 100, polish_cooling = 1, ...) {
  if (is.na(cur_index)) cur_index <- index(current)
  try <- 1

  while (try <= polish_max_tries) {
    # could use replicate here
    basis <- lapply(1:n_sample, function(x) {
      tibble::tibble(basis = list(basis_nearby(current,
        alpha = alpha
      )))
    })
    polish <- dplyr::mutate(dplyr::bind_rows(basis),
      index_val = vapply(basis, function(x) index(x), double(1)),
      alpha = round(alpha, 4), tries = tries, info = "polish",
      loop = try, method = "search_polish"
    )

    # could use which.max
    best_row <- dplyr::filter(polish, index_val == max(index_val))
    best_row <- dplyr::mutate(best_row, info = "loop_best", method = "search_polish")

    rcd_env <- parent.frame(n = 4)
    if (is.null(rcd_env[["record"]])) rcd_env <- parent.frame(n = 1)
    rcd_env[["record"]] <- dplyr::bind_rows(rcd_env[["record"]], polish, best_row)
    rcd_env[["record"]] <- dplyr::mutate(
      rcd_env[["record"]],
      id = dplyr::row_number()
    )

    if (best_row$index_val > cur_index) {
      polish_dist <- proj_dist(current, best_row$basis[[1]])
      polish_pdiff <- (best_row$index_val - cur_index) / cur_index

      # check condition 1: the two bases can't be too close

      if (polish_dist < 1e-3) {
        cat("The new basis is too close to the current one! \n")
        cat("current basis: ", current, "cur_index: ", cur_index, "\n")
        return(list(target = current, alpha = alpha))
      }

      # check condition 2: there needs to be certain improvement

      if (polish_pdiff < 1e-5) {
        cat("The improvement is too small! \n")
        cat("current basis: ", current, "cur_index: ", cur_index, "\n")
        return(list(target = current, alpha = alpha))
      }

      cat("better basis found, index_val = ", best_row$index_val, "\n")
      cur_index <- best_row$index_val
      current <- best_row$basis[[1]]
      best_row <- dplyr::mutate(best_row,
        info = "loop_best",
        method = "search_polish"
      )

      rcd_env[["record"]] <- dplyr::bind_rows(rcd_env[["record"]], best_row)

    } else {
      polish_cooling <- polish_cooling * 0.95
      alpha <- alpha * polish_cooling
      cat("alpha gets updated to", alpha, "\n")

      # check condition 3: alpha can't be too small

      if (alpha < 0.01) {
        cat("alpha is", alpha, "and it is too small! \n")
        cat("current basis: ", current, "cur_index: ", cur_index, "\n")
        return(list(target = current, alpha = alpha))
      }
    }

    try <- try + 1
  }

  cat("No better bases found after ", polish_max_tries, " tries.  Giving up.\n",
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

  cat("current basis: ", current, "cur_index: ", cur_index, "\n")
  return(list(target = current, alpha = alpha))
}
# globalVariables("index_val")

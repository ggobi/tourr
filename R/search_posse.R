#' Search for a better projection based on Poss, 1995
#' @param current starting projection
#' @param alpha the angle used to search the target basis from the current basis
#' @param index index function
#' @param tries the counter of the outer loop of the opotimiser
#' @param max.tries maximum number of iteration before giving up
#' @param cur_index the index value of the current basis
#' @param ... other arguments being passed into the \code{search_better()}
#' @keywords optimize
#' @export
search_posse <- function(current, alpha = 0.5, index, tries, max.tries = 300, cur_index = NA, ...) {
  if (is.na(cur_index)) cur_index <- index(current)

  try <- 1
  h <- 0

  while (try < max.tries) {
    new_basis <- orthonormalise(current + alpha * basis_random(nrow(current), ncol(current)))
    new_index <- index(new_basis)

    rcd_env <- parent.frame(n = 4)
    rcd_env[["record"]] <- dplyr::add_row(
      rcd_env[["record"]],
      basis = list(new_basis),
      index_val = new_index,
      info = "random_search",
      tries = tries,
      loop = try,
      method = "search_posse",
      alpha = round(alpha, 4)
    )

    if (new_index > cur_index) {
      cat("New", new_index, "try", try, "\n")

      nr <- nrow(rcd_env[["record"]])
      rcd_env[["record"]][nr, "info"] <- "new_basis"

      return(list(target = new_basis, h = h))
    } else {
      h <- h + 1
    }

    try <- try + 1
  }

  cat("No better bases found after ", max.tries, " tries.  Giving up.\n",
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

  NULL
}

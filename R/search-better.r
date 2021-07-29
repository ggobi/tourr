#' Generate nearby bases, e.g. for simulated annealing.
#' @keywords internal
basis_nearby <- function(current, alpha = 0.5, method = "linear") {
  method <- match.arg(method, c("linear", "geodesic"))
  new <- basis_random(nrow(current), ncol(current))

  switch(method,
    linear = orthonormalise((1 - alpha) * current + alpha * new),
    geodesic = step_fraction(geodesic_info(current, new), alpha)
  )
}

#' check if the current and target bases are of the same orientation
#' @keywords internal
correct_orientation <- function(current, target){

  for (i in ncol(current)){
    if (det(t(current[,i]) %*% target[,i]) < 0){
      target[,i] <- -target[,i]
    }
  }

  return(target)
}


#' Search for a better projection near the current projection.
#'
#' @param current starting projection
#' @param alpha the angle used to search the target basis from the current basis
#' @param index index function
#' @param tries the counter of the outer loop of the opotimiser
#' @param max.tries maximum number of iteration before giving up
#' @param ... other arguments being passed into the \code{search_better()}
#' @param method whether the nearby bases are found by a linear/ geodesic formulation
#' @param cur_index the index value of the current basis
#' @keywords optimize
#' @importFrom utils tail globalVariables
#' @export
#' @examples
#' animate_xy(flea[, 1:6], guided_tour(holes(), search_f = search_better))
search_better <- function(current, alpha = 0.5, index, tries, max.tries = Inf,...,
                          method = "linear", cur_index = NA) {

  if (is.na(cur_index)) cur_index <- index(current)

  if (cur_index == 0) {
    warning("cur_index is zero!")
  }

  cat("Old", cur_index, "\n")
  try <- 1

  while (try < max.tries) {
    new_basis <- basis_nearby(current, alpha, method)
    new_index <- index(new_basis)

    rcd_env <- parent.frame(n = 4)
    if (is.null(rcd_env[["record"]])) rcd_env <- parent.frame(n = 1)
    rcd_env[["record"]] <- dplyr::add_row(
      rcd_env[["record"]],
      basis = list(new_basis),
      index_val = new_index,
      info = "random_search",
      tries = tries,
      loop = try,
      method = "search_better",
      alpha = round(alpha, 4)
    )


    if (new_index > cur_index) {
      cat("New", new_index, "try", try, "\n")

      nr <- nrow(rcd_env[["record"]])
      rcd_env[["record"]][nr, "info"] <- "new_basis"

      new_basis <- correct_orientation(current, new_basis)
      rcd_env[["record"]][[nr, "basis"]] <- list(new_basis)

      return(list(target = new_basis, alpha = alpha))
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

  rcd_env[["record"]] <- dplyr::mutate(
    rcd_env[["record"]],
    id = dplyr::row_number()
  )
  NULL
}

#' Search for a better projection using simulated annealing
#'
#' Given an initial \eqn{t0}, the cooling scheme updates temperature at \deqn{T = t0 /\log(i + 1)}
#' The candidate basis is sampled via \deqn{B_j = (1 - \alpha) * B_i + \alpha * B} where alpha defines the neighbourhood, \eqn{B_i} is the current basis, B is a randomly generated basis
#' The acceptance probability is calculated as \deqn{prob = \exp{-abs(I(B_i) - I(B_j))/ T}}
#' For more information, see
#' \url{https://projecteuclid.org/download/pdf_1/euclid.ss/1177011077}
#'
#' @param current starting projection
#' @param alpha the angle used to search the target basis from the current basis
#' @param index index function
#' @param tries the counter of the outer loop of the opotimiser
#' @param max.tries maximum number of iteration before giving up
#' @param method whether the nearby bases are found by a linear/ geodesic formulation
#' @param cur_index the index value of the current basis
#' @param t0 initial decrease in temperature
#' @param ... other arguments being passed into the \code{search_better_random()}
#'
#' @keywords optimize
#' @export
#' @examples
#' animate_xy(flea[, 1:6], guided_tour(holes(), search_f = search_better_random))
search_better_random <- function(current, alpha = 0.5, index, tries,
                                 max.tries = Inf, method = "linear", cur_index = NA, t0 = 0.01,
                                 ...) {
  if (is.na(cur_index)) cur_index <- index(current)

  if (cur_index == 0) {
    warning("cur_index is zero!")
  }

  cat("Old", cur_index, "\n")
  try <- 1
  while (try < max.tries) {
    new_basis <- basis_nearby(current, alpha, method)
    new_index <- index(new_basis)
    temperature <- t0 / log(try + 1)

    rcd_env <- parent.frame(n = 4)
    rcd_env[["record"]] <- dplyr::add_row(
      rcd_env[["record"]],
      basis = list(new_basis),
      index_val = new_index,
      info = "random_search",
      tries = tries,
      loop = try,
      method = "search_better_random",
      alpha = round(alpha, 4)
    )

    if (new_index > cur_index) {
      cat("New", new_index, "try", try, "\n")
      cat("Accept \n")

      nr <- nrow(rcd_env[["record"]])
      rcd_env[["record"]][nr, "info"] <- "new_basis"

      new_basis <- correct_orientation(current, new_basis)
      rcd_env[["record"]][[nr, "basis"]] <- list(new_basis)

      return(list(target = new_basis, alpha = alpha))
    }
    else {
      prob <- min(exp(-abs(cur_index - new_index) / temperature), 1)
      rand <- stats::runif(1)

      if (prob > rand) {
        cat("New", new_index, "try", try, "\n")
        cat("Accept with probability, prob =", prob, "\n")

        nr <- nrow(rcd_env[["record"]])
        rcd_env[["record"]][nr, "info"] <- "new_basis"

        rcd_env[["record"]] <- dplyr::mutate(
          rcd_env[["record"]],
          id = dplyr::row_number()
        )

        return(list(target = new_basis, alpha = alpha))
      }
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

  rcd_env[["record"]] <- dplyr::mutate(
    rcd_env[["record"]],
    id = dplyr::row_number()
  )
  NULL
}

# globalVariables(c("t0","tries", "info", "runif"))

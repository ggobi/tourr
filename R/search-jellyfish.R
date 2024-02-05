#' An jellyfish optimisers for the projection pursuit guided tour
#'
#' @param current starting projection, a list of basis of class "multi-bases"
#' @param index index function
#' @param tries the counter of the outer loop of the opotimiser
#' @param max.tries maximum number of iteration before giving up
#' @param ... other arguments being passed into the \code{search_jellyfish()}
#' @keywords optimize
#' @export
#' @examples
#' res <- animate_xy(flea[, 1:6], guided_tour(holes(), search_f = search_jellyfish))
#' res
search_jellyfish <- function(current, index, tries, max.tries = Inf, min.tries = 1, ...) {
  rcd_env <- parent.frame(n = 4)
  if (is.null(rcd_env[["record"]])) rcd_env <- parent.frame(n = 1)
  best_jelly <- current[[attr(current, "best_id")]]
  current_idx <- index(best_jelly)

  c_t = abs((1 - tries / max.tries) * (2 * runif(1) - 1))

  if (c_t >= 0.5) {
    trend = best_jelly - 3 * runif(1) * Reduce("+", current) / length(current) # eq 9
    target = lapply(current, function(x) {
      orthonormalise(x + runif(1) * trend)
    })
  } else if (runif(1) > (1 - c_t)) {
    # type A passive
    target = lapply(current, function(x) {
      orthonormalise(x + 0.1 * runif(1) * 2)
    }) # eq 12
  } else {
    # type B active
    # generate random jelly fish j and its index value
    update_typeB <- function(jelly_i, jellies) {
      jelly_j = jellies[[sample(1:length(jellies), 1)]]
      if (index(jelly_i) > index(jelly_j)) {
        new_i = orthonormalise(jelly_i + runif(1) * (jelly_i - jelly_j))
      } else {
        new_i = orthonormalise(jelly_i + runif(1) * (jelly_j - jelly_i)) # eq 16
      }
      return(new_i)
    }

    target = lapply(current, function(i) {update_typeB(i, current)})
  }

  target <- purrr::map2(current, target, correct_orientation)
  target_idx <- sapply(target, index)

  best_id <- which.max(target_idx)
  cat("Best Index: ", max(target_idx), "\n")
  attr(target, "best_id") <- best_id
  class(target) <- c("multi-bases", class(target))

  rcd_env[["record"]] <- dplyr::add_row(
    rcd_env[["record"]], basis = unclass(target),
    index_val = target_idx, info = "jellyfish_update",
    tries = tries, method = "search_jellyfish", alpha = NA,
    loop = 1:length(target)
  )
  rcd_env[["record"]] <- dplyr::mutate(
    rcd_env[["record"]],
    info = ifelse(tries == max(tries) & loop == best_id, "current_best", info)
  )


  if (abs(max(target_idx) - current_idx) < 0.05  &&
      tries >= max.tries) {
    print_final_proj(target[[attr(target, "best_id")]])
    rcd_env[["record"]] <- dplyr::mutate(
      rcd_env[["record"]],
      id = dplyr::row_number()
    )
    NULL
  } else{
    tries <- tries + 1
    return(list(target = target))
  }
}


print_final_proj <- function(current){
  cat("Final projection: \n")
  if (ncol(current) == 1) {
    for (i in 1:length(current)) {
      cat(sprintf("%.3f", current[i]), " ")
    }
    cat("\n")
  } else {
    for (i in 1:nrow(current)) {
      for (j in 1:ncol(current)) {
        cat(sprintf("%.3f", current[i, j]), " ")
      }
      cat("\n")
    }
  }
}

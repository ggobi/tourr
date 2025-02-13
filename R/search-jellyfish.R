#' A jellyfish optimiser for projection pursuit guided tour
#'
#' @param current starting projection, a list of basis of class "multi-bases"
#' @param index index function
#' @param tries the counter of the outer loop of the opotimiser
#' @param max.tries the maximum number of iteration before giving up
#' @param ... other arguments being passed into the \code{search_jellyfish()}
#' @keywords optimize
#' @importFrom stats runif
#' @rdname jellyfish
#' @export
#' @examples
#' library(dplyr)
#' data(flea)
#' res <- animate_xy(flea[, 1:6], guided_tour(lda_pp(cl = flea$species),
#' search_f = search_jellyfish))
#' bases <- res |> filter(loop == 1) |> pull(basis) |> check_dup(0.1)
#' animate_xy(data = flea[,1:6], tour_path = planned_tour(bases), col = flea$species)
search_jellyfish <- function(current, index, tries, max.tries = 50, ...) {
  rcd_env <- parent.frame(n = 4)
  if (is.null(rcd_env[["record"]])) rcd_env <- parent.frame(n = 1)
  best_jelly <- current[[attr(current, "best_id")]]
  current_idx <- sapply(current, index)

  c_t = abs((1 - tries / max.tries) * (2 * runif(1) - 1))

  if (c_t >= 0.5) {
    trend = best_jelly - 3 * runif(1) * Reduce("+", current) / length(current) # eq 9
    target = lapply(current, function(x) {
      orthonormalise(x + runif(1) * trend)
    })
  } else if (runif(1) > (1 - c_t)) {
    # type A passive
    target = lapply(current, function(x) {
      orthonormalise(x + 0.1 * runif(1))
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

  target <- mapply(correct_orientation, current, target)
  target_idx <- sapply(target, index)

  # if the target is worse than current, use current
  worse_id <- current_idx > target_idx
  target[worse_id] <- current[worse_id]
  target_idx[worse_id] <- current_idx[worse_id]

  best_id <- which.max(target_idx)
  #message("Target: ", sprintf("%.3f", max(target_idx)))
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

  if (tries >= max.tries) {
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

#' @param bases a list of bases extracted from the data collection object, see examples
#' @param min_dist the minimum distance between two bases
#' @rdname jellyfish
#' @export
check_dup <- function(bases, min_dist) {
  res <- list()
  res[[1]] <- bases[[1]]
  i <- 1; j <- 2
  while(j <= length(bases)){
    if (proj_dist(bases[[i]], bases[[j]]) >= min_dist) {
      res <- c(res, list(bases[[j]]))
      i <- i + 1; j <- j + 1
    } else{
      j <- j + 1
    }
  }
  return(res)
}

globalVariables(c("loop"))

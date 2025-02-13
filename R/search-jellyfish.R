#' A jellyfish optimiser for projection pursuit guided tour
#'
#' @param current starting projection, a list of basis of class "multi-bases"
#' @param index index function
#' @param tries the counter of the outer loop of the opotimiser
#' @param max.tries the maximum number of iteration before giving up
#' @param ... other arguments being passed into the \code{search_jellyfish()}
#' @param verbose whether to print out the progress messages
#' @keywords optimize
#' @importFrom stats runif
#' @rdname jellyfish
#' @export
#' @examples
#' library(dplyr)
#' res <- animate_xy(flea[, 1:6], guided_tour(lda_pp(cl = flea$species),
#' search_f = search_jellyfish))
#' bases <- res |> filter(loop == 1) |> pull(basis) |> check_dup(0.1)
#' animate_xy(data = flea[,1:6], tour_path = planned_tour(bases), col = flea$species)
search_jellyfish <- function(current, index, tries, max.tries = 50, verbose = FALSE, ...) {
  rcd_env <- parent.frame(n = 4)
  if (is.null(rcd_env[["record"]])) rcd_env <- parent.frame(n = 1)
  best_jelly <- current[[attr(current, "best_id")]]
  last_tries <- tries - 1
  if (tries == 2){
    current_idx <- sapply(current, index)
  } else{
    current_idx <- rcd_env[["record"]] |>
      dplyr::filter(tries == last_tries) |>
      dplyr::pull(index_val)
  }

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
    update_typeB <- function(current, idx){
      idx_jelly_i <- current_idx[idx]; jelly_i <- current
      jelly_j_no <- sample(1:length(current_idx), 1)
      idx_jelly_j <- current_idx[jelly_j_no]
      current_basis <- rcd_env[["record"]] |>
        dplyr::filter(tries == last_tries) |> dplyr::pull(basis)
      jelly_j <- current_basis[[jelly_j_no]]
      if (idx_jelly_i > idx_jelly_j) {
        new_i = orthonormalise(jelly_i + runif(1) * (jelly_i - jelly_j))
      } else {
        new_i = orthonormalise(jelly_i + runif(1) * (jelly_j - jelly_i)) # eq 16
      }
      return(new_i)
    }
    target <- mapply(update_typeB, current, 1:length(current), SIMPLIFY = FALSE)
    #target = lapply(current, function(i) {update_typeB(i, current)})
  }

  target <- mapply(correct_orientation, current, target, SIMPLIFY = FALSE)
  target_idx <- sapply(target, index)

  # check for NA for index computation (skinny gives NA if the alphahull can't be computed)
  na_idx <- which(is.na(target_idx))
  if (length(na_idx) > 0) {
    target[na_idx] <- current[na_idx]
    target_idx[na_idx] <- current_idx[na_idx]
  }

  # if the target is worse than current, use current
  worse_id <- current_idx > target_idx
  target[worse_id] <- current[worse_id]
  target_idx[worse_id] <- current_idx[worse_id]

  best_id <- which.max(target_idx)
  if (verbose) message("Target: ", sprintf("%.3f", max(target_idx)))
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

globalVariables(c("loop", "basis"))

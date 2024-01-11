search_jellyfish <- function(current, alpha = 0.5, index, tries,
                             max.tries = Inf, ..., cur_index = NA) {
  rcd_env <- parent.frame(n = 4)
  if (is.null(rcd_env[["record"]])) rcd_env <- parent.frame(n = 1)
  best_jelly <- current[[attr(current, "best_id")]]

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

  target_idx <- sapply(target, index)
  best_id <- which.max(target_idx)
  cat("Best Index: ", max(target_idx), "\n")
  attr(target, "best_id") <- best_id
  class(target) <- c("multi-bases", class(target))

  rcd_env[["record"]] <- dplyr::add_row(
    rcd_env[["record"]], basis = list(target[-best_id]),
    index_val = target_idx[-best_id], info = "jellyfish_update",
    tries = tries, method = "jellyfish_optimiser", alpha = NA
  )
  rcd_env[["record"]] <- dplyr::add_row(
    rcd_env[["record"]], basis = list(target[best_id]),
    index_val = max(target_idx), info = "current_best", tries = tries,
    method = "jellyfish_optimiser", alpha = NA
  )


  if (diff(quantile(target_idx, c(0.05, 0.95))) < 0.05 || tries > max.tries) {
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

#' Search very locally to find slightly better projections to polish a broader search.

#' @param current the current projection basis
#' @param alpha the angle used to search the target basis from the current basis
#' @param index index function
#' @param polish_max_tries maximum number of iteration before giving up
#' @param cur_index the index value of the current basis
#' @param n_sample number of samples to generate
#' @param polish_cooling percentage of reduction in polish_alpha when no better basis is found
#' @param ... other arguments being passed into the \code{search_polish()}
#' @keywords optimize
#' @export
search_polish <- function(current, alpha = 0.5, index, polish_max_tries = 30,
                          cur_index = NA, n_sample = 5, polish_cooling = 1, ...){

  if (is.na(cur_index)) cur_index <- index(current)
  try <- 1

  while (try <= polish_max_tries){
    # could use replicate here
    basis <- lapply(1:n_sample, function(x) {dplyr::tibble(basis = list(basis_nearby(current,
                                                                              alpha = alpha)))})
    polish <-  dplyr::mutate(dplyr::bind_rows(basis),
                             index_val = vapply(basis, function(x) index(x), double(1)),
                             alpha = round(alpha, 4), tries = tries, info = "polish",
                             loop = try, method = "search_polish")

    # could use which.max
    best_row <- dplyr::filter(polish, index_val == max(index_val))

    if (getOption("tourr.verbose", default = FALSE))
      best_row <- dplyr::mutate(best_row,
                                info = "loop_best",
                                method = "search_polish")
      record <<- dplyr::bind_rows(polish, record, best_row)
    if (best_row$index_val > cur_index) {

      polish_dist <- proj_dist(current, best_row$basis[[1]])
      polish_pdiff <-  (best_row$index_val - cur_index)/cur_index

      # check condition 1: the two bases can't be too close

      if (polish_dist <  1e-3) {
        cat("The new basis is too close to the current one! \n")
        cat("current basis: ", current, "cur_index: ", cur_index, "\n")
        cur_index <<- cur_index
        current <<- current
        return(list(target = current, alpha = alpha))
      }

      #check condition 2: there needs to be certain improvement

      if (polish_pdiff < 1e-5){
        cat("The improvement is too small! \n")

        cat("current basis: ", current, "cur_index: ", cur_index, "\n")
        cur_index <<- cur_index
        current <<- current
        return(list(target = current, alpha = alpha))

      }

      cat("better basis found, index_val = ", best_row$index_val, "\n")

      cur_index <- best_row$index_val
      current <- best_row$basis[[1]]
      if (getOption("tourr.verbose", default = FALSE))
        best_row <- dplyr::mutate(best_row,
                                  info = "loop_best",
                                  method = "search_polish")
        record <<- dplyr::bind_rows(record, best_row)


    } else {

      polish_cooling <-  polish_cooling * 0.95
      alpha <- alpha * polish_cooling
      cat("alpha gets updated to", alpha, "\n")

      # check condition 3: alpha can't be too small

      if (alpha < 0.01){
        cat("alpha is", alpha, "and it is too small! \n")

        cat("current basis: ", current, "cur_index: ", cur_index, "\n")
        cur_index <<- cur_index
        current <<- current
        return(list(target = current, alpha = alpha))

      }
    }

    try <- try + 1

  }

  cat("No better bases found after ", polish_max_tries, " tries.  Giving up.\n",
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

  cat("current basis: ", current, "cur_index: ", cur_index, "\n")
  cur_index <<- cur_index
  current <<- current
  return(list(target = current, alpha = alpha))

}
#globalVariables("index_val")

#' Search for most interesting projection along random geodesics.

#' @param current the current projeciton basis
#' @param polish_alpha the angle used to search the target basis from the current basis
#' @param index index function
#' @param max.tries maximum number of iteration before giving up
#' @param cur_index the index value of the current basis
#' @param n_sample number of samples to generate
#' @param polish_cooling percentage of reductio in polish_alpha when no better basis is found
#' @keywords optimize
#'
#' You should not to have call this function directly, but should supply it
#' to the \code{\link{guided_tour}} as a search strategy.


search_polish <- function(current, polish_alpha = 0.5, index, polish_max_tries = 30,
                          cur_index = NA, n_sample = 1000, polish_cooling = 1, ...){

  basis <- rlang::sym("basis")
  index_val <- rlang::sym("index_val")
  try <- rlang::sym("try")

  if (is.na(cur_index)) cur_index <- index(current)
  try <- 1

  while (try <= polish_max_tries){

    polish <- purrr::map_dfr(1:n_sample, ~tibble::tibble(basis = list(basis_nearby(current,
                                                                                   alpha = polish_alpha)))) %>%
      dplyr::mutate(index_val = purrr::map_dbl(basis, ~index(.x)),
                    polish_alpha = polish_alpha, tries = !! tries, info = "polish",
                    loop = try)

    best_row <- polish %>% dplyr::filter(index_val == max(index_val))

    if(best_row$index_val > cur_index){

      polish_dist <- proj_dist(current, best_row$basis[[1]])
      polish_pdiff <-  (best_row$index_val - cur_index)/cur_index

      cur_index <- best_row$index_val
      current <- best_row$basis[[1]]


      # check condition 1: the two bases can't be too close

      if(polish_dist <  1e-3){
        cat("The new basis is too close to the current one! \n")
        if (verbose){
          cat("current basis: ", current, "cur_index: ", cur_index, "\n")
          return(list(record = record, target = current))
        } else {
          cat("current basis: ", current, "cur_index: ", cur_index, "\n")
          return(list(target = current))
        }

      }

      #check condition 2: there needs to be certain improvement

      if (polish_pdiff < 1e-10){
        cat("The improvement is too small! \n")
        if (verbose){
          cat("current basis: ", current, "cur_index: ", cur_index, "\n")
          return(list(record = record, target = current))
        } else {
          cat("current basis: ", current, "cur_index: ", cur_index, "\n")
          return(list(target = current))
        }
      }

      cat("better basis found, index_val = ", best_row$index_val, "\n")

      if (verbose)
        record <<- record %>% dplyr::bind_rows(polish) %>%
          dplyr::bind_rows(best_row %>% dplyr::mutate(info = "polish_best"))

    }else{

      polish_cooling <-  polish_cooling * 0.95
      polish_alpha <- polish_alpha * polish_cooling
      cat("polish_alpha gets updated to", polish_alpha, "\n")

      # check condition 3: polish_alpha can't be too small

      if (polish_alpha < 0.01){
        cat("polish_alpha is", polish_alpha, "and it is too small! \n")
        if (verbose){
          cat("current basis: ", current, "cur_index: ", cur_index, "\n")
          return(list(record = record, target = current))
        } else {
          cat("current basis: ", current, "cur_index: ", cur_index, "\n")
          return(list(target = current))
        }
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

  NULL

}

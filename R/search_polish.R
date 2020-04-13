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


search_polish <- function(current, polish_alpha = 0.05, index, max.tries = 5,
                          cur_index = NA, n_sample = 1000, polish_cooling = 1, ...){

  basis <- rlang::sym("basis")
  polish_alpha <- rlang::sym("polish_alpha")
  index_val <- rlang::sym("index_val")
  tries <- rlang::sym("tries")
  try <- rlang::sym("try")

  if (is.na(cur_index)) cur_index <- index(current)
  try <- 1

  while (try < max.tries){

    polish <- purrr::map_dfr(1:n_sample, ~tibble::tibble(basis = list(basis_nearby(current,
                                                alpha = polish_alpha))), .id = "id") %>%
      dplyr::mutate(index_val = purrr::map_dbl(basis, ~index(.x)),
                    alpha = polish_alpha, tries = tries, info = "polish",
                    loop = try, id = 0)

    best_row <- polish %>% dplyr::filter(index_val == max(index_val))

    if(best_row$index_val > cur_index){
      if(proj_dist(current, best_row$basis[[1]] < 1e-3)){
        cat("The new basis is too close to the current one! \n")
      }else{
        cur_index <<- best_row$index_val
        cat("better basis found, index_val = ", best_row$index_val, "\n")

        record <<- record %>% dplyr::bind_rows(polish) %>%
          dplyr::bind_rows(best_row %>% dplyr::mutate(info = "polish_best"))

        target <- best_row %>% dplyr::pull(basis) %>% utils::tail(1)

        return(list(record = record,
                    target = target[[1]]))

      }
    }

    try <- try + 1

    polish_alpha <<- polish_alpha * polish_cooling
    cat("new polish alpha: ", polish_alpha * polish_cooling, "\n")


  }

  cat("No better bases found after ", max.tries, " tries.  Giving up.\n",
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

#' Search for a better projection based on Poss, 1995
#' @keywords internal
search_posse <- function(current, posse_alpha = 0.5, index, max.tries = 300, cur_index = NA,
                         cooling = 0.9){
  #browser()

  info <- rlang::sym("info")
  basis <- rlang::sym("basis")

  if (is.na(cur_index)) cur_index <- index(current)

  try <- 1
  h <- 0

  while (try < max.tries){
    new_basis <- orthonormalise(current + posse_alpha * basis_random(nrow(current), ncol(current)))
    new_index <- index(new_basis)

    if (verbose)
      record <<- record %>% dplyr::add_row(basis = list(new_basis),
                                           index_val = new_index,
                                           info = "random_search",
                                           tries = tries,
                                           loop = try,
                                           method = "search_posse",
                                           alpha = round(posse_alpha,4))
    if (new_index > cur_index) {
      cat("New", new_index, "try", try, "\n")

      if (verbose) {
        record <<- record %>%
          dplyr::mutate(row = dplyr::row_number(),
                        info = ifelse(row == max(row), "new_basis", !!info)) %>%
          dplyr::select(-row)

        return(list(record = record, target = new_basis, h = h))
      }else{

        return(list(target = new_basis, h = h))
      }

    } else {
      h <- h + 1
    }

    try <- try + 1

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

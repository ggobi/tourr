#' Generate nearby bases, e.g. for simulated annealing.
#' @keywords internal
basis_nearby <- function(current, alpha = 0.5, method = "linear") {
  method <- match.arg(method, c("linear", "geodesic"))
  new <- basis_random(nrow(current), ncol(current))

  switch(method,
    linear =   orthonormalise((1 - alpha) * current + alpha * new),
    geodesic = step_fraction(geodesic_info(current, new), alpha)
  )
}


#' Search for a better projection near the current projection.
#' @keywords internal
search_better <- function(current, alpha = 0.5, index, max.tries = Inf,
  method = "linear", cur_index = NA, cooling = 0.99, ...) {

  info <- rlang::sym("info")
  basis <- rlang::sym("basis")

  if (is.na(cur_index)) cur_index <- index(current)

  if(cur_index == 0){
    warning("cur_index is zero!")
  }

  cat("Old", cur_index, "\n")
  try <- 1

  while (try < max.tries) {
    new_basis <- basis_nearby(current, alpha)
    new_index <- index(new_basis)

    if (verbose)
      record <<- record %>% dplyr::add_row(basis = list(new_basis),
                                          index_val = new_index,
                                          info = "random_search",
                                          tries = tries,
                                          loop = try)

    if (new_index > cur_index) {
      cat("New", new_index, "try", try, "\n")

      if (verbose) {
        record <<- record %>%
          dplyr::mutate(row = dplyr::row_number(),
                      info = ifelse(row == max(row), "new_basis", !!info)) %>%
          dplyr::select(-row)

        return(list(record = record, target = new_basis))
      }else{
        return(list(target = new_basis))
      }

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

#' Search for better projection, with stochastic component.
#' @keywords internal
search_better_random <- function(current, alpha = 0.5, index,
  max.tries = Inf, method = "linear", eps = 0.001, cur_index = NA,
  ...
) {

  info <- rlang::sym("info")
  basis <- rlang::sym("basis")


  if (is.na(cur_index)) cur_index <- index(current)

  if(cur_index == 0){
    warning("cur_index is zero!")
  }

  cat("Old", cur_index, "\n")
  try <- 1
  while(try < max.tries) {
    new_basis <- basis_nearby(current, alpha, method)
    new_index <- index(new_basis)

    if (verbose)
      record <<- record %>% dplyr::add_row(basis = list(new_basis),
                                        index_val = new_index,
                                        info = "random_search",
                                        tries = tries,
                                        loop = try)

    if (new_index > cur_index) {
      cat("New", new_index, "try", try, "\n")

      if (verbose) {
        record <<- record %>%
          dplyr::mutate(row = dplyr::row_number(),
                      info = ifelse(row == max(row), "new_basis", info)) %>%
          dplyr::select(-row)

        return(list(record = record, target = new_basis))
      }else{
        return(list(target = new_basis))
      }
    }
    else if (try > 900) {
      new_basis <- basis_nearby(current, alpha, method)
      cat("insert random step, index_val = ", index(new_basis), "\n")

      if (verbose) {
        record <<- record %>% dplyr::add_row(basis = list(new_basis),
                                             index_val = new_index,
                                             info = "random_step",
                                             tries = tries,
                                             loop = try)

        return(list(record = record, target = new_basis))
      }else{
        return(list(target = new_basis))
      }

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


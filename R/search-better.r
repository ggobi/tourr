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
  method = "linear", cur_index = NA) {
  if (is.na(cur_index)) cur_index <- index(current)

  cat("Old", cur_index, "\n")
  try <- 1

  while(try < max.tries) {
    new_basis <- basis_nearby(current, alpha, method)
    new_index <- index(new_basis)

    record <- record %>% dplyr::add_row(basis = list(new_basis),
                                          index_val = new_index,
                                          info = "random_search",
                                          tries = tries,
                                          loop = try)

    if (new_index > cur_index) {
      cat("New", new_index, "try", try, "\n")
      record <- record %>%
        dplyr::mutate(row = row_number(),
                      info = ifelse(row == max(row), "new_basis", info)) %>%
        dplyr::select(-row)

      target <- record %>% tail(1) %>% pull(basis)
      return(list(record = record,
                  target = target[[1]]))
    }
    try <- try + 1
  }

  NULL
}

#' Search for better projection, with stochastic component.
#' @keywords internal
search_better_random <- function(current, alpha = 0.5, index,
  max.tries = Inf, method = "linear", eps = 0.001, cur_index = NA
) {
  if (is.na(cur_index)) cur_index <- index(current)

  cat("Old", cur_index, "\n")
  try <- 1
  while(try < max.tries) {
    new_basis <- basis_nearby(current, alpha, method)
    new_index <- index(new_basis)

    record <- record %>% dplyr::add_row(basis = list(new_basis),
                                        index_val = new_index,
                                        info = "random_search",
                                        tries = tries,
                                        loop = try)

    if (new_index > cur_index) {
      cat("New", new_index, "try", try, "\n")

      record <- record %>%
        dplyr::mutate(row = row_number(),
                      info = ifelse(row == max(row), "new_basis", info)) %>%
        dplyr::select(-row)

      target <- record %>% tail(1) %>% pull(basis)
      return(list(record = record,
                  target = target[[1]]))
    }
    else if (abs(new_index-cur_index) < eps) {
      cat("insert random step since abs(new_index-cur_index) < eps")
      new_basis <- basis_random(nrow(current), ncol(current))

      record <- record %>% dplyr::add_row(basis = list(new_basis),
                                          index_val = new_index,
                                          info = "random_search",
                                          tries = tries,
                                          loop = try)

      target <- record %>% tail(1) %>% pull(basis)
      return(list(record = record,
                  target = target[[1]]))
    }
    try <- try + 1
  }

  NULL
}


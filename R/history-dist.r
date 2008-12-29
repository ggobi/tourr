#' Compute distance matrix from bases
#' 
#' @examples
#' 
history_dist <- function(history) {
  history <- as.array(history)
  
  n <- dim(history)[3]
  d <- matrix(NA, nrow = n, ncol = n)

  for(i in seq_len(n)) {
    for (j in seq_len(i - 1)) {
      d[i, j] <- proj_dist(history[, , i], history[, , j])
    }
  }
  as.dist(d)
}



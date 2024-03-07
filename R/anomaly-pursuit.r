#' Anomaly index.
#'
#' Calculates an index that looks for the best projection of
#' observations that are outside a pre-determined p-D ellipse.
#'
#' @export
anomaly_index <- function() {

  function(mat, ell2d) {

    mat_tab <- #mean(mahal_dist(mat, ell2d))
      mean(mahalanobis(mat, center=c(0,0), cov=ell2d))
  }
}


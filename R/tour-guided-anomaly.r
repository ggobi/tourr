#' A guided anomaly tour path.
#'
#' The guided anomaly tour is a variation of the guided tour that is
#' using an ellipse to determine anomalies on which to select target planes.
#'
#' Usually, you will not call this function directly, but will pass it to
#' a method that works with tour paths like \code{\link{animate_slice}},
#' \code{\link{save_history}} or \code{\link{render}}.
#'
#' @param index_f the section pursuit index function to optimise. The function
#'   needs to take two arguments, the projected data, indexes of anomalies.
#' @param d target dimensionality
#' @param alpha the initial size of the search window, in radians
#' @param cooling the amount the size of the search window should be adjusted
#'   by after each step
#' @param search_f the search strategy to use
#' @param max.tries the maximum number of unsuccessful attempts to find
#'   a better projection before giving up
#' @param max.i the maximum index value, stop search if a larger value is found
#' @param ellipse pxp variance-covariance matrix defining ellipse, default NULL.
#'        Useful for comparing data with some hypothesized null.
#' @param ellc This can be considered the equivalent of a critical value, used to
#'        scale the ellipse larger or smaller to capture more or fewer anomalies. Default 3.
#' @param ellmu This is the centre of the ellipse corresponding to the mean of the
#'        normal population. Default vector of 0's
#' @param ... arguments sent to the search_f
#' @seealso \code{\link{slice_index}} for an example of an index functions.
#' \code{\link{search_geodesic}}, \code{\link{search_better}},
#'   \code{\link{search_better_random}} for different search strategies
#' @importFrom stats mahalanobis qchisq
#' @export
#' @examples
#' animate_xy(flea[, 1:6], guided_anomaly_tour(anomaly_index(),
#'   ellipse=cov(flea[,1:6])), ellipse=cov(flea[,1:6]), axes="off")
guided_anomaly_tour <- function(index_f, d = 2, alpha = 0.5, cooling = 0.99,
                                max.tries = 25, max.i = Inf,
                                ellipse, ellc=NULL, ellmu=NULL,
                                search_f = search_geodesic, ...) {
  h <- NULL

  generator <- function(current, data, tries, ...) {
    if (is.null(current)) {
      return(basis_init(ncol(data), d))
    }

    if (is.null(h)) {
      half_range <- compute_half_range(NULL, data, FALSE)
    }

    index <- function(proj) {
      if (nrow(ellipse) == nrow(proj)) {

        if (is.null(ellc))
          ellc <<- qchisq(0.95, nrow(proj))
        else
          stopifnot(ellc > 0) # Needs to be positive
        if (is.null(ellmu))
          ellmu <<- rep(0, nrow(proj))
        else
          stopifnot(length(ellmu) == nrow(proj)) # Right dimension
        #message("Using ellc = ", format(ellc, digits = 2))

        # Check which observations are outside pD ellipse
        mdst <- mahalanobis(data,
                            center=ellmu,
                            cov=ellipse)
      #mdst <- mahal_dist(data, ellipse)
        anomalies <- which(mdst > ellc)
        stopifnot(length(anomalies) > 0)
        #cat(length(anomalies), "\n")

        # Project ellipse into 2D
        evc <- eigen(ellipse) #
        ellinv <- (evc$vectors) %*% as.matrix(diag(evc$values)) %*% t(evc$vectors)
        e2 <- t(proj) %*% ellipse %*% proj
        evc2 <- eigen(e2)
        ell2d <- as.matrix(evc2$vectors) %*% diag(sqrt(evc2$values*ellc)) %*% t(as.matrix(evc2$vectors))

        ell2dinv <- (evc2$vectors) %*% diag(evc2$values*ellc) %*% t(evc2$vectors)
        ellmu2d <- t(as.matrix(ellmu)) %*% proj
        #evc <- eigen(ellipse)
        #ellinv <- (evc$vectors) %*% diag(evc$values) %*% t(evc$vectors)
        #e2 <- t(proj) %*% ellinv %*% proj
        #evc2 <- eigen(e2)
        #ell2d <- (evc2$vectors) %*% diag(sqrt(evc2$values)) %*% t(evc2$vectors)
        #e3 <- eigen(ell2d)
        #ell2dinv <- (e3$vectors) %*% diag(e3$values) %*% t(e3$vectors)
        index_f(as.matrix(data[anomalies,]) %*% proj, e2, ellmu2d)
      }
    }

    cur_index <- index(current)

    if (cur_index > max.i) {
      cat("Found index ", cur_index, ", larger than selected maximum ", max.i, ". Stopping search.\n",
        sep = ""
      )
      cat("Final projection: \n")
      if (ncol(current) == 1) {
        for (i in 1:length(current)) {
          cat(sprintf("%.3f", current[i]), " ")
        }
        cat("\n")
      }
      else {
        for (i in 1:nrow(current)) {
          for (j in 1:ncol(current)) {
            cat(sprintf("%.3f", current[i, j]), " ")
          }
          cat("\n")
        }
      }
      return(NULL)
    }

    basis <- search_f(current, alpha, index, tries, max.tries, cur_index = cur_index, ...)
    alpha <<- alpha * cooling

    list(target = basis$target, index = index)
  }

  new_geodesic_path("guided", generator)
}

#' Section pursuit index.
#'
#' Calculates a section pursuit index that compares the distribution
#' inside and outside a slice.
#'
#' @param breaks_x binning on the first variable (x or radius).
#' @param breaks_y binning on the second variable (y or angle).
#' @param eps cutoff values to suppress summing up small differences.
#'   Vector with one entry for each bin, can be estimated
#'   using \code{\link{estimate_eps}}.
#' @param bintype select polar (default) or square binning.
#' @param power exponent q used in the index compuatation.
#' @param flip sign of the index computation, select +1 when searching
#'   for low densities and -1 when searching for high densities.
#' @param reweight if TRUE will reweight according to the expected distribution
#'   in a uniform hypersphere (default is FALSE).
#' @param p number of variables in the data (needed for accurate reweighting,
#'   default is 4).
#' @export
slice_index <- function(breaks_x, breaks_y, eps, bintype = "polar", power = 1, flip = 1,
                        reweight = FALSE, p = 4) {
  if (reweight) {
    if (bintype != "polar") {
      cat("Reweighting is only defined for polar binning and will be ignored.")
    }
    else {
      cat(paste0("Reweighting assuming p=", p))
    }
  }

  resc <- 1
  if (bintype == "polar") {
    resc <- 1 / (1 - (1 / 10)^(1 / power))^power
    cat(paste0("Rescaling raw index by a factor ", resc))
  }

  function(mat, dists, h) {

    # call binner, return zero if binning is not recognised
    mat_tab <- slice_binning(mat, dists, h, breaks_x, breaks_y, bintype = bintype)
    if (length(mat_tab) == 1 && mat_tab == 0) {
      return(0)
    }

    # no result if no points inside the slice
    if (length(mat_tab$n[mat_tab$inSlice == TRUE]) == 0) {
      return(0)
    }

    # splitting into in and out of slice
    mat_in <- mat_tab[mat_tab$inSlice == TRUE, ]
    mat_out <- mat_tab[mat_tab$inSlice == FALSE, ]

    if (bintype == "polar" && reweight) {
      # get bin-wise weights
      w_in <- weights_bincount_radial(mat_in, 2)
      w_out <- weights_bincount_radial(mat_out, p)
      # rescale and normalise n
      mat_in$n <- w_in * mat_in$n / sum(mat_in$n)
      mat_out$n <- w_out * mat_out$n / sum(mat_out$n)
    }
    else {
      # normalise n
      mat_in$n <- mat_in$n / sum(mat_in$n)
      mat_out$n <- mat_out$n / sum(mat_out$n)
    }

    # getting binwise density differences
    if (power != 1) {
      x <- flip * (mat_out$n^(1 / power) -
        mat_in$n^(1 / power))
      y <- flip * (mat_out$n -
        mat_in$n)
      x[y < eps] <- 0 # dropping bins based on y
    }
    else {
      x <- flip * (mat_out$n - mat_in$n)
      x[x < eps] <- 0
    }
    if (power != 1) {
      resc * sum(x^power)
    } else {
      resc * sum(x)
    }
  }
}

#' Estimate cutoff eps for section pursuit.
#'
#' @param N total number of points in the input data.
#' @param p number of dimensions of the input data.
#' @param res resolution, (slice radius)/(data radius)
#' @param K total number of bins
#' @param K_theta number of angular bins
#' @param r_breaks boundaries of the radial bins
#' @export
estimate_eps <- function(N, p, res, K, K_theta, r_breaks) {
  r_max <- max(r_breaks)
  ret <- c()
  for (i in 2:length(r_breaks)) {
    r1 <- r_breaks[i - 1]
    r2 <- r_breaks[i]
    delta_i <- (r_max / sqrt(r2^2 - r1^2)) * sqrt(2 * K_theta / N) *
      res^((2 - p) / 2) / sqrt(p - (p - 2) * res^2)
    eps <- delta_i / K
    ret <- c(ret, rep(eps, K_theta))
  }
  ret
}

#' Separately binning observations inside and outside the slice.
#'
#' @param mat projected data points
#' @param dists vector of all point distances from the projection plane.
#' @param h slice thickness.
#' @param breaks_x binning on the first variable (x or radius).
#' @param breaks_y binning on the second variable (y or angle).
#' @param bintype select polar (default) or square binning.
#' @keywords internal
slice_binning <- function(mat, dists, h, breaks_x, breaks_y, bintype = "square") {

  # centering
  mat <- apply(mat, 2, function(x) (x - mean(x)))

  if (bintype == "square") {
    xbin <- cut(mat[, 1], breaks_x)
    ybin <- cut(mat[, 2], breaks_y)
  }
  else if (bintype == "polar") {
    rad <- sqrt(mat[, 1]^2 + mat[, 2]^2)
    ang <- atan2(mat[, 2], mat[, 1])
    xbin <- cut(rad, breaks_x)
    ybin <- cut(ang, breaks_y)
  }
  else {
    cat(bintype, " is not a recognised bin type\n")
    return(0)
  }

  # track which points are inside the current slice
  inSlice <- factor(dists < h)

  # return binned data
  # to match order of radial rescaling vector we pass in ybin first
  ret <- as.data.frame(table(ybin, xbin, inSlice, useNA = "no"))
  colnames(ret) <- c("ybin", "xbin", "inSlice", "n")
  ret
}

#' Returns n equidistant bins between -pi and pi
#'
#' @param n number of bins
#' @export
angular_breaks <- function(n) {
  seq(-pi, pi, length.out = n + 1)
}

#' Returns n equidistant bins between a and b
#'
#' @param n number of bins
#' @param a lower bound
#' @param b upper bound
#' @export
linear_breaks <- function(n, a, b) {
  seq(a, b, length.out = n + 1)
}

#' Cumulative distribution in radius.
#'
#' Fraction of points within radius r given 2D projection of a
#' hypersphere with radius R in p dimensions.
#'
#' @keywords internal
cumulative_radial <- function(r, R, p) {
  1 - (1 - (r / R)^2)^(p / 2)
}


#' Inverse weights for rescaling counts in radial bins.
#'
#' @keywords internal
radial_bin_weight_inv <- function(r1, r2, R, p) {
  cumulative_radial(r2, R, p) - cumulative_radial(r1, R, p)
}

#' Computes weights for the rescaling of radial bin counts.
#'
#' @param histo input histogram (for bin definitions)
#' @param p number of variables
#'
#' @keywords internal
weights_bincount_radial <- function(histo, p) {
  # store weights in vector w
  w <- rep(0, nrow(histo))
  # extract maximum radius from all bins
  R <- max(as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", histo$xbin)))
  n <- length(unique(histo$xbin))
  for (i in 1:nrow(histo)) {
    r1 <- as.numeric(sub("\\((.+),.*", "\\1", histo$xbin[[i]]))
    r2 <- as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", histo$xbin[[i]]))
    w[i] <- radial_bin_weight_inv(r1, r2, R, p)
  }
  1 / (n * w)
}

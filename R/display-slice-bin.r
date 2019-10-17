#' Display tour path with a sliced scatterplot
#'
#' Animate a 2D tour path with a sliced scatterplot.
#'
#' @param axes position of the axes: center, bottomleft or off
#' @param center if TRUE, centers projected data to (0,0).  This pins the
#'  center of data cloud and make it easier to focus on the changing shape
#'  rather than position.
#' @param half_range half range to use when calculating limits of projected.
#'   If not set, defaults to maximum distance from origin to each row of data.
#' @param edges A two column integer matrix giving indices of ends of lines.
#' @param edges.col colour of edges to be plotted, Defaults to "black.
#' @param col color to be plotted.  Defaults to "black"
#' @param pch_slice marker for plotting points inside the slice.
#'   Defaults to 20.
#' @param pch_other marker for plotting points outside the slice.
#'   Defaults to 46.
#' @param eps volume of the slice. If not set, suggested value is caluclated and
#'   printed to the screen.
#' @param anchor A vector specifying the reference point to anchor the slice.
#'   If NULL (default) the slice will be anchored at the origin.
#' @param rescale if true, rescale all variables to range [0,1].
#' @param ...  other arguments passed on to \code{\link{animate}} and
#'   \code{\link{display_slice}}
#' @export
#' @examples
#' # Generate samples on a 3d and 5d hollow sphere using the geozoo package
#' sphere3 <- geozoo::sphere.hollow(3)$points
#' sphere5 <- geozoo::sphere.hollow(5)$points
#'
#' # Columns need to be named before launching the tour
#' colnames(sphere3) <- c("x1", "x2", "x3")
#' colnames(sphere5) <- c("x1", "x2", "x3", "x4", "x5")
#'
#' # Animate with the slice display using the default parameters
#' animate_slice(sphere3)
#' animate_slice(sphere5)
#'
#' # Animate with off-center anchoring
#' anchor3 <- rep(0.7, 3)
#' anchor5 <- rep(0.3, 5)
#' animate_slice(sphere3, anchor = anchor3)
#' # Animate with thicker slice to capture more points in each view
#' animate_slice(sphere5, anchor = anchor5, eps = 0.02)


display_slice_bin <- function(center = TRUE, axes = "center", half_range = NULL,
                             col = "black", pch_slice  = 20, pch_other = 46, eps = NULL,
                             anchor = NULL, edges = NULL, nbin = 30, edges.col = "black",...) {

  labels <- NULL
  h <- NULL
  init <- function(data) {
    half_range <<- compute_half_range(half_range, data, center)
    labels <<- abbreviate(colnames(data), 3)
    eps <<- compute_epsilon(eps, half_range, ncol(data))
    # Translate volume eps to cutoff h
    h <<- eps^(1/(ncol(data)-2))
    message("Using eps=", format(eps, digits = 2), ", corresponding to a cutoff h=", format(h, digits = 2))
  }

  if (!is.null(edges)) {
    if (!is.matrix(edges) && ncol(edges) == 2) {
      stop("Edges matrix needs two columns, from and to, only.")
    }
  }

  render_frame <- function() {
    par(pty = "s", mar = rep(0.1, 4))
    blank_plot(xlim = c(-1, 1), ylim = c(-1, 1))
  }
  render_transition <- function() {
    rect(-1, -1, 1, 1, col="#FFFFFFE6", border=NA)
  }

  render_data <- function(data, proj, geodesic) {

    # Render projected points
    x <- data %*% proj
    d <- anchored_orthogonal_distance(proj, data, anchor)
    x <- center(x)
    x <- x / half_range
    binvec <- seq(-1, 1, length.out = nbin+1) #defining the bins along one axis
    xbins <- cut(x[,1], binvec) #binning along x
    ybins <- cut(x[,2], binvec) #binning along y
    hbins <- as.data.frame(table(xbins, ybins)) #use table function to get 2D binning
    z <- matrix(hbins$Freq, nrow=nbin) #read off frequency and write in a matrix for image function
    par(mfrow=c(2,1))
    par(mar=c(0, 5, 5, 5)) # by default image will fill the full space, so setting margins
    image(binvec, binvec, # first two arguments repeat bin boundaries
          z, # filling based on matrix with histogram counts
          col = rev(gray.colors(20, start = 0, end = 1)), # generate color scale from white to black
          axes=FALSE, xlab = "", ylab = "") # turn off axes and labels
    par(mar=c(5, 5, 0, 5)) # by default image will fill the full space, so setting margins
    par(xaxs="i")
    draw_tour_axes(proj, labels, limits = 1, axes)

  }


  list(
    init = init,
    render_frame = render_frame,
    render_transition = render_transition,
    render_data = render_data,
    render_target = nul
  )
}

#' @rdname display_slice
#' @inheritParams animate
#' @export
animate_slice_bin <- function(data, tour_path = grand_tour(), rescale = TRUE, ...) {
  animate(data, tour_path, display_slice_bin(...), rescale = rescale)
}

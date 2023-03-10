#' Render frames of animation to disk
#'
#' @param data matrix, or data frame containing numeric columns
#' @param tour_path tour path generator
#' @param display the method used to render the projected data,
#'   e.g. \code{\link{display_xy}}, \code{\link{display_pcp}}
#' @param dev name of output device to use (e.g. \code{\link{png}},
#'   \code{\link{pdf}})
#' @param ... other options passed to output device
#' @param apf angle (in radians) per frame
#' @param frames number of frames in output
#' @param rescale if true, rescale all variables to range [0,1]
#' @param sphere if true, sphere all variables
#' @param start starting projection.  If \code{NULL}, uses path default.
#' @keywords hplot
#' @export
#' @examples
#' \donttest{
#' tmp_path <- tempdir()
#' render(flea[, 1:6], grand_tour(), display_xy(), "pdf",
#'   frames = 3,
#'   file.path(tmp_path, "test.pdf")
#' )
#' render(flea[, 1:6], grand_tour(), display_xy(), "png",
#'   frames = 3,
#'   file.path(tmp_path, "test-%03d.png")
#' )
#' }
render <- function(data, tour_path, display, dev, ..., apf = 1 / 10, frames = 50, rescale = TRUE, sphere = FALSE, start = NULL) {
  if (!is.matrix(data)) {
    message("Converting input data to the required matrix format.")
    data <- as.matrix(data)
  }
  if (rescale) data <- rescale(data)
  if (sphere) data <- sphere_data(data)

  dev <- match.fun(dev)
  dev(...)
  on.exit(dev.off())

  record <-
    tibble::tibble(
      basis = list(),
      index_val = numeric(),
      info = character(),
      method = character(),
      alpha = numeric(),
      tries = numeric(),
      loop = numeric()
    )

  tour <- new_tour(data, tour_path, start, ...)
  step <- tour(0, ...)

  display$init(data)

  i <- 0
  stop_next <- FALSE
  while (i < frames) {
    display$render_frame()
    display$render_data(data, step$proj, step$target)

    if (stop_next) {
      return(invisible())
    }
    i <- i + 1
    step <- tour(apf)
    if (step$step < 0) stop_next <- TRUE
  }
  invisible()
}


#' Render frames of animation to a gif file
#'
#' @param data matrix, or data frame containing numeric columns
#' @param tour_path tour path generator
#' @param display the method used to render the projected data,
#'   e.g. \code{\link{display_xy}}, \code{\link{display_pcp}}
#' @param gif_file Name of gif file (default = "animation.gif")
#' @param ... other options passed to \code{\link{png}}
#' @param apf angle (in radians) per frame
#' @param frames number of frames in output
#' @param rescale if true, rescale all variables to range [0,1]
#' @param sphere if true, sphere all variables
#' @param start starting projection.  If \code{NULL}, uses path default.
#' @param loop Logical for gifski to loop or not, default=TRUE
#'
#' @examples
#' \dontrun{
#' # gifski needs to be installed to render a gif
#' if (requireNamespace("gifski", quietly = TRUE)) {
#'   gif_file <- file.path(tempdir(), "test.gif")
#'   render_gif(flea[, 1:6], grand_tour(), display_xy(), gif_file)
#'   utils::browseURL(gif_file)
#'   unlink(gif_file)
#' }
#' }
#' @export
render_gif <- function(data, tour_path, display, gif_file = "animation.gif", ..., apf = 1 / 10, frames = 50, rescale = TRUE, sphere = FALSE, start = NULL, loop = TRUE) {
  if (!requireNamespace("gifski", quietly = TRUE)) {
    stop("To use this function please install the 'gifski' package",
      call. = FALSE
    )
  }

  # temp png files
  dir <- tempdir()
  png_path <- file.path(dir, "frame%03d.png")

  render(
    data = data,
    tour_path = tour_path,
    display = display,
    dev = "png",
    png_path,
    ...,
    apf = apf,
    frames = frames,
    rescale = rescale,
    sphere = sphere,
    start = start
  )

  png_files <- sprintf(png_path, 1:frames)
  on.exit(unlink(png_files))

  # Handle smaller number of frames than requested
  # as happens with the guided tour
  png_exist <- sapply(png_files, file.exists)
  if (!all(png_exist)){
    n_png <- sum(png_exist)
    warning(paste0("Note: only ", n_png, " frames generated, argument frames = ", frames, " is ignored."))
  }
  png_files <- png_files[png_exist]

  gifski::gifski(png_files, gif_file, delay = apf, progress = TRUE, loop = loop, ...)
}

#' Render a frame
#'
#' This function takes a projection matrix as produced by
#' save_history(), and draws it on the projected data
#' like a biplot. This will product the data objects needed
#' in order for the user to plot with base or ggplot2.
#'
#' @param data matrix, or data frame containing numeric columns,
#'   should be standardised to have mean 0, sd 1
#' @param prj projection matrix
#' @param location of the axes display on the plot: center (default), bottomleft, off.
#' @param limits value setting the lower and upper limits of
#'   projected data, default 1
#' @param position position of the axes: center (default),
#'   bottomleft or off
#'
#' @return list containing projected data, circle and segments for axes
#' @export
#' @examples
#' data(flea)
#' flea_std <- apply(flea[,1:6], 2, function(x) (x-mean(x))/sd(x))
#' prj <- basis_random(ncol(flea[,1:6]), 2)
#' p <- render_proj(flea_std, prj)
#' if (require("ggplot2")) {
#' ggplot() +
#'   geom_path(data=p$circle, aes(x=c1, y=c2)) +
#'   geom_segment(data=p$axes, aes(x=x1, y=y1, xend=x2, yend=y2)) +
#'   geom_text(data=p$axes, aes(x=x2, y=y2, label=rownames(p$axes))) +
#'   geom_point(data=p$data_prj, aes(x=P1, y=P2)) +
#'   xlim(c(-1, 1)) + ylim(c(-1,1)) +
#'   theme_bw() +
#'   theme(axis.text=element_blank(),
#'         axis.title=element_blank(),
#'         axis.ticks=element_blank(),
#'         panel.grid=element_blank())
#' }
render_proj <- function(data, prj, labels=NULL, limits=1, position="center"){
  # Check dimensions ok
  try(if (ncol(data) != nrow(prj))
           stop("Number of columns of data don't match number of rows of prj"))
  try(if(ncol(prj) != 2)
           stop("Number of columns of prj needs to be 2"))

  # Project data and scale into unit box
  data_prj <- as.matrix(data) %*% as.matrix(prj)
  rng <- range(data_prj)
  data_prj <- data_prj/max(abs(rng))
  colnames(data_prj) <- c("P1", "P2")
  data_prj <- data.frame(data_prj)

  # Make labels if missing
  if (is.null(labels))
    labels <- colnames(data)

  # Axis scale
  if (position == "center") {
    axis_scale <- 2 * limits / 3
    axis_pos <- 0
  } else if (position == "bottomleft") {
    axis_scale <- limits / 6
    axis_pos <- -2 / 3 * limits
  }
  adj <- function(x) axis_pos + x * axis_scale

  # Compute segments
  axes <- data.frame(x1=adj(0), y1=adj(0),
                     x2=adj(prj[, 1]), y2=adj(prj[, 2]))
  rownames(axes) <- colnames(data)

  # Compute circle
  theta <- seq(0, 2 * pi, length = 50)
  circle <- data.frame(c1 = adj(cos(theta)), c2=adj(sin(theta)))

  return(list(data_prj=data_prj, axes=axes, circle=circle))
}

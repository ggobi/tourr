#' Rescale a matrix or data frame
#'
#' Standardise each column to have range [0, 1]
#'
#' @param df data frame or matrix
#' @keywords manip
#' @export
rescale <- function(df) {
  apply(df, 2, function(x) (x - min(x)) / diff(range(x)))
}

#' Center a numeric vector by subtracting off its mean.
#'
#' @param x numeric vector
#' @export
center <- function(x) {
  scale(x, center = TRUE, scale = FALSE)
}

#' Sphere a matrix (or data frame) by transforming variables to
#' principal components.
#'
#' Sphering is often useful in conjunction with the guided tour, as it
#' removes simpler patterns that may conceal more interesting findings.
#'
#' @param df   data frame or matrix
#' @keywords manip
#' @export
sphere_data <- function(df) {
  apply(stats::predict(stats::prcomp(df)), 2, scale)
}


#' A null function
#'
#' This function does nothing, and is a useful default callback function
#'
#' @param ... all arguments to \code{...} are ignore
#' @name xnul
#' @keywords internal
nul <- function(...) {}


#' Set up a blank plot to display data projections
#' @keywords internal
blank_plot <- function(...) {
  old <- par(mar = c(0, 0, 0, 0))
  # on.exit(par(old))
  plot(
    x = NA, y = NA, xlab = "", ylab = "",
    axes = FALSE, frame = TRUE, xaxs = "i", yaxs = "i",
    ...
  )
}


#' Find the platform
#' Find the platform being used by the user
#'
#' @keywords internal
#' @export
find_platform <- function() {
  os <- R.Version()$os
  gui <- .Platform$GUI

  if (length(grep("linux", os)) == 1) {
    osType <- "lin"
  } else if (length(grep("darwin", os)) == 1) {
    osType <- "mac"
  } else {
    osType <- "win"
  }

  if (gui == "RStudio") {
    type <- "rstudio"
  } else if (osType %in% c("lin", "mac") && gui != "X11") {
    type <- "gui"
  } else if (osType == "win" && gui == "Rgui") {
    type <- "gui"
  } else {
    type <- "cli"
  }

  list(os = osType, iface = type)
}

#' Prints information on how to stop the output
#'
#' This function prints the corresponding information on how to stop the plotting.
#'
#' @keywords internal
to_stop <- function() {
  plat <- find_platform()
  if (plat$os == "win" || plat$iface == "rstudio") {
    key <- "Esc"
  } else if (plat$os == "mac" && plat$iface == "gui") {
    key <- "Esc"
  } else {
    key <- "Ctrl + C"
  }
  message("Press ", key, " to stop tour running")
}

#' Test if all entries are colors
#'
#' @param x vector
#' @export
areColors <- function(x) {
  all(
    sapply(x, function(X) {
      tryCatch(is.matrix(grDevices::col2rgb(X)),
        error = function(e) FALSE
      )
    })
  )
}

#' Map vector of factors to color
#'
#' @param x vector
#' @param palette name of color palette for point colour, used by \code{\link{hcl.colors}}, default "Zissou 1"
#' @export
mapColors <- function(x, palette) {
  n <- length(unique(x))
  # Handle manual colour setting
  if (length(palette) > 1) {
    stopifnot(length(palette) == n)
    pal <- palette
  }
  else {
    pal <- grDevices::hcl.colors(n, palette=palette)
  }
  pal[as.numeric(as.factor(x))]
}

#' Map vector of factors to pch
#'
#' @param x vector
#' @param shapeset vector of integers indicating point shapes
#' @export
mapShapes <- function(x, shapeset) {
  n <- length(unique(x))
  stopifnot(length(shapeset) >= n)
  shapes <- shapeset
  shapes[as.numeric(x)]
}

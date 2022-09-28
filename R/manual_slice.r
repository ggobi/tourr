#' Manually slice along a variable axis.
#'
#' The manual slice tour takes the current projection, with display_slice,
#' and changes the slice center.
#'
#' @param data numeric matrix, with n rows and p columns
#' @param proj projection from which slices are constructed
#' @param var variable axis to run the center along: 1, ..., p
#' @param nsteps number of changes in center to make
#' @param rescale if true, rescale all variables to range [0,1]?
#' @param col color to be plotted.  Defaults to "black"
#' @param sphere if true, sphere all variables
#' @param ... other options passed to output device
#' @export
#' @examples
#' sphere5 <- data.frame(geozoo::sphere.hollow(5)$points)
#' proj <- matrix(c(1, 0, 0, 1, 0, 0, 0, 0, 0, 0), ncol=2, byrow=TRUE)
#' manual_slice(sphere5, proj, var=3, rescale=TRUE)
manual_slice <- function(data, proj, var=1, nsteps=20, v_rel = 0.01, rescale = FALSE, sphere = FALSE, col = "black", ...) {
  # Standard checks, and scaling
  if (!is.matrix(data)) {
    message("Converting input data to the required matrix format.")
    data <- as.matrix(data)
  }
  if (rescale) data <- rescale(data)
  if (sphere) data <- sphere_data(data)
  if (!areColors(col)) col <- mapColors(col)

  # Going to use nsteps twice, to go out to edge, come back to 0
  # go out to negative edge and back in to stop at 0
  rng_var <- range(data[,var])
  anchor <- matrix(colMeans(data), ncol = length(proj[, 1]))
  anchor_steps <- c(anchor[,var] + 0.8*(rng_var[2]-anchor[,var])*((0:nsteps)/nsteps),
                    anchor[,var] + 0.8*(rng_var[2]-anchor[,var])*((nsteps:0)/nsteps),
                    anchor[,var] - 0.8*(anchor[,var]-rng_var[1])*((0:nsteps)/nsteps),
                    anchor[,var] - 0.8*(anchor[,var]-rng_var[1])*((nsteps:0)/nsteps))

  # Initialise tour
  tour <- new_tour(data, grand_tour(), proj, ...)

  # Always doing slices
  display <- display_slice(axes="bottomleft", v_rel = v_rel)

  # Initialise display
  display$init(data)
  display$render_frame()
  display$render_data(data, proj, proj, with_anchor=anchor)

  plat <- find_platform()

  for (j in anchor_steps) {
    anchor[,var] <- j
    dev.hold()
    on.exit(dev.flush())
    if (plat$os == "win" || plat$iface == "rstudio") {
      display$render_frame()
    } else {
      display$render_transition()
    }
    display$render_data(data, proj, proj, with_anchor = anchor)
    dev.flush()
    Sys.sleep(1 / 25)
  }
  invisible()
}

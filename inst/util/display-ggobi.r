#' Display tour path with ggobi
#'
#' Use ggobi to display a 2D tour.
#'
#' @param ...  other arguments passed on to \code{\link{animate}}
#' @export
#' @examples
#' \dontrun{
#' animate_ggobi(flea[, 1:6])
#' # Spelling things out explicitly
#' animate(flea[, 1:6], grand_tour(), display_ggobi())
#' }
display_ggobi <- function() {
  if(!require("rggobi", quietly = TRUE)) {
    stop("rggobi required for ggobi based tour")
  }
  
  gd <- NULL  
  init <- function(data) {
    message("Pause the tour in GGobi to allow R control to begin")
    g <- ggobi(data)
    gd <<- displays(g)[[1]]
    pmode(gd) <- "2D Tour"
  }
  
  render_data <- function(data, proj, geodesic) {
    ggobi_display_set_tour_projection(gd, proj)
  }
  
  list(
    init = init,
    render_frame = nul,
    render_transition = nul,
    render_data = render_data,
    render_target = nul
  )
}

#' @rdname display_ggobi
#' @inheritParams animate
#' @export
animate_ggobi <- function(data, tour_path = grand_tour(), ...) {
  animate(data, tour_path, display_ggobi(...), ...)
}



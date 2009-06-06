#' @param render_target function called whenever new target projection is 
#'   generated
#' @param render_data function called after every new projection is generated
#'   to render the data.  The function has three arguments: the data, the
#'   the projection matrix and the geodesic path 
#'  (see \code{\link{geodesic_path}}) for more details.
#' @param render_transition function called before rendering data.  This is
#'   typically used to draw a transparent rectangle over the previous data to
#'   preserve some continuity.  It is not used when saving to disk.

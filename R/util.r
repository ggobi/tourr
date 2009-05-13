#' Rescale a matrix or data frame
#'
#' Standardise each column to have range [0, 1]
#' 
#' @param df data frame or matrix
#' @keywords manip
rescale <- function(df) {
  apply(df, 2, function(x) (x - min(x)) / diff(range(x)))
}

#' Sphere a matrix (or data frame)
#'
#' Sphering is often useful in conjunction with the guided tour, as it 
#' removes simpler patterns that may conceal more interesting findings.
#'
#' @param df   data frame or matrix
#' @keywords manip
sphere <- function(df) {
  apply(predict(princomp(df)), 2, scale)
}


#' A null function
#'
#' This function does nothing, and is a useful default callback function
#' 
#' @param ... all arguments to \code{...} are ignore
#' @keywords internal
nul <- function(...) {}


#' Set up a blank plot to display data projections
#' @keywords internal
blank_plot <- function(...) {
  plot(
    x = NA, y = NA, xlab = "", ylab = "",
    axes = FALSE, frame = TRUE, xaxs = "i", yaxs = "i",
    ...
  )  
}


#' Find the Platform
#' Find the platform being used by the user
#' keywords internal
find_platform <- function()
{
	os <- R.Version()$os
	osType <- "PC"

	if(length(strsplit(os,"linux")[[1]]) > 1)
		osType <- "Linux"
	if(length(strsplit(os,"darwin")[[1]]) > 1)
		osType <- "Mac"

	type <- "Terminal"
	
	if(osType %in% c("Linux", "Mac"))
	{
		if(.Platform$GUI != "X11")
			type <- "GUI"
	}else{
		if(.Platform$GUI == "Rgui")
			type <- "GUI"
	}

	list(os = osType, gui = type)

}


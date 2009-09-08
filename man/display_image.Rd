\name{display_image}
\title{Image tour path animation.}
\usage{display_image(data, xs, ys, ...)}
\description{Image tour path animation.}
\details{Animate a 1d tour path with an image plot.  This animation requires a 
different input data structure, a 3d array.  The first two dimensions are
locations on a grid, and the 3rd dimension gives the observations to be 
mixed with the tour.}
\seealso{\code{\link{animate}} for options that apply to all animations}
\keyword{hplot}
\alias{display_image}
\alias{animate_image}
\arguments{\item{data}{matrix, or data frame containing numeric columns}
\item{tour_path}{tour path generator, defaults to the grand tour}
\item{...}{other arguments passed on to \code{\link{animate}}}}
\examples{str(ozone)
animate_image(ozone)}

\name{display_stereo}
\title{Anaglpyh tour path animation.}
\usage{display_stereo(data, blue, red, ...)
animate_stereo(data, tour_path = grand_tour(3), blue = rgb(0, 0.91, 0.89), red = rgb(0.98, 0.052, 0), ...)}

\description{
  Anaglpyh tour path animation.
}

\details{
  Uses red-blue anaglyphs to display a 3d tour path.  You'll need some
  red- blue glasses to get much out of this displays!
}
\keyword{hplot}
\alias{display_stereo}
\alias{animate_stereo}
\arguments{
  \item{data}{matrix, or data frame containing numeric columns}
  \item{tour_path}{tour path, defaults to a 3d grand tour}
  \item{blue}{blue colour (for right eye)}
  \item{red}{red colour (for left eye)}
  \item{...}{other arguments passed on to \code{\link{animate}}}
}
\examples{animate_stereo(flea[, 1:6])}

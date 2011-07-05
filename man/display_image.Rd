\name{display_image}
\alias{animate_image}
\alias{display_image}
\title{Image tour path animation.}
\usage{
  display_image(xs, ys, ...)
}
\arguments{
  \item{xs}{x limit that is used in making the size of the
  plot}

  \item{ys}{y limit that is used in making the size of the
  plot}

  \item{...}{other arguments passed on to
  \code{\link{animate}}}
}
\description{
  Image tour path animation.
}
\details{
  Animate a 1d tour path with an image plot.  This
  animation requires a different input data structure, a 3d
  array.  The first two dimensions are locations on a grid,
  and the 3rd dimension gives the observations to be mixed
  with the tour.
}
\examples{
str(ozone)
animate_image(ozone)
}
\seealso{
  \code{\link{animate}} for options that apply to all
  animations
}
\keyword{hplot}


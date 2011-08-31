\name{display_scatmat}
\alias{animate_scatmat}
\alias{display_scatmat}
\title{Scatterplot matrix tour path animation.}
\usage{
  display_scatmat(...)
}
\arguments{
  \item{...}{other arguments passed on to
  \code{\link{animate}}}
}
\description{
  Animate a nD tour path with a scatterplot matrix.
}
\details{
  The lines show the observations, and the points, the
  values of the projection matrix.
}
\examples{
animate_scatmat(flea[, 1:6], grand_tour(2))
animate_scatmat(flea[, 1:6], grand_tour(6))
}
\seealso{
  \code{\link{animate}} for options that apply to all
  animations
}
\keyword{hplot}


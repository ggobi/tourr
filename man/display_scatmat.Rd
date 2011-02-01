\name{display_scatmat}
\title{Scatterplot matrix tour path animation.}
\usage{display_scatmat(...)}

\description{
  Scatterplot matrix tour path animation.
}

\details{
  Animate a nD tour path with a scatterplot matrix.

  The lines show the observations, and the points, the
  values of the projection matrix.
}
\seealso{\code{\link{animate}} for options that apply to all animations}
\keyword{hplot}
\alias{display_scatmat}
\alias{animate_scatmat}
\arguments{
  \item{...}{other arguments passed on to \code{\link{animate}}}
}
\examples{animate_scatmat(flea[, 1:6], grand_tour(2))
animate_scatmat(flea[, 1:6], grand_tour(6))}

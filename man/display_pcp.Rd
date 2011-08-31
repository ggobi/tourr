\name{display_pcp}
\alias{animate_pcp}
\alias{display_pcp}
\title{Parallel coordinates tour path animation.}
\usage{
  display_pcp(...)
}
\arguments{
  \item{...}{other arguments passed on to
  \code{\link{animate}}}
}
\description{
  Animate a nD tour path with a parallel coordinates plot.
}
\details{
  The lines show the observations, and the points, the
  values of the projection matrix.
}
\examples{
animate_pcp(flea[, 1:6], grand_tour(3))
animate_pcp(flea[, 1:6], grand_tour(5))
}
\seealso{
  \code{\link{animate}} for options that apply to all
  animations
}
\keyword{hplot}


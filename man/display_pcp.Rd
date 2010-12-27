\name{display_pcp}
\title{Parallel coordinates tour path animation.}
\usage{display_pcp(...)}

\description{
  Parallel coordinates tour path animation.
}

\details{
  Animate a nD tour path with a parallel coordinates plot.

  The lines show the observations, and the points, the
  values of the projection matrix.
}
\seealso{\code{\link{animate}} for options that apply to all animations}
\keyword{hplot}
\alias{display_pcp}
\alias{animate_pcp}
\arguments{
  \item{...}{other arguments passed on to \code{\link{animate}}}
}
\examples{animate_pcp(flea[, 1:6], grand_tour(3))
animate_pcp(flea[, 1:6], grand_tour(5))}

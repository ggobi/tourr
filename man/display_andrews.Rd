\name{display_andrews}
\title{Andrews' curves tour path animation.}
\usage{display_andrews(...)}

\description{
  Andrews' curves tour path animation.
}

\details{
  Animate a nD tour path with Andrews' curves.  For more
  details about Andrew's curves, see \code{\link{andrews}}
}
\seealso{\code{\link{animate}} for options that apply to all animations}
\keyword{hplot}
\alias{animate_andrews}
\alias{display_andrews}
\arguments{
  \item{...}{other arguments passed on to \code{\link{animate}}}
}
\examples{animate_andrews(flea[, 1:6])
animate_andrews(flea[, 1:6], grand_tour(d = 3))
animate_andrews(flea[, 1:6], grand_tour(d = 6))

# It's easy to experiment with different tour paths:
animate_andrews(flea[, 1:6], guided_tour(cm))}

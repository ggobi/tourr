\name{find_path_peak}
\alias{find_path_peak}
\title{Find the most interesting projection along a geodesic.}
\usage{find_path_peak(old, new, index, max_dist=pi/4)}

\description{
  Find the most interesting projection along a geodesic.
}

\details{
  Use \code{\link{optimize}} to find the most interesting
  projection amongst all projections on a geodesic.  This
  method assumes that the function is continuous with a
  single maximum, but seems to do ok even if there are
  multiple maxima.
}
\keyword{optimize}
\keyword{internal}
\arguments{
  \item{old}{currention project}
  \item{new}{projection that gives direction to travel in}
  \item{index}{interestingness index function}
  \item{max_dist}{maximum distance to travel along in radians}
}

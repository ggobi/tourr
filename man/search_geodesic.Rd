\name{search_geodesic}
\alias{search_geodesic}
\title{Search for most interesting projection along random geodesics.}
\usage{
  search_geodesic(current, alpha = 1, index, max.tries = 5,
  n = 5)
}
\arguments{
  \item{current}{starting projection}

  \item{alpha}{maximum distance to travel (currently
  ignored)}

  \item{index}{interestingness index function}

  \item{max.tries}{maximum number of failed attempts before
  giving up}

  \item{n}{number of random steps to take to find best
  direction}
}
\description{
  Search for most interesting projection along random
  geodesics.
}
\details{
  This is a novel method for finding more interesting
  projections for the guided tour.  It works by first
  taking a small step in \code{n} random directions, and
  then picking the direction that looks most promising
  (based on the height of the index function).  Once the
  best direction is found, it performs a linear search
  along the geodesic in that direction, traveling up to
  half way around the sphere.

  You should not to have call this function directly, but
  should supply it to the \code{\link{guided_tour}} as a
  search strategy.
}
\keyword{optimize}


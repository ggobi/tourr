\name{local_tour}
\alias{local_tour}
\title{The local tour.}
\usage{local_tour(start, angle=pi/4, ...)}

\description{
  The local tour.
}

\details{
  The local tour alternates between the starting position and a nearby
  random projection.

  Usually, you will not call this function directly, but will pass it
  to a method that works with tour paths like
  \code{\link{save_history}}, or \code{\link{animate}}
}
\arguments{
  \item{start}{initial projection matrix}
  \item{angle}{distance in radians to stay within}
  \item{...}{Not Used}
}
\examples{animate_xy(flea[, 1:3], local_tour(basis_init(3, 2)))
animate_xy(flea[, 1:3], local_tour(basis_init(3, 2), 0.2))
animate_xy(flea[, 1:3], local_tour(basis_random(3, 2), 0.2))}

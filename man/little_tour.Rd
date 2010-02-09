\name{little_tour}
\alias{little_tour}
\title{A little tour path.}
\usage{little_tour(d=2)}

\description{
  A little tour path.
}

\details{
  The little tour is a planned tour that travels between all axis
  parallel projections. (John McDonald named this type of tour.)

  Usually, you will not call this function directly, but will pass it
  to a method that works with tour paths like \code{\link{animate}},
  \code{\link{save_history}} or \code{\link{render}}.
}
\arguments{
  \item{d}{target dimensionality}
}
\examples{animate_xy(flea[, 1:6], little_tour())
animate_pcp(flea[, 1:6], little_tour(3))
animate_scatmat(flea[, 1:6], little_tour(3))
animate_pcp(flea[, 1:6], little_tour(4))}

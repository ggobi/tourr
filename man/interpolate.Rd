\name{interpolate}
\alias{interpolate}
\title{Interpolate geodesically between bases.}
\usage{
  interpolate(basis_set, angle = 0.05)
}
\arguments{
  \item{basis_set}{input basis set}

  \item{angle}{target distance (in radians) between bases}
}
\description{
  This function takes a set of bases and produces a tour by
  geodesically interpolating between each basis
}
\examples{
t1 <- save_history(flea[, 1:6], grand_tour(1), max = 10)
dim(t1)
dim(interpolate(t1, 0.01))
dim(interpolate(t1, 0.05))
dim(interpolate(t1, 0.1))
}
\keyword{hplot}


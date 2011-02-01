\name{interpolate}
\alias{interpolate}
\title{Interpolate geodesically between bases.}
\usage{interpolate(basis_set, angle=0.05)}

\description{
  Interpolate geodesically between bases.
}

\details{
  This function takes a set of bases and produces a tour by
  geodesically interpolating between each basis
}
\keyword{hplot}
\arguments{
  \item{basis_set}{input basis set}
  \item{angle}{target distance (in radians) between bases}
}
\examples{t1 <- save_history(flea[, 1:6], grand_tour(1), max = 10)
dim(t1)
dim(interpolate(t1, 0.01))
dim(interpolate(t1, 0.05))
dim(interpolate(t1, 0.1))}

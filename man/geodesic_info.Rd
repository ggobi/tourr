\name{geodesic_info}
\alias{geodesic_info}
\title{Calculate information required to interpolate along a geodesic path between...}
\usage{geodesic_info(Fa, Fz, epsilon=1e-06)}

\description{
  Calculate information required to interpolate along a
  geodesic path between two frames.
}

\details{
  The methdology is outlined in
  \url{http:www-stat.wharton.upenn.edu/~buja/PAPERS/paper-dyn-proj-algs.pdf}
  and
  \url{http:www-stat.wharton.upenn.edu/~buja/PAPERS/paper-dyn-proj-math.pdf},
  and the code follows the notation outlined in those
  papers:

  \itemize{ \item p = dimension of data \item d = target
  dimension \item F = frame, an orthonormal p x d matrix
  \item Fa = starting frame, Fz = target frame \item Fa'Fz
  = Va lamda Vz' (svd) \item Ga = Fa Va, Gz = Fz Vz \item
  tau = principle angles }
}
\keyword{internal}
\arguments{
  \item{Fa}{starting frame, will be orthonormalised if necessary}
  \item{Fz}{target frame, will be orthonormalised if necessary}
  \item{epsilon}{epsilon used to determine if an angle is effectively equal
to 0}
}

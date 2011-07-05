\name{freeze}
\alias{freeze}
\alias{thaw}
\title{Freeze and thaw matrices...}
\usage{
  freeze(input, frozen)
}
\description{
  Freeze and thaw matrices
}
\details{
  Some terminology: * frozen variables: the variables that
  have fixed values * warm variables: the remaining
  variables that vary freely

  A frozen matrix specifies which variables to fix in a
  projection matrix. Warm variables should be missing
  (\code{NA}) while frozen variables should be set to their
  fixed values.
}
\examples{
frozen <- matrix(NA, nrow = 4, ncol = 2)
frozen[3, ] <- .5

input <- basis_random(4, 2)
freeze(input, frozen)
thaw(input, frozen)
freeze(basis_random(4, 2), frozen)
}
\keyword{internal}


\name{search_frozen_geodesic}
\alias{search_frozen_geodesic}
\title{Search for most interesting projection along frozen geodesics.}
\usage{search_frozen_geodesic(current, index, max.tries=5, n=5,
    frozen)}

\description{
  Search for most interesting projection along frozen
  geodesics.
}

\details{
  These three functions perform a corresponding role to
  \code{\link{search_geodesic}},
  \code{\link{find_best_dir}} and
  \code{\link{find_path_peak}} but for the frozen tour.
  They work by zero'ing out the frozen variables and
  travelling in that restricted subspace.
}
\section{TODO}{eliminate these functions}
\keyword{internal}

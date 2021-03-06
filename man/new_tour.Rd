% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tour.r
\name{new_tour}
\alias{new_tour}
\title{Create a new tour.}
\usage{
new_tour(data, tour_path, start = NULL, ...)
}
\arguments{
\item{data}{the data matrix to be projected}

\item{tour_path}{basis generator, a function that generates a new basis,
called with the previous projection and the data set.  For more
complicated tour paths, this will need to be a closure with local
variables.  Should return NULL if the tour should terminate}

\item{start}{starting projection, if omitted will use default projection
from generator}
}
\value{
a function with single argument, step_size.  This function returns
 a list containing the new projection, the current target and the number
 of steps taken towards the target.
}
\description{
The tour function provides the common machinery behind all tour methods:
interpolating from basis to basis, and generating new bases when necessary.
You should not have to call this function.
}
\details{
If you need to define your own \code{new_tour()} inside a function, please add the following in the function:

\code{record <- dplyr::tibble(basis = list(),index_val = numeric(), info = character(), method = character(), alpha = numeric(), tries = numeric(), loop = numeric()}

The line on itself doesn't do anything other than initialising an empty \code{record} object, as what is done in \code{animate()},
but would ensure the \code{record} object can always be found in the optimisation recording.

If you are intended to call \code{new_tour()} from the global environment, try \code{save_history()} and then animate with a \code{planned_tour()}. See \code{\link[tourr]{save_history}} for examples on this.
}
\seealso{
\code{\link{save_history}}, \code{\link{render}} and
  \code{\link{animate}} for examples of functions that use this function
  to run dynamic tours.
}
\keyword{dynamic}
\keyword{hplot}
\keyword{internal}

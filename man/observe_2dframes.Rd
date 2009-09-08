\name{observe_2dframes}
\alias{observe_2dframes}
\title{Di Function...}
\usage{observe_2dframes(basis_set, ...)}
\description{Di Function}
\keyword{internal}
\examples{t1 <- save_history(flea[, 1:6], nbases = 3)
t1 <- save_history(flea[, 1:6], nbases = 100, interpolate=T)
t1 <- save_history(flea[, c(1,2,4,5)], nbases = 2)
t1 <- save_history(flea[, c(1,2,4)], nbases = 2)
t1 <- save_history(flea[, c(1,2,4)], nbases = 500)
observe_2dframes(interpolate(t1))}

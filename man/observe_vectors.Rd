\name{observe_vectors}
\alias{observe_vectors}
\title{Di Function...}
\usage{observe_vectors(basis_set, index_f, nbases=2, ...)}
\description{Di Function}
\keyword{internal}
\examples{t1 <- save_history(flea[, 1:3], nbases = 50, d = 1)
t1 <- save_history(flea[, 1:6], nbases = 3, d = 1, interpolate = T)
observe_vectors(interpolate(t1), nbases = 3)
t2 <- save_history(flea[,c(1,2,4)], tour_f = guided_tour, index_f = holes, d=1, sphere=T, basis_f = basis_geodesic_search)
observe_vectors(interpolate(t2), holes)
t3 <- save_history(flea[,1:6], tour_f = guided_tour, index_f = holes, d=1, sphere=T, basis_f = basis_geodesic_search)
observe_vectors(interpolate(t3), holes)}

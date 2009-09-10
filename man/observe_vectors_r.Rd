\name{observe_vectors_r}
\alias{observe_vectors_r}
\title{Di Function...}
\usage{observe_vectors_r(basis_set, index_f, plt_data=TRUE, plt_proj=FALSE,
    ...)}
\description{Di Function}
\keyword{internal}
\examples{#testdata <- matrix(rnorm(100*2), ncol=2)
#testdata[1:50,1] <- testdata[1:50,1] + 10
#testdata <- sphere(testdata)
#t1 <- save_history(testdata, nbases=3, d=1, rescale=FALSE, sphere=FALSE)
#observe_vectors_r(t1)
#observe_vectors_r(t1, plt_proj=TRUE)
#t1 <- save_history(testdata, tour_f = guided_tour, index_f = holes, nbases=5, d=1, rescale=FALSE, sphere=FALSE, max.tries = 100, cooling = 0.95)
#observe_vectors_r(t1, holes)}

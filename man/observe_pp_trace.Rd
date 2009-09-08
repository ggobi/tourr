\name{observe_pp_trace}
\alias{observe_pp_trace}
\title{Di Function...}
\usage{observe_pp_trace(basis_set, index_f=holes, nbases=2, ...)}
\description{Di Function}
\keyword{internal}
\examples{t2 <- save_history(testdata, tour_f = guided_tour, index_f = holes, nbases=5, d=1, rescale=F, sphere=F, max.tries = 100, cooling = 0.95)
library(rggobi)
gd<-observe_pp_trace(t2, nbases=10000)
gd<-observe_pp_trace(interpolate(t2), nbases=10000)
d<-displays(gd)[[1]]
pmode(d) <- "1D Tour"
ggsgnl <- gSignalConnect(gd, "identify-point", linker)
#Open a scatterplot display of the trace
gSignalHandlerDisconnect(gd, ggsgnl)
flea_s <- sphere(flea[,c(1,2,4)])
t2 <- save_history(flea_s, tour_f = guided_tour, index_f = holes, nbases=10, d=1, rescale=F, sphere=F, max.tries = 100, cooling = 0.99)
gd<-observe_pp_trace(interpolate(t2), nbases=10000)
d<-displays(gd)[[1]]
pmode(d) <- "1D Tour"
t3 <- interpolate(t2)
ggsgnl <- gSignalConnect(gd, "identify-point", linker2)
gSignalHandlerDisconnect(gd, ggsgnl)}

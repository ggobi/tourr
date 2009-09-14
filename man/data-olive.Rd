\name{Ozone measurements}
\docType{data}
\alias{ozone}
\title{Monthly ozone measurements over Central America}
\description{
 
572 olive oil samples from nine different regions of Italy; for each sample the normalized concentrations of eight fatty acids are given. We thank Prof. Michele Forina, University of Genova, Italy for making this dataset available.

}
\usage{data(ozone)}
\format{A 572 x 10 numeric array}
\references{\url{http://www2.chemie.uni-erlangen.de/publications/ANN-book/datasets/oliveoil/index.html}}
\keyword{datasets}
\examples{
value <- ozone[1, 1, ]
time <- 1:72
month.abbr <- c("Jan", "Feb", "Mar", "Apr", "May",
 "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
month <- factor(rep(month.abbr, length = 72), levels = month.abbr)
year <- rep(1:6, each = 12)
deseasf <- function(value) lm(value ~ month - 1)
 
models <- alply(ozone, 1:2, deseasf)
coefs <- laply(models, coef)
dimnames(coefs)[[3]] <- month.abbr
names(dimnames(coefs))[3] <- "month"
 
deseas <- laply(models, resid)
dimnames(deseas)[[3]] <- 1:72
names(dimnames(deseas))[3] <- "time"
 
dim(coefs)
dim(deseas)
}
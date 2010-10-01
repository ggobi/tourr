\name{range_dist}
\alias{range_dist}
\title{Calculate distribution of ranges of projected data...}
\usage{range_dist(data, n=1000)}

\description{
  Calculate distribution of ranges of projected data
}
\examples{data <- rescale(as.matrix(flea[, 1:5]))
ranges <- range_dist(data, 10000)
qplot(c(scaled_limits), geom = "freqpoly", binwidth = 0.1) + expand_limits(x = c(-1,1))}

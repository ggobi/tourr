\name{paths_index}
\alias{paths_index}
\title{Compute index value for many histories.}
\usage{paths_index(bases_list, index_f)}

\description{
  Compute index value for many histories.
}

\details{
  This is a convenience method that returns a data frame
  summarising the index values for multiple tour paths.
}
\keyword{internal}
\arguments{
  \item{bases_list}{list of histories produced by \code{\link{save_history}}}
  \item{index_f}{index function to apply to each projection}
}
\examples{holes1d <- guided_tour(holes, 1)
# Perform guided tour 5 times, saving results
tries <- replicate(5, save_history(flea[, 1:6], holes1d), simplify = FALSE)
# Interpolate between target bases 
itries <- lapply(tries, interpolate)

paths <- paths_index(itries, holes)
head(paths)

if (require(ggplot2)) {
qplot(step, value, data=paths, group=try, geom="line")
qplot(step, improvement, data=paths, group=try, geom="line")
}}

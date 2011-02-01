\name{path_index}
\alias{path_index}
\title{Compute index values for a tour history.}
\usage{path_index(history, index_f, data=attr(history, "data"))}

\description{
  Compute index values for a tour history.
}
\keyword{hplot}
\seealso{\code{\link{save_history}} for options to save history}
\arguments{
  \item{history}{list of bases produced by \code{\link{save_history}} 
(or otherwise)}
  \item{index_f}{index function to apply to each basis}
  \item{data}{dataset to be projected on to bases}
}
\examples{fl_holes <- save_history(flea[, 1:6], guided_tour(holes), sphere = TRUE)
path_index(fl_holes, holes)
path_index(fl_holes, cm)

plot(path_index(fl_holes, holes), type = "l")
plot(path_index(fl_holes, cm), type = "l")

# Use interpolate to show all intermediate bases as well
hi <- path_index(interpolate(fl_holes), holes)
hi
plot(hi)}

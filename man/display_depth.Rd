\name{display_depth}
\title{Display 3d projection with depth cues...}
\usage{display_depth(data, ...)
animate_depth(data, tour_path = grand_tour(3), ...)}

\description{
  Display 3d projection with depth cues
}

\details{
  Suggestion to use gray background and colour saturation (instead of
  gray shading) by Graham Wills. Animate a 3d projection with depth
  cues.
}
\seealso{\code{\link{animate}} for options that apply to all animations}
\keyword{hplot}
\alias{display_depth}
\alias{animate_depth}
\arguments{
  \item{data}{matrix, or data frame containing numeric columns}
  \item{tour_path}{tour path generator, defaults to the grand tour}
  \item{...}{other arguments passed on to \code{\link{animate}}}
}
\examples{animate_depth(flea[, 1:6])}

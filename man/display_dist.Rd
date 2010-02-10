\name{display_dist}
\title{1d distribution tour path animation.}
\usage{display_dist(method="density", center=TRUE, ...)}

\description{
  1d distribution tour path animation.
}

\details{
  Animate a 1d tour path with a density plot or histogram.
}
\seealso{\code{\link{animate}} for options that apply to all animations}
\keyword{hplot}
\alias{display_dist}
\alias{animate_dist}
\arguments{
  \item{method}{display method, histogram or density plot}
  \item{center}{should 1d projection be centered to have mean zero (default: TRUE).
This pins the centre of distribution to the same place, and makes it
easier to focus on the shape of the distribution.}
  \item{...}{other arguments passed on to \code{\link{animate}}}
}
\examples{animate_dist(flea[, 1:6])

# When the distribution is not centred, it tends to wander around in a 
# distracting manner
animate_dist(flea[, 1:6], center = FALSE)

# Alternatively, you can display the distribution with a histogram
animate_dist(flea[, 1:6], method = "hist")}

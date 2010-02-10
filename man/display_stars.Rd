\name{display_stars}
\title{Star glyph tour path animation.}
\usage{display_stars(...)}

\description{
  Star glyph tour path animation.
}

\details{
  Animate a nD tour path with star glyphs.

  Currently, scaling doesn't seem to be computed absolutely correctly,
  as centres move around as well as outside points.
}
\seealso{\code{\link{animate}} for options that apply to all animations}
\keyword{hplot}
\alias{display_stars}
\alias{animate_stars}
\arguments{
  \item{...}{other arguments passed on to \code{\link{stars}}}
}
\examples{animate_stars(flea[1:10, 1:6])
animate_stars(flea[1:10, 1:6], grand_tour(5))
animate_stars(flea[, 1:6], grand_tour(5))
animate_stars(flea[1:10, 1:6], grand_tour(5), 
col.stars = rep("grey50", 10), radius = FALSE)}

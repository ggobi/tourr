\name{display_faces}
\title{Chernoff faces tour path animation.}
\usage{display_faces(...)}

\description{
  Chernoff faces tour path animation.
}

\details{
  Animate a nD tour path with Chernoff's faces.  Can
  display up to 18 dimensions.

  This function requires the \code{TeachingDemos} package
  to draw the Chernoff faces.  See
  \code{\link[TeachingDemos]{faces2}} for more details.
}
\seealso{\code{\link{animate}} for options that apply to all animations}
\keyword{hplot}
\alias{display_faces}
\alias{animate_faces}
\arguments{
  \item{...}{other arguments passed on to \code{\link{animate}}}
}
\examples{# The drawing code is fairly slow, so this animation works best with a 
# limited number of cases
animate_faces(flea[1:2, 1:6])
animate_faces(flea[1:4, 1:6])

animate_faces(flea[1:2, 1:6], grand_tour(5))}

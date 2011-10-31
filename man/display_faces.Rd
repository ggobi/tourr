\name{display_faces}
\alias{animate_faces}
\alias{display_faces}
\title{Chernoff faces tour path animation.}
\usage{
  display_faces(...)

  animate_faces(data, tour_path = grand_tour(3), ...)
}
\arguments{
  \item{...}{other arguments passed on to
  \code{\link{animate}}}

  \item{data}{matrix, or data frame containing numeric
  columns}

  \item{tour_path}{tour path generator, defaults to 2d
  grand tour}
}
\description{
  Animate a nD tour path with Chernoff's faces.  Can
  display up to 18 dimensions.
}
\details{
  This function requires the \code{TeachingDemos} package
  to draw the Chernoff faces.  See
  \code{\link[TeachingDemos]{faces2}} for more details.
}
\examples{
# The drawing code is fairly slow, so this animation works best with a
# limited number of cases
animate_faces(flea[1:2, 1:6])
animate_faces(flea[1:4, 1:6])

animate_faces(flea[1:2, 1:6], grand_tour(5))
}
\seealso{
  \code{\link{animate}} for options that apply to all
  animations
}
\keyword{hplot}


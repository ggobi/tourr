\name{animate}
\alias{animate}
\title{Animate a tour path.}
\usage{animate(data, tour_path=grand_tour(), display=display_xy(),
    start, aps=1, fps=30, max_frames=Inf, rescale=TRUE,
    sphere=FALSE, ...)}

\description{
  Animate a tour path.
}

\details{
  This is the function that powers all of the tour
  animations.  If you want to write your own tour animation
  method, the best place to start is by looking at the code
  for animation methods that have already implemented in
  the package.

  See \code{\link{render}} to render animations to disk.
}
\value{an (invisible) list of bases visited during this tour}
\arguments{
  \item{data}{matrix, or data frame containing numeric columns}
  \item{tour_path}{tour path generator, defaults to 2d grand tour}
  \item{start}{projection to start at, if not specified, uses default 
associated with tour path}
  \item{display}{takes the display that is suppose to be used, defaults to
the xy display}
  \item{aps}{target angular velocity (in radians per second)}
  \item{fps}{target frames per second (defaults to 30)}
  \item{max_frames}{the maximum number of bases to generate.  Defaults to
Inf for interactive use (must use Ctrl + C to terminate), and 1 for 
non-interactive use.}
  \item{rescale}{if true, rescale all variables to range [0,1]?}
  \item{sphere}{if true, sphere all variables}
  \item{...}{ignored}
}
\examples{f <- flea[, 1:6]
animate(f, grand_tour(), display_xy())
# or in short
animate(f)
animate(f, max_frames = 30)

\dontrun{animate(f, max_frames = 10, fps = 1, aps = 0.1)}}

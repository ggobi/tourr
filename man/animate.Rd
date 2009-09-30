\name{animate}
\alias{animate}
\title{Animate a tour path.}
\usage{animate(data, tour_path=grand_tour(), display=display_xy(), aps=1,
    fps=30, max_frames=Inf, ..., rescale=TRUE, sphere=FALSE)}

\description{
  Animate a tour path.
}

\details{
  This is the function that powers all of the tour animations.  If you
  want to write your own tour animation method, the best place to start
  is by looking at the code for animation methods that have already
  implemented in the package.

  Animations can be rendered on screen, or saved to disk.  Saving an
  animation to disk allows you to recreate a movie that is much
  smoother, but takes considerably more time to generate.
}
\arguments{
  \item{data}{matrix, or data frame containing numeric columns}
  \item{tour_path}{tour path generator, defaults to the grand tour}
  \item{display}{takes the display that is suppose to be used, defaults to the xy display}
  \item{aps}{target angular velocity (in radians per second)}
  \item{fps}{target frames per second (defaults to 30)}
  \item{max_frames}{the maximum number of bases to generate.  Defaults to
Inf for interactive use (must use Ctrl + C to terminate), and 1 for 
non-interactive use.  It's recommended that you set this value to a 
finite number if you are saving the animation to disk.}
  \item{...}{other arguments are based on to the tour path generator}
  \item{rescale}{if true, rescale all variables to range [0,1]?}
  \item{sphere}{if true, sphere all variables
device}
}
\examples{f <- flea[, 1:6]
animate(f, grand_tour(), display_xy())
# or in short
animate(f)
animate(f, max_frames = 30)
animate(f, max_frames = 10, fps = 1, aps = 0.1)

animate_xy(f, max_frames = 100, file = "test.pdf", dev = pdf)}

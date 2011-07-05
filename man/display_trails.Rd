\name{display_trails}
\alias{animate_trails}
\alias{display_trails}
\title{Display tour path with trails...}
\usage{
  display_trails(center = TRUE, axes = "center", half_range
  = NULL, col = "black", pch = 20, past = 3, ...)
}
\arguments{
  \item{axes}{position of the axes: center, bottomleft or
  off}

  \item{center}{if TRUE, centers projected data to (0,0).
  This pins the center of data cloud and make it easier to
  focus on the changing shape rather than position.}

  \item{half_range}{half range to use when calculating
  limits of projected. If not set, defaults to maximum
  distance from origin to each row of data.}

  \item{col}{color to be plotted.  Defaults to "black"}

  \item{pch}{size of the point to be plotted.  Defaults to
  20.}

  \item{past}{draw line between current projection and
  projection \code{past} steps ago}

  \item{...}{other arguments passed on to
  \code{\link{animate}} and \code{\link{display_xy}}}
}
\description{
  Display tour path with trails
}
\details{
  Animate a 2D tour path with a point trails
}


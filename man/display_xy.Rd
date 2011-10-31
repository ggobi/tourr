\name{display_xy}
\alias{animate_xy}
\alias{display_xy}
\title{Display tour path with a scatterplot}
\usage{
  display_xy(center = TRUE, axes = "center",
    half_range = NULL, col = "black", pch = 20, ...)

  animate_xy(data, tour_path = grand_tour(), ...)
}
\arguments{
  \item{axes}{position of the axes: center, bottomleft or
  off}

  \item{center}{if TRUE, centers projected data to (0,0).
  This pins the center of data cloud and make it easier to
  focus on the changing shape rather than position.}

  \item{half_range}{half range to use when calculating
  limits of projected.  If not set, defaults to maximum
  distance from origin to each row of data. #'}

  \item{col}{color to be plotted.  Defaults to "black"}

  \item{pch}{size of the point to be plotted.  Defaults to
  20.}

  \item{...}{other arguments passed on to
  \code{\link{animate}} and \code{\link{display_xy}}}

  \item{data}{matrix, or data frame containing numeric
  columns}

  \item{tour_path}{tour path generator, defaults to 2d
  grand tour}
}
\description{
  Animate a 2D tour path with a scatterplot.
}
\examples{
animate_xy(flea[, 1:6])
animate(flea[, 1:6], grand_tour(), display_xy())
animate(flea[, 1:6], grand_tour(), display_xy(axes = "bottomleft"))
animate(flea[, 1:6], grand_tour(), display_xy(half_range = 0.5))
animate_xy(flea[, 1:6], little_tour())
animate_xy(flea[, 1:3], guided_tour(holes), sphere = TRUE)
animate_xy(flea[, 1:6], center = FALSE)

# The default axes are centered, like a biplot, but there are other options
animate_xy(flea[, 1:6], axes = "bottomleft")
animate_xy(flea[, 1:6], axes = "off")
animate_xy(flea[, 1:6], dependence_tour(c(1, 2, 1, 2, 1, 2)),
  axes = "bottomleft")
require(colorspace)
pal <- rainbow_hcl(length(levels(flea$species)))
col <- pal[as.numeric(flea$species)]
animate_xy(flea[,-7], col=col)
}


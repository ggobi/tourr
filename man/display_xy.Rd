\name{display_xy}
\title{Display tour path with a scattploer...}
\usage{display_xy(data, center = TRUE, axes = "center", limit = NULL, col = "black", pch  = 20, ...)
animate_xy(data, tour_path = grand_tour(), ...)}
\description{Display tour path with a scattploer}
\details{Animate a 2D tour path with a scatterplot.}
\alias{display_xy}
\alias{animate_xy}
\arguments{\item{data}{matrix, or data frame containing numeric columns}
\item{axes}{position of the axes: center, bottomleft or off}
\item{center}{if TRUE, centers projected data to (0,0).  This pins the 
center of data cloud and make it easier to focus on the changing shape
rather than position.}
\item{limit}{limits of the projected data.  Defaults to 3 * square root
of the largest eigenvalue.}
\item{col}{color to be plotted.  Defaults to "black"}
\item{pch}{size of the point to be plotted.  Defaults to 20.}
\item{...}{other arguments passed on to \code{\link{animate}} and
\code{\link{display_xy}}}
\item{tour_path}{tour path, defaults to the grand tour}}
\examples{animate_xy(flea[, 1:6])
animate(flea[, 1:6], grand_tour(), display_xy())
animate(flea[, 1:6], grand_tour(), display_xy(axes = "bottomleft"))
animate(flea[, 1:6], grand_tour(), display_xy(limits = c(-3, 3)))
animate_xy(flea[, 1:6], little_tour())
animate_xy(flea[, 1:3], guided_tour(holes), sphere = TRUE)
animate_xy(flea[, 1:6], center = FALSE)

# The default axes are centered, like a biplot, but there are other options
animate_xy(flea[, 1:6], axes = "bottomleft")
animate_xy(flea[, 1:6], axes = "off")
animate_xy(flea[, 1:6], dependence_tour(c(1, 2, 1, 2, 1, 2)),
axes = "bottomleft")}

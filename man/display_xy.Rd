\name{display_xy}
\alias{display_xy}
\title{Display tour path with a scattploer...}
\usage{display_xy(data, center=TRUE, axes=center, limit, col=black, pch=20,
    ...)}
\description{Display tour path with a scattploer}
\details{Animate a 2D tour path with a scatterplot.}
\arguments{\item{data}{matrix, or data frame containing numeric columns}
\item{axes}{position of the axes: center, bottomleft or off}
\item{center}{if TRUE, centers projected data to (0,0).  This pins the 
center of data cloud and make it easier to focus on the changing shape
rather than position.}
\item{limit}{limits of the projected data.  Defaults to 3 * square root
of the largest eigenvalue.}
\item{col}{color to be plotted.  Defaults to "black"}
\item{pch}{size of the point to be plotted.  Defaults to 20.}
\item{...}{Not Used}}
\examples{animate(flea[, 1:6], grand_tour(), display_xy())
animate(flea[, 1:6], grand_tour(), display_xy(axes = "bottomleft"))
animate(flea[, 1:6], grand_tour(), display_xy(limits = c(-3, 3)))}

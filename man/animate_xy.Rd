\name{animate_xy}
\alias{animate_xy}
\title{Scatterplot tour path animation.}
\usage{animate_xy(data, tour_path=grand_tour(), ...)}
\description{Scatterplot tour path animation.}
\details{Animate a 2D tour path with a scatterplot.}
\arguments{\item{data}{matrix, or data frame containing numeric columns}
\item{tour_path}{tour path, defaults to the grand tour}
\item{...}{other arguments passed on to \code{\link{animate}} and
\code{\link{display_xy}}}}
\examples{animate_xy(flea[, 1:6])
animate_xy(flea[, 1:6], little_tour())
animate_xy(flea[, 1:3], guided_tour(holes), sphere = TRUE)
animate_xy(flea[, 1:6], center = FALSE)

# The default axes are centered, like a biplot, but there are other options
animate_xy(flea[, 1:6], axes = "bottomleft")
animate_xy(flea[, 1:6], axes = "off")
animate_xy(flea[, 1:6], dependence_tour(c(1, 2, 1, 2, 1, 2)),
axes = "bottomleft")}

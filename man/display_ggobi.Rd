\name{display_ggobi}
\alias{animate_ggobi}
\alias{display_ggobi}
\title{Display tour path with ggobi}
\usage{
  display_ggobi()

  animate_ggobi(data, tour_path = grand_tour(), ...)
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
  Use ggobi to display a 2D tour.
}
\examples{
\dontrun{
animate_ggobi(flea[, 1:6])
# Spelling things out explicitly
animate(flea[, 1:6], grand_tour(), display_ggobi())
}
}


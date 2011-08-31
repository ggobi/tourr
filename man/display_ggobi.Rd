\name{display_ggobi}
\alias{animate_ggobi}
\alias{display_ggobi}
\title{Display tour path with ggobi}
\arguments{
  \item{...}{other arguments passed on to
  \code{\link{animate}}}
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


\name{display_ggobi}
\title{Display tour path with ggobi...}

\description{
  Display tour path with ggobi
}

\details{
  Use ggobi to display a 2D tour.
}
\alias{display_ggobi}
\alias{animate_ggobi}
\arguments{
  \item{...}{other arguments passed on to \code{\link{animate}}}
}
\examples{\dontrun{
animate_ggobi(flea[, 1:6])
# Spelling things out explicitly
animate(flea[, 1:6], grand_tour(), display_ggobi())
}}

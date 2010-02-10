\name{render}
\alias{render}
\title{Render frames of animation to disk...}
\usage{render(data, tour_path, display, dev, ..., apf=1/10, frames=50,
    rescale=TRUE, sphere=FALSE, start)}

\description{
  Render frames of animation to disk
}
\keyword{hplot}
\arguments{
  \item{data}{matrix, or data frame containing numeric columns}
  \item{tour_path}{tour path generator}
  \item{display}{takes the display that is suppose to be used, defaults to
the xy display}
  \item{dev}{name of output device to use (e.g. \code{\link{png}},
\code{\link{pdf}})}
  \item{...}{other options passed to output device}
  \item{apf}{angle (in radians) per frame}
  \item{frames}{number of frames in output}
  \item{rescale}{if true, rescale all variables to range [0,1]}
  \item{sphere}{if true, sphere all variables}
  \item{start}{starting projection.  If \code{NULL}, uses path default.}
}
\examples{render(flea[, 1:4], grand_tour(), display_xy(), "pdf", "test.pdf")}

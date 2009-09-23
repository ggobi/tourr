\name{Laser measurements}
\docType{data}
\alias{laser}
\title{Turnable laser measurements from Bellcore}
\description{
 
This data came from an investigation of an experimental laser at
Bellcore.  It was a tunable laser, in the sense that both its
wavelength and power output were controllable.

Rotation helped the experimental physicists to characterize the
laser, which turned out not to be a very good one, due to its
unstable operating region.

This data initially came to the statistics research group when
Janette Cooper asked Paul Tukey to help her analyze the data
she had collected to describe the laser.

\itemize{
  \item ifront, current applied to the front of the laser
  \item iback, current applied to the back of the laser
  \item power, output power
  \item lambda, output wavelength
}

}
\usage{data(laser)}
\format{A 64 x 4 numeric array}
\keyword{datasets}
\examples{
head(laser)
animate_xy(laser[,-4])
}


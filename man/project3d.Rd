\name{project3d}
\alias{project3d}
\title{Stereographic projection...}
\usage{project3d(d3, length=par("din")[1] * 25.4, z0=300, d=30)}

\description{
  Stereographic projection
}

\details{
  Math from http://dogfeathers.com/java/3dproj.html
}
\keyword{internal}
\arguments{
  \item{d3}{3d numeric matrix giving position of points}
  \item{length}{width of plot, mm}
  \item{z0}{distance from eye to screen, mm}
  \item{d}{half interpupilary distance, mm}
}

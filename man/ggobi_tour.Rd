\name{ggobi_tour}
\alias{ggobi_tour}
\title{Use ggobi to display a tour path.}
\usage{ggobi_tour(data, tour_f=grand_tour, aps=1, fps=30, ..., rescale=TRUE,
    sphere=FALSE)}
\description{Use ggobi to display a tour path.}
\details{Prompts R to use rggobi as the window for the plotting}
\arguments{\item{data}{matrix, or data frame containing numeric columns}
\item{tour_f}{function sent to perform the tour}
\item{aps}{}
\item{fps}{how many frames per second should be used}
\item{...}{other arguments passed on to \code{\link{animate}}}
\item{rescale}{boolean as to put standardized data into ggobi (a transformation)}
\item{sphere}{boolean as to put principle components into ggobi (a transformation)}}
\examples{ggobi_tour(flea[,1:6])}

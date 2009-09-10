\name{planned_tour}
\alias{planned_tour}
\title{The planned tour.}
\usage{planned_tour(basis_set, cycle=FALSE, ...)}
\description{The planned tour.}
\details{The planned tour takes you from one basis to the next in a 
set order.  Once you have visited all the planned bases, you either stop
or start from the beginning once more (if \code{cycle = TRUE}). 

Usually, you will not call this function directly, but will pass it to 
a method that works with tour paths like \code{\link{save_history}}, 
or \code{\link{animate}}}
\keyword{hplot}
\keyword{dynamic}
\seealso{The \code{\link{little_tour}}, a special type of planned tour
which cycles between all axis parallel projections.  
\code{save_history} for saving the output of another tour path
to replay later with the planned tour}
\arguments{\item{basis_set}{the set of bases as a list of projection matrices
or a 3d array}
\item{cycle}{cycle through continuously (\code{TRUE}) or stop after 
first pass (\code{FALSE})}
\item{...}{Not Used}}
\examples{twod <- save_history(flea[, 1:3], max = 5)
str(twod)
animate_xy(flea[, 1:3], planned_tour(twod))
animate_xy(flea[, 1:3], planned_tour(twod, TRUE))

oned <- save_history(flea[, 1:6], grand_tour(1), max = 3)
animate_dist(flea[, 1:6], planned_tour(oned))}

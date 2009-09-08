\name{tour}
\alias{tour}
\title{Generic tour method.}
\usage{tour(data, tour_path, start, velocity=0.05, total_steps=100,
    step_fun=nul, target_fun=nul, ...)}
\description{Generic tour method.}
\details{The tour function provides the common machinery behind all tour methods:
interpolating from basis to basis, and generating new bases when necessary.
You should not have to call this function unless you are writing your own
tour path method.}
\seealso{\code{\link{grand_tour}}, \code{\link{guided_tour}}, 
\code{\link{local_tour}}, \code{\link{little_tour}} for concrete 
tour path implementations.}
\keyword{hplot}
\keyword{dynamic}
\arguments{\item{data}{the data matrix to be projected}
\item{tour_path}{basis generator, a function that generates a new basis,
called with the previous projection and the data set.  For more
complicated tour paths, this will need to be a closure with local
variables.  Should return NULL if the tour should terminate}
\item{start}{starting projection, if omitted will use random projection}
\item{velocity}{tour velocity in radians per step}
\item{total_steps}{maximum number of steps before termination (set to Inf 
to never terminate)}
\item{step_fun}{a function that is evaluated every time a step is taken.  
the function should have arguments step (the number of steps), proj (the
new projection matrix) and target (the current target frame)}
\item{target_fun}{a function that is evaluated every time a new target is 
chosen}}

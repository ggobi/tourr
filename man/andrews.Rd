\name{andrews}
\alias{andrews}
\title{Compute Andrews' curves...}
\usage{andrews(x)}

\description{
  Compute Andrews' curves
}

\details{
  This function takes a numeric vector of input, and returns a function
  which allows you to compute the value of the Andrew's curve at every
  point along its path from -pi to pi.
}
\value{a function with single argument, theta}
\arguments{
  \item{x}{input a new parameter}
}
\examples{a <- andrews(1:2)
a(0)
a(-pi)
grid <- seq(-pi, pi, length = 50)
a(grid)

plot(grid, andrews(1:2)(grid), type = "l")
plot(grid, andrews(runif(5))(grid), type = "l")}

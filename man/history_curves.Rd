\name{history_curves}
\alias{history_curves}
\title{Draw the path that the geodesics took.}
\usage{history_curves(history, data=attr(history, "data"))}
\description{Draw the path that the geodesics took.}
\details{This computes the projected values of each observation at each step, and 
allows you to recreate static views of the animated plots.}
\arguments{\item{history}{list of bases produced by \code{\link{save_history}} 
(or otherwise)}
\item{data}{dataset to be projected on to bases}}
\examples{path1d <- save_history(flea[, 1:6], grand_tour(1), 10)
path2d <- save_history(flea[, 1:6], grand_tour(2), 10)

plot(history_curves(path1d))
plot(history_curves(interpolate(path1d)))

plot(history_curves(path2d))
plot(history_curves(interpolate(path2d)))

# Instead of relying on the built in plot method, you might want to 
# generate your own.  Here are few examples of alternative displays:

df <- history_curves(path2d)
qplot(step, value, data = df, group = obs:var, geom = "line", colour=var)

qplot(`1`, `2`, data = cast(df, ... ~ var)) + 
facet_wrap( ~ step) + 
coord_equal()
qplot(step, value, data = df, colour = var, geom="line") + 
facet_wrap( ~ obs)}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.r
\docType{data}
\name{Places Ratings}
\alias{Places Ratings}
\alias{places}
\title{Ratings of different locations across North America}
\format{
A 329 x 14 numeric array
}
\description{
The "places data" were distributed to interested ASA members a few years ago
so that they could apply contemporary data analytic methods to describe
these data and then present results in a poster session at the ASA annual
conference.  Latitude and longitude have been added by Paul Tukey.
}
\details{
____________________________________________________________________

The first dataset is taken from the Places Rated Almanac, by Richard Boyer
and David Savageau, copyrighted and published by Rand McNally. This book
order (SBN) number is 0-528-88008-X, and it retails for $14.95 .  The data
are reproduced on disk by kind permission of the publisher, and with the
request that the copyright notice of Rand McNally, and the names of the
authors appear in any paper or presentation using these data.

The nine rating criteria used by Places Rated Almanac are: Climate and
Terrain Housing Health Care and Environment Crime Transportation Education
The Arts Recreation Economics

For all but two of the above criteria, the higher the score, the better.
For Housing and Crime, the lower the score the better.

The scores are computed using the following component statistics for each
criterion (see the Places Rated Almanac for details):

Climate and Terrain: very hot and very cold months, seasonal temperature
variation, heating- and cooling-degree days, freezing days, zero-degree
days, ninety-degree days.

Housing: utility bills, property taxes, mortgage payments.

Health Care and Environment: per capita physicians, teaching hospitals,
medical schools, cardiac rehabilitation centers, comprehensive cancer
treatment centers, hospices, insurance/hospitalization costs index,
flouridation of drinking water, air pollution.

Crime: violent crime rate, property crime rate.

Transportation: daily commute, public transportation, Interstate highways,
air service, passenger rail service.

Education: pupil/teacher ratio in the public K-12 system, effort index in
K-12, accademic options in higher education.

The Arts: museums, fine arts and public radio stations, public television
stations, universities offering a degree or degrees in the arts, symphony
orchestras, theatres, opera companies, dance companies, public libraries.

Recreation: good restaurants, public golf courses, certified lanes for
tenpin bowling, movie theatres, zoos, aquariums, family theme parks,
sanctioned automobile race tracks, pari-mutuel betting attractions, major-
and minor- league professional sports teams, NCAA Division I football and
basketball teams, miles of ocean or Great Lakes coastline, inland water,
national forests, national parks, or national wildlife refuges, Consolidated
Metropolitan Statistical Area access.

Economics: average household income adjusted for taxes and living costs,
income growth, job growth.
}
\examples{

head(places)
animate_xy(places[, 1:9])
}
\keyword{datasets}

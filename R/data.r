#' Flea beatle measurements
#'
#' This data is from a paper by A. A. Lubischew, "On the Use of Discriminant
#' Functions in Taxonomy", Biometrics, Dec 1962, pp.455-477. Data is
#' standardized, and original units are in flea_raw.
#'
#' \itemize{
#'   \item tars1, width of the first joint of the first tarsus in microns
#'   (the sum of measurements for both tarsi)
#'   \item tars2, the same for the second joint
#'   \item head, the maximal width of the head between the external edges of the
#'   eyes in 0.01 mm
#'   \item ade1, the maximal width of the aedeagus in the fore-part in microns
#'   \item ade2, the front angle of the aedeagus ( 1 unit = 7.5 degrees)
#'   \item ade3, the aedeagus width from the side in microns
#'   \item species, which species is being examined - concinna, heptapotamica, heikertingeri
#' }
#'
#' @name Flea measurements
#' @aliases flea, flea_raw
#' @docType data
#' @format A 74 x 7 numeric array
#' @keywords datasets
#' @examples
#'
#' head(flea)
#' animate_xy(flea[, -7])
#' animate_xy(flea[, -7], col = flea[, 7])
"flea"

#' Turnable laser measurements from Bellcore
#'
#'
#' This data came from an investigation of an experimental laser at Bellcore.
#' It was a tunable laser, in the sense that both its wavelength and power
#' output were controllable.
#'
#' Rotation helped the experimental physicists to characterize the laser, which
#' turned out not to be a very good one, due to its unstable operating region.
#'
#' This data initially came to the statistics research group when Janette
#' Cooper asked Paul Tukey to help her analyze the data she had collected to
#' describe the laser.
#'
#' \itemize{
#'  \item ifront, current applied to the front of the laser
#'  \item iback, current applied to the back of the laser
#'  \item power, output power
#'  \item lambda, output wavelength
#' }
#'
#'
#' @name Laser measurements
#' @aliases laser
#' @docType data
#' @format A 64 x 4 numeric array
#' @keywords datasets
#' @examples
#'
#' head(laser)
#' animate_xy(laser[, -4])
NULL

#' Olive oil samples from Italy
#'
#' This data is from a paper by Forina, Armanino, Lanteri, Tiscornia (1983)
#' Classification of Olive Oils from their Fatty Acid Composition, in Martens
#' and Russwurm (ed) Food Research and Data Anlysis. We thank Prof. Michele
#' Forina, University of Genova, Italy for making this dataset available.
#'
#' \itemize{
#'   \item region Three super-classes of Italy: North, South and the
#'     island of Sardinia
#'   \item area Nine collection areas: three from North, four from South
#'     and 2 from Sardinia
#'   \item palmitic, palmitoleic, stearic, oleic, linoleic, linolenic,
#'     arachidic, eicosenoic fatty acids percent x 100
#' }
#'
#' @name Olive oil measurements
#' @aliases olive
#' @docType data
#' @format A 572 x 10 numeric array
#' @keywords datasets
#' @examples
#'
#' head(olive)
#' animate_xy(olive[, c(7, 9, 10)])
#' animate_xy(olive[, c(7, 9, 10)], col = olive[, 1])
NULL

#' Monthly ozone measurements over Central America
#'
#'
#' This data set is a subset of the data from the 2006 ASA Data expo challenge.
#' The data are monthly ozone averages on a very coarse 24 by 24 grid covering
#' Central America, from Jan 1995 to Dec 2000. The data is stored in a 3d area
#' with the first two dimensions representing latitude and longitude, and the
#' third representing
#' time.
#'
#' @name Ozone measurements
#' @aliases ozone
#' @docType data
#' @format A 24 x 24 x 72 numeric array
#' @keywords datasets
#' @examples
#'
#' example(display_image)
NULL

#' Ratings of different locations across North America
#'
#'
#' The "places data" were distributed to interested ASA members a few years ago
#' so that they could apply contemporary data analytic methods to describe
#' these data and then present results in a poster session at the ASA annual
#' conference.  Latitude and longitude have been added by Paul Tukey.
#'
#' ____________________________________________________________________
#'
#' The first dataset is taken from the Places Rated Almanac, by Richard Boyer
#' and David Savageau, copyrighted and published by Rand McNally. This book
#' order (SBN) number is 0-528-88008-X, and it retails for $14.95 .  The data
#' are reproduced on disk by kind permission of the publisher, and with the
#' request that the copyright notice of Rand McNally, and the names of the
#' authors appear in any paper or presentation using these data.
#'
#' The nine rating criteria used by Places Rated Almanac are: Climate and
#' Terrain Housing Health Care and Environment Crime Transportation Education
#' The Arts Recreation Economics
#'
#' For all but two of the above criteria, the higher the score, the better.
#' For Housing and Crime, the lower the score the better.
#'
#' The scores are computed using the following component statistics for each
#' criterion (see the Places Rated Almanac for details):
#'
#' Climate and Terrain: very hot and very cold months, seasonal temperature
#' variation, heating- and cooling-degree days, freezing days, zero-degree
#' days, ninety-degree days.
#'
#' Housing: utility bills, property taxes, mortgage payments.
#'
#' Health Care and Environment: per capita physicians, teaching hospitals,
#' medical schools, cardiac rehabilitation centers, comprehensive cancer
#' treatment centers, hospices, insurance/hospitalization costs index,
#' flouridation of drinking water, air pollution.
#'
#' Crime: violent crime rate, property crime rate.
#'
#' Transportation: daily commute, public transportation, Interstate highways,
#' air service, passenger rail service.
#'
#' Education: pupil/teacher ratio in the public K-12 system, effort index in
#' K-12, accademic options in higher education.
#'
#' The Arts: museums, fine arts and public radio stations, public television
#' stations, universities offering a degree or degrees in the arts, symphony
#' orchestras, theatres, opera companies, dance companies, public libraries.
#'
#' Recreation: good restaurants, public golf courses, certified lanes for
#' tenpin bowling, movie theatres, zoos, aquariums, family theme parks,
#' sanctioned automobile race tracks, pari-mutuel betting attractions, major-
#' and minor- league professional sports teams, NCAA Division I football and
#' basketball teams, miles of ocean or Great Lakes coastline, inland water,
#' national forests, national parks, or national wildlife refuges, Consolidated
#' Metropolitan Statistical Area access.
#'
#' Economics: average household income adjusted for taxes and living costs,
#' income growth, job growth.
#'
#'
#' @name Places Ratings
#' @aliases places
#' @docType data
#' @format A 329 x 14 numeric array
#' @keywords datasets
#' @examples
#'
#' head(places)
#' animate_xy(places[, 1:9])
NULL

#' Rat CNS Gene Expression
#'
#'
#' Columns:
#'
#' e11  e13  e15  e18  e21   p0   p7  p14    a class1 class2
#' \itemize{
#'   \item e11, an ebryonic timepoint from the original data with the number
#'      corresponding to the day
#'   \item e13, an ebryonic timepoint from the original data with the number
#'      corresponding to the day
#'   \item e15, an ebryonic timepoint from the original data with the number
#'      corresponding to the day
#'   \item e18, an ebryonic timepoint from the original data with the number
#'      corresponding to the day
#'   \item e21, an ebryonic timepoint from the original data with the number
#'     corresponding to the day
#'   \item p0, a postnatal timpoint from the original data with the number
#'     corresponding to the day
#'   \item p7, a postnatal timpoint from the original data with the number
#'     corresponding to the day
#'   \item p14, a postnatal timpoint from the original data with the number
#'     corresponding to the day
#'   \item a, a postnatal timpoint from the original data. It is equivalent to
#'     p90.
#'   \item class1, is the high-level class: its range is 1:4
#'   \item class2, breaks down the high-level classes, so its range is 1:14
#' }
#'
#' Rows: Each case is a gene (or gene family?) And each cell is the gene
#' expression level for that gene at time t, averaging a few measured values
#' and normalizing using the maximum expression value for that gene.
#'
#' Reference (available on the web at pnas.org): Large-scale temporal gene
#' expression mapping of central nervous system development by X. Wen, S.
#' Fuhrman, G. S. Michaels, D. B. Carr, S. Smith, J. L. Barker, R. Somogyi in
#' the Proceedings of the National Academy of Science, Vol 95, pp. 334-339,
#' January 1998
#'
#'
#' @name Rat CNS
#' @aliases ratcns
#' @docType data
#' @format A 112 x 11 numeric array
#' @references https://www.pnas.org
#' @keywords datasets
#' @examples
#'
#' head(ratcns)
#' animate_xy(ratcns[, 1:8], col = ratcns[, 10])
NULL

#' Tropical Atmosphere Ocean data
#'
#'
#' This is a subset of data taken from the NOAA web site
#' https://www.pmel.noaa.gov/tao/. The data is generated from recording
#' instruments on a grid of buoys laid out over the Pacific Ocean.  The grid
#' was setup to monitor El Nino and La Nina events. This subset contains
#' measurements from 5 locations (0deg/110W, 2S/110W, 0deg/95W,2S/95W,5S/95W)
#' and two time points Nov-Jan 1993 (normal), 1997 (El Nino). There are missing
#' values in this data set, which need to be removed, or imputed before running
#' a tour.
#'
#'
#' @name Tropical Atmosphere Ocean
#' @aliases tao
#' @docType data
#' @format A 736 x 8 numeric array
#' @references https://www.pmel.noaa.gov/tao/
#' @keywords datasets
NULL

#' Saved history of guided tour with holes
#'
#' This data was generated from the following code:
#' set.seed(2020)
#' t1 <- save_history(flea[, 1:6], guided_tour(holes()), max = 100)
#' attr(t1, "class") <- NULL
#' And used as an example for search_polish() to start
#' optimising from the best projection from search_geodesic.
#' t1 is a 3D array or 2D projections.
#'
#' @name t1
#' @aliases t1
#' @docType data
#' @keywords datasets
NULL

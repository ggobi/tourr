\name{Flea measurements}
\docType{data}
\alias{flea}
\title{Flea beatle measurements}
\description{
This data is from a paper by A. A. Lubischew, "On the Use of
Discriminant Functions in Taxonomy", Biometrics, Dec 1962, pp.455-477.


\itemize{
\item tars1, width of the first joint of the first tarsus in microns (the sum of measurements for both tarsi)
\item tars2, the same for the second joint
\item head, the maximal width of the head between the external edges of the
        eyes in 0.01 mm
\item ade1, the maximal width of the aedeagus in the fore-part in microns
\item ade2, the front angle of the aedeagus ( 1 unit = 7.5 degrees)
\item ade3, the aedeagus width from the side in microns
\item species, which species is being examined - concinna, heptapotamica, heikertingeri
}
    
    
    
}
\usage{data(flea)}
\format{A 74 x 7 numeric array}

\keyword{datasets}
\examples{
head(flea)
animate_xy(flea[,-7])
animate_xy(flea[,-7],col=flea[,7])
}

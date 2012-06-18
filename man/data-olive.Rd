\name{Olive oil measurements}
\docType{data}
\alias{olive}
\title{Olive oil samples from Italy}
\description{
This data is from a paper by Forina, Armanino, Lanteri, Tiscornia (1983) Classification of Olive Oils from their Fatty Acid Composition, in Martens and Russwurm (ed) Food Research and Data Anlysis. We thank Prof. Michele Forina, University of Genova, Italy for making this dataset available.

\itemize{
\item region Three super-classes of Italy: North, South and the island of Sardinia
\item area Nine collection areas: three from North, four from South and 2 from Sardinia
\item palmitic, palmitoleic, stearic, oleic, linoleic, linolenic, arachidic, eicosenoic fatty acids percent x 100
}
}

\usage{data(olive)}
\format{A 572 x 10 numeric array}
\keyword{datasets}
\examples{
head(olive)
animate_xy(olive[,c(7,9,10)])
animate_xy(olive[,c(7,9,10)],col=olive[,1])
}

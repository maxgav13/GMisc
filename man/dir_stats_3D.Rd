% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dir_stats_3D.R
\name{dir_stats_3D}
\alias{dir_stats_3D}
\title{Directional statistics for the 3D case}
\usage{
dir_stats_3D(dir, dip, conf.level = 0.95)
}
\arguments{
\item{dir}{A vector of dip directions measurements in degrees}

\item{dip}{A vector of dip angles measurements in degrees}

\item{conf.level}{Confidence level to use for the cone of confidence (Default is 0.95)}
}
\value{
A data frame with the mean direction, mean dip angle, mean resultant length, spherical variance, concentration parameter, and cone of confidence
}
\description{
Calculates the directional (spherical) statistics of a sample of directional measurements for the 3D case (plane with dip direction and dip angle or line with trend and plunge).
}
\details{
This always considers a plane expressed as dip direction and dip angle, it could be a bedding plane or a fault plane
}
\examples{
dir = c(12,18,22,15,10,20)
dip = c(42,40,48,30,42,30)
dir_stats_3D(dir, dip)

}
\references{
Borradaile, G. (2003). Statistics of Earth Science Data. Springer.
}

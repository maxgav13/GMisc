% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/d2s.R
\name{d2s}
\alias{d2s}
\title{Conversion from dip directions to strike}
\usage{
d2s(x)
}
\arguments{
\item{x}{A vector of directions in degrees}
}
\value{
A vector or directions
}
\description{
Calculates strike directions from dip directions.
}
\examples{
x <- runif(min = 30, max = 80, n = 20)
strike <- d2s(x)
dip.dir <- s2d(strike)

}

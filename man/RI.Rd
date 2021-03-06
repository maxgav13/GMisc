% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RI.R
\name{RI}
\alias{RI}
\title{Intraclass Correlation Coefficient for layer boundary determination}
\usage{
RI(x, k = 6)
}
\arguments{
\item{x}{A data frame containing the depth of perforation in the first column, and the value of interest in the second column}

\item{k}{The window length for the number of data points to include in the calculation of RI. Always and even (par) number}
}
\value{
ggplot and plotly objects showing the RI statistic and lines marking the critical values of 0.7 and 0.8, and suggested boundaries
}
\description{
Calculates the intraclass correlation coefficient (RI) for a perforation log, and shows the locations where this coefficient is higher than 0.7 and 0.8. The coefficient is used to find layer boundaries in a perforation log.
}
\details{
The example data given is intended to show the structure needed for input data. The user should follow this structure, which in general corresponds with a data frame with a sequence in the first column and the observed/measured values in the second column
}
\examples{
RI(DPM_data, k = 6)

}
\references{
Mora, R. (2013). Uso de metodos estadisticos para la determinacion de capas homogeneas de suelos volcanicos en un sitio de las laderas del Volcan Irazu, Cartago, Costa Rica. - Rev. Geol. Amer. Central, 49: 101-108.
}

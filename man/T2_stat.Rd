% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/T2_stat.R
\name{T2_stat}
\alias{T2_stat}
\title{Hotelling T2-statistic for layer boundary determination}
\usage{
T2_stat(x, k = 50)
}
\arguments{
\item{x}{A data frame containing the depth of perforation in the first column, and the variables of interest in the rest of the columns, for a CPTu test: point resistance (qc), sleeve friction (fs), and pore-water pressure (u)}

\item{k}{The window length for the number of data points to include in the calculation of T2-statistic. Always and even (par) number}
}
\value{
ggplot and plotly objects showing the Mahalanobis D2 statistic and lines marking the critical values at 0.95, 0.99, and 0.999, and suggested boundaries
}
\description{
Calculates the T2-statistic and corresponding effect size (Mahalanobis distance) for a perforation log with more than 1 variable. The coefficient is used to find layer boundaries in a perforation log.
}
\details{
The example data given is intended to show the structure needed for input data. The user should follow this structure, which in general corresponds with a data frame with a sequence in the first column and the observed/measured values in the second, third, and fourth columns
}
\examples{
T2_stat(CPTu_data, k = 50)

}
\references{
Davis, J. C. (2002). Statistical and Data Analysis in Geology. 3rd ed. John Wiley & Sons.
}

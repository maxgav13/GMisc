% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DS_plot.R
\name{DS_plot}
\alias{DS_plot}
\title{Draws the results for a direct shear test}
\usage{
DS_plot(sig.n, tau, units = "kPa")
}
\arguments{
\item{sig.n}{A numeric vector of the normal stress values}

\item{tau}{A numeric vector of the shear stress values}

\item{units}{A string with the units of the measurements}
}
\value{
A ggplot of the direct shear test results
}
\description{
Draws the sigma-tau pairs along with the Mohr-Coulomb failure envelope for a set of direct shear lab test results, and annotates the graph with the values of cohesion and friction angle.
}
\examples{
sig.n = c(80,237,395)
tau = c(127,345,475)
DS_plot(sig.n, tau)

}
\references{
Coduto, D. P. (1999). Geotechnical Engineering - Principles and Practices. Prentice Hall.

Holtz, R. D., Kovacs, W. D. & Sheahan, T. C. (2011). An Introduction to Geotechnical Engineering. Prentice Hall.

Gonzalez de Vallejo, L. I. (2004). Ingenieria Geologica. Prentice Hall.
}

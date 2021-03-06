% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CoDA_2Group_All.R
\name{CoDA_2Group_All}
\alias{CoDA_2Group_All}
\title{CoDA Two Group Hypothesis All At Once}
\usage{
CoDA_2Group_All(comp1, comp2)
}
\arguments{
\item{comp1}{A matrix of observations for composition 1. Entries must be non-zero and positive.}

\item{comp2}{A matrix of observations for composition 2. Entries must be non-zero and positive.}
}
\value{
A tibble with the statistic (Q), degrees of freedom (nu), p-value, and null hypothesis (H0)
}
\description{
Performs the calculations for the three hypotheses for two diferent samples, using Compositional Data Analysis (CoDA) principles.
}
\examples{
data("Hongite", package = 'compositions')
data("Kongite", package = 'compositions')
CoDA_2Group_All(Hongite,Kongite)

}
\references{
Pawlowsky-Hlahn, V., Egozcue, J.J & Tolosna-Delgado, R. (2015). Modeling and analysis of compositional data. John Wiley & Sons.

Aitchison, J. (1986). The statistical analysis of compositional data. Chapman and Hall.
}

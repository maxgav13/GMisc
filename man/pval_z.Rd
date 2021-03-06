% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pval_z.R
\name{pval_z}
\alias{pval_z}
\title{P-value for proposed mean of a population (known sigma)}
\usage{
pval_z(x, sig, n, mu = 0, side = c("two", "one"))
}
\arguments{
\item{x}{Sample mean}

\item{sig}{Population standard deviation}

\item{n}{Sample size}

\item{mu}{Population mean}

\item{side}{To either get a two-tail or one-tail p-value (Default is "two")}
}
\value{
P-value
}
\description{
Calculates the p-value for a proposed mean for a population with known standard deviation, using the Z-statistic (normal distribution).
}
\examples{
x <- 2.6
sig <- 0.3
n <- 36
mu <- 2.5
pval_z(x, sig, n, mu)

}

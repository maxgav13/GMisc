#' @title CoDA Two Group Hypothesis 1
#' @description Performs the calculations for the hypothesis of equal centers and covariance for two diferent samples, using Compositional Data Analysis (CoDA) principles.
#' @param comp1 A matrix of observations for composition 1. Entries must be non-zero and positive.
#' @param comp2 A matrix of observations for composition 2. Entries must be non-zero and positive.
#' @export
#' @return A tibble with the statistic (Q), degrees of freedom (nu), p-value, and null hypothesis (H0)
#' @references Pawlowsky-Hlahn, V., Egozcue, J.J & Tolosna-Delgado, R. (2015). Modeling and analysis of compositional data. John Wiley & Sons.
#' @references Aitchison, J. (1986). The statistical analysis of compositional data. Chapman and Hall.
#' @import stats
#' @import compositions
#' @import tidyverse
#' @name CoDA_2Group_H1
#' @examples
#' data("Hongite", package = 'compositions')
#' data("Kongite", package = 'compositions')
#' CoDA_2Group_H1(Hongite,Kongite)
#'
CoDA_2Group_H1 = function(comp1, comp2) {
  A = comp1
  B = comp2
  n1 = nrow(A)
  n2 = nrow(B)
  J = ncol(A)

  Aalr = alr(acomp(A)) %>% unclass()
  Balr = alr(acomp(B)) %>% unclass()

  A2 = Aalr
  B2 = Balr
  D = ncol(A2)

  Amu = colMeans(A2)
  Bmu = colMeans(B2)
  Acov = var(A2)*((n1-1)/n1)
  Bcov = var(B2)*((n2-1)/n2)
  Covp = (n1 * Acov + n2 * Bcov)/(n1 + n2) # pooled covariance
  muc = (n1 * Amu + n2 * Bmu)/(n1 + n2) # combined mean vector
  Covc = Covp + (n1*n2*(Amu-Bmu) %*%t (Amu-Bmu))/(n1+n2)^2 # combined covariance

  # H1 - equal centers and covariance
  H0 = 'equal centers and covariance'
  Q1vsg = n1*log(det(Covc)/det(Acov)) + n2*log(det(Covc)/det(Bcov))
  nu1 = .5*D*(D+3)
  qchisq(.95, nu1) # critical chisq value
  pval1 = pchisq(Q1vsg, nu1, lower.tail = F) # p-value of the statistic

  res = tibble(Q = Q1vsg, nu = nu1, pval = pval1, H0 = H0)

  return(res)

}

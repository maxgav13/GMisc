#' @title CoDA Two Group Hypothesis 2
#' @description Performs the calculations for the hypothesis of equal covariance with different centers for two diferent samples, using Compositional Data Analysis (CoDA) principles.
#' @param comp1 A matrix or data frame of observations for composition 1. Entries must be non-zero and positive.
#' @param comp2 A matrix or data frame of observations for composition 2. Entries must be non-zero and positive.
#' @export
#' @return A tibble with the statistic (Q), degrees of freedom (nu), p-value, and null hypothesis (H0)
#' @references Pawlowsky-Hlahn, V., Egozcue, J.J & Tolosna-Delgado, R. (2015). Modeling and analysis of compositional data. John Wiley & Sons.
#' @references Aitchison, J. (1986). The statistical analysis of compositional data. Chapman and Hall.
#' @name CoDA_2Group_H2
#' @examples
#' data("Hongite", package = 'compositions')
#' data("Kongite", package = 'compositions')
#' CoDA_2Group_H2(Hongite,Kongite)
#'
CoDA_2Group_H2 = function(comp1, comp2) {

  if (any(class(comp1) == "matrix")) {
    comp1 = comp1
  } else {
    comp1 = as.matrix(comp1)
  }

  if (any(class(comp2) == "matrix")) {
    comp2 = comp2
  } else {
    comp2 = as.matrix(comp2)
  }

  A = comp1
  B = comp2
  n1 = nrow(A)
  n2 = nrow(B)
  J = ncol(A)

  Aalr = compositions::alr(compositions::acomp(A)) %>% unclass()
  Balr = compositions::alr(compositions::acomp(B)) %>% unclass()

  A2 = Aalr
  B2 = Balr
  D = ncol(A2)

  Amu = colMeans(A2)
  Bmu = colMeans(B2)
  Acov = stats::var(A2)*((n1-1)/n1)
  Bcov = stats::var(B2)*((n2-1)/n2)
  Covp = (n1 * Acov + n2 * Bcov)/(n1 + n2) # pooled covariance
  muc = (n1 * Amu + n2 * Bmu)/(n1 + n2) # combined mean vector
  Covc = Covp + (n1*n2*(Amu-Bmu) %*%t (Amu-Bmu))/(n1+n2)^2 # combined covariance

  # H2 - equal covariance with different centers
  H0 = 'equal covariance with different centers'
  Q2vsg = n1*log(det(Covp)/det(Acov)) + n2*log(det(Covp)/det(Bcov))
  nu2 = .5*(D)*(D+1)
  stats::qchisq(.95, nu2) # critical chisq value
  pval2 = stats::pchisq(Q2vsg, nu2, lower.tail = F) # p-value of the statistic

  res = tibble::tibble(Q = Q2vsg, nu = nu2, pval = pval2, H0 = H0)

  return(res)

}

#' @title CoDA Two Group Hypothesis All At Once
#' @description Performs the calculations for the three hypotheses for two diferent samples, using Compositional Data Analysis (CoDA) principles.
#' @param comp1 A matrix of observations for composition 1. Entries must be non-zero and positive.
#' @param comp2 A matrix of observations for composition 2. Entries must be non-zero and positive.
#' @export
#' @return A tibble with the statistic (Q), degrees of freedom (nu), p-value, and null hypothesis (H0)
#' @references Pawlowsky-Hlahn, V., Egozcue, J.J & Tolosna-Delgado, R. (2015). Modeling and analysis of compositional data. John Wiley & Sons.
#' @references Aitchison, J. (1986). The statistical analysis of compositional data. Chapman and Hall.
#' @name CoDA_2Group_All
#' @examples
#' data("Hongite", package = 'compositions')
#' data("Kongite", package = 'compositions')
#' CoDA_2Group_All(Hongite,Kongite)
#'
CoDA_2Group_All = function(comp1, comp2) {
  res = dplyr::bind_rows(CoDA_2Group_H1(comp1,comp2),
                         CoDA_2Group_H2(comp1,comp2),
                         CoDA_2Group_H3(comp1,comp2))

  return(res)
}

#' @title CoDA Atypicality Index
#' @description Calculates the atypicality index (ranges between 0 and 1, 1 being more atypical/different) for all the observations of a composition or compares one sample to the given composition, using Compositional Data Analysis (CoDA) principles.
#' @param comp A matrix of observations for a given composition.
#' @param sample A one sample vector to compare against the given composition.
#' @export
#' @return A matrix with the atypicality index or indices.
#' @references Aitchison, J. (1986). The statistical analysis of compositional data. Chapman and Hall.
#' @import stats
#' @import compositions
#' @import tidyverse
#' @name CoDA_Atyp_Idx
#' @examples
#' data("Hongite", package = 'compositions')
#' samp = c(44, 20.4, 13.9, 9.1, 12.6)
#' CoDA_Atyp_Idx(Hongite)
#' CoDA_Atyp_Idx(Hongite, samp)
#'
CoDA_Atyp_Idx = function(comp, sample = NULL) {

  n1 = nrow(comp)
  A = alr(acomp(comp)) %>% unclass()
  Amu = colMeans(A)
  Acov = var(A)*((n1-1)/n1)
  N = nrow(comp)
  D = ncol(A)
  k = (N*(N-D))/((N^2-1)*D)

  if (is.null(sample)) {
    ai = matrix(0,nrow = N,ncol = 1)
    for (i in seq_len(N)) {
      xalr = alr(acomp(comp[i,])) %>% unclass()
      Amu = colMeans(alr(acomp(comp[-i,])))
      Acov = var(alr(acomp(comp[-i,])))
      k = ((N-1)*(N-1-D))/(((N-1)^2-1)*D)
      ai0 = (t(xalr-Amu) %*% solve(Acov) %*% (xalr-Amu)) * k
      ai[i,] = pf(ai0,D,N-D-1) %>% round(4)
    }
  } else {
    x = sample
    xalr = alr(acomp(x)) %>% unclass()
    ai0 = (t(xalr-Amu) %*% solve(Acov) %*% (xalr-Amu)) * k
    ai = pf(ai0,D,N-D) %>% round(4)
  }
  return(ai)
}

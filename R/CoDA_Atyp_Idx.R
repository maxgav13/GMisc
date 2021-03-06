#' @title CoDA Atypicality Index
#' @description Calculates the atypicality index (ranges between 0 and 1, 1 being more atypical/different) for all the observations of a composition or compares one sample to the given composition, using Compositional Data Analysis (CoDA) principles.
#' @param comp A matrix of observations for a given composition. Entries must be non-zero and positive.
#' @param sample A one sample vector to compare against the given composition.
#' @export
#' @return A matrix with the atypicality index or indices.
#' @references Aitchison, J. (1986). The statistical analysis of compositional data. Chapman and Hall.
#' @name CoDA_Atyp_Idx
#' @examples
#' data("Hongite", package = 'compositions')
#' samp = c(44, 20.4, 13.9, 9.1, 12.6)
#' CoDA_Atyp_Idx(Hongite)
#' CoDA_Atyp_Idx(Hongite, samp)
#'
CoDA_Atyp_Idx = function(comp, sample = NULL) {

  n1 = nrow(comp)
  A = compositions::alr(compositions::acomp(comp)) %>% unclass()
  Amu = colMeans(A)
  Acov = stats::var(A)*((n1-1)/n1)
  N = nrow(comp)
  D = ncol(A)
  k = (N*(N-D))/((N^2-1)*D)

  if (is.null(sample)) {
    ai = matrix(0,nrow = N,ncol = 1)
    for (i in seq_len(N)) {
      xalr = compositions::alr(compositions::acomp(comp[i,])) %>% unclass()
      Amu = colMeans(compositions::alr(compositions::acomp(comp[-i,])))
      Acov = stats::var(compositions::alr(compositions::acomp(comp[-i,])))
      k = ((N-1)*(N-1-D))/(((N-1)^2-1)*D)
      ai0 = (t(xalr-Amu) %*% solve(Acov) %*% (xalr-Amu)) * k
      ai[i,] = stats::pf(ai0,D,N-D-1) %>% round(4)
    }
  } else {
    x = sample
    xalr = compositions::alr(compositions::acomp(x)) %>% unclass()
    ai0 = (t(xalr-Amu) %*% solve(Acov) %*% (xalr-Amu)) * k
    ai = stats::pf(ai0,D,N-D) %>% round(4)
  }
  return(ai)
}

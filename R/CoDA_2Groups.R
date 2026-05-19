#' @title CoDA Two Group Hypotheses
#' @description Performs the calculations for the hypothesis of equal centers and covariance (\code{H1}), of equal covariance with different centers (\code{H2}), of equal centers with different covariance (\code{H3}), and all at once \code{All}, for two diferent samples, using Compositional Data Analysis (CoDA) principles.
#' @param comp1 A matrix or data frame of observations for composition 1. Entries must be non-zero and positive.
#' @param comp2 A matrix or data frame of observations for composition 2. Entries must be non-zero and positive.
#' @export
#' @return A tibble with the statistic (Q), degrees of freedom (nu), p-value, and null hypothesis (H0)
#' @references Pawlowsky-Hlahn, V., Egozcue, J.J & Tolosna-Delgado, R. (2015). Modeling and analysis of compositional data. John Wiley & Sons.
#' @references Aitchison, J. (1986). The statistical analysis of compositional data. Chapman and Hall.
#' @rdname CoDA_2Groups
#' @family compositional data analysis
#' @examples
#' data("Hongite", package = 'compositions')
#' data("Kongite", package = 'compositions')
#' CoDA_2Group_H1(Hongite,Kongite)
#'
CoDA_2Group_H1 = function(comp1, comp2) {
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
  Acov = stats::var(A2) * ((n1 - 1) / n1)
  Bcov = stats::var(B2) * ((n2 - 1) / n2)
  Covp = (n1 * Acov + n2 * Bcov) / (n1 + n2) # pooled covariance
  muc = (n1 * Amu + n2 * Bmu) / (n1 + n2) # combined mean vector
  Covc = Covp + (n1 * n2 * (Amu - Bmu) %*% t(Amu - Bmu)) / (n1 + n2)^2 # combined covariance

  # H1 - equal centers and covariance
  H0 = 'equal centers and covariance'
  Q1vsg = n1 * log(det(Covc) / det(Acov)) + n2 * log(det(Covc) / det(Bcov))
  nu1 = .5 * D * (D + 3)
  stats::qchisq(.95, nu1) # critical chisq value
  pval1 = stats::pchisq(Q1vsg, nu1, lower.tail = F) # p-value of the statistic

  res = tibble::tibble(Q = Q1vsg, nu = nu1, pval = pval1, H0 = H0)

  return(res)
}

#' @export
#' @rdname CoDA_2Groups
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
  Acov = stats::var(A2) * ((n1 - 1) / n1)
  Bcov = stats::var(B2) * ((n2 - 1) / n2)
  Covp = (n1 * Acov + n2 * Bcov) / (n1 + n2) # pooled covariance
  muc = (n1 * Amu + n2 * Bmu) / (n1 + n2) # combined mean vector
  Covc = Covp + (n1 * n2 * (Amu - Bmu) %*% t(Amu - Bmu)) / (n1 + n2)^2 # combined covariance

  # H2 - equal covariance with different centers
  H0 = 'equal covariance with different centers'
  Q2vsg = n1 * log(det(Covp) / det(Acov)) + n2 * log(det(Covp) / det(Bcov))
  nu2 = .5 * (D) * (D + 1)
  stats::qchisq(.95, nu2) # critical chisq value
  pval2 = stats::pchisq(Q2vsg, nu2, lower.tail = F) # p-value of the statistic

  res = tibble::tibble(Q = Q2vsg, nu = nu2, pval = pval2, H0 = H0)

  return(res)
}

#' @export
#' @rdname CoDA_2Groups
#' @examples
#' data("Hongite", package = 'compositions')
#' data("Kongite", package = 'compositions')
#' CoDA_2Group_H3(Hongite,Kongite)
#'
CoDA_2Group_H3 = function(comp1, comp2) {
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
  Acov = stats::var(A2) * ((n1 - 1) / n1)
  Bcov = stats::var(B2) * ((n2 - 1) / n2)
  Covp = (n1 * Acov + n2 * Bcov) / (n1 + n2) # pooled covariance
  muc = (n1 * Amu + n2 * Bmu) / (n1 + n2) # combined mean vector
  Covc = Covp + (n1 * n2 * (Amu - Bmu) %*% t(Amu - Bmu)) / (n1 + n2)^2 # combined covariance

  # H3 - equal centers with different covariance
  H0 = 'equal centers with different covariance'
  CovAh = Acov
  CovBh = Bcov

  FisherBehrens = function() {
    muhi = solve(n1 * solve(CovAh) + n2 * solve(CovBh)) %*%
      (n1 * solve(CovAh) %*% Amu + n2 * solve(CovBh) %*% Bmu)
    muc = (n1 * Amu + n2 * Bmu) / (n1 + n2)

    muhu = muhi
    repeat {
      CovAh = Acov + (Amu - muhu) %*% t(Amu - muhu)
      CovBh = Bcov + (Bmu - muhu) %*% t(Bmu - muhu)
      muh = solve(n1 * solve(CovAh) + n2 * solve(CovBh)) %*%
        (n1 * solve(CovAh) %*% Amu + n2 * solve(CovBh) %*% Bmu)
      # if (sum((muc-muh)^2/(muc^2)) < .95) return(list(muh=muh,CovAh=CovAh,CovBh=CovBh))
      if (diff(c(sum(abs(muc - muh)), sum(abs(muc - muhu)))) < 1e-6) {
        return(list(muh = muh, CovAh = CovAh, CovBh = CovBh))
      }
      muhu = muh
    }
  }

  res = FisherBehrens()
  muh = solve(n1 * solve(res$CovAh) + n2 * solve(res$CovBh)) %*%
    (n1 * solve(res$CovAh) %*% Amu + n2 * solve(res$CovBh) %*% Bmu)

  Q3vsg = n1 *
    log(det(res$CovAh) / det(Acov)) +
    n2 * log(det(res$CovBh) / det(Bcov))
  nu3 = D
  stats::qchisq(.95, nu3) # critical chisq value
  pval3 = stats::pchisq(Q3vsg, nu3, lower.tail = F) # p-value of the statistic

  res = tibble::tibble(Q = Q3vsg, nu = nu3, pval = pval3, H0 = H0)

  return(res)
}

#' @export
#' @rdname CoDA_2Groups
#' @examples
#' data("Hongite", package = 'compositions')
#' data("Kongite", package = 'compositions')
#' CoDA_2Group_All(Hongite,Kongite)
#'
CoDA_2Group_All = function(comp1, comp2) {
  res = dplyr::bind_rows(
    CoDA_2Group_H1(comp1, comp2),
    CoDA_2Group_H2(comp1, comp2),
    CoDA_2Group_H3(comp1, comp2)
  )

  return(res)
}

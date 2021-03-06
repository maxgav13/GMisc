#' @title Similarity Coefficient
#' @description Calculates the similarity coeficient between observations based on their features (columns). Coefficient ranges between 0 and 1, with values close to 1 indicating more similarity.
#' @param x A data frame of observations in rows and features (variables) in columns. Features must be non-zero and positive.
#' @param samp.names A character vector with the names of the observations (samples)
#' @param digits Number of digits to use
#' @export
#' @return A matrix of similarity coefficients
#' @references Borchardt, G.A. (1974). The SIMAN Coefficient for Similarity Analysis. Classification Society Bulletin, 3(2), 2–8.
#' @name similarity_coef
#' @examples
#' dat = iris[c(1,2,51,52,101,102),]
#' similarity_coef(dat)
#'
similarity_coef = function(x, samp.names = NULL, digits = 3) {

  if (is.null(samp.names)) {
    noms = row.names(x)
  } else {
    noms = samp.names
  }

  x = x %>%
    as.data.frame() %>%
    dplyr::select_if(is.numeric)
  n = nrow(x)
  res = NULL
  key_pairs <- expand.grid(noms, noms)

  for (i in 1:n^2) {
    res[i] = mean(unlist(1/(exp(abs(log(x[key_pairs[i,1],]/x[key_pairs[i,2],]))))))
  }

  simcoef = matrix(round(res, digits), nrow = n,
                   dimnames = list(noms,noms))

  return(simcoef)
}

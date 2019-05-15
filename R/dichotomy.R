#' @title Splits a vector into two classes
#' @description Splits (dichotomizes) a numeric vector into two classes using the "up-down" or "median" criteria. The results is intendend to be used in a runs test for tendency.
#' @param x A numeric vector of values to split
#' @param citeria Splitting criteria to use ("up-down" or "median")
#' @export
#' @return A vector with the splitted classes
#' @references Davis, J. C. (2002). Statistical and Data Analysis in Geology. 3rd ed. John Wiley & Sons.
#' @import stats
#' @examples
#' x1 = c(17, 14, 13, 17, 9, 15, 36, 13, 26, 10, 25, 13, 25, 8, 28, 15, 19, 32, 22)
#' x2 = c(17, 14, 14, 17, 9, 15, 36, 13, 26, 10, 25, 13, 25, 8, 28, 15, 19, 32, 22)
#' dichotomy(x1)
#' dichotomy(x2)
#' dichotomy(x2, criteria = "median")
#'
dichotomy = function(x, criteria = c("up-down", "median")) {

  dico = vector("numeric", length(x))

  if (any(criteria == "up-down")) {
    for (i in 1:(length(x)-1)) {
      if (x[i+1] == x[i] & x[i+2] > x[i]) {
        dico[i] = "+"
      } else if (x[i+1] > x[i]) {
        dico[i] = "+"
      } else {
        dico[i] = "-"
      }
    }
    dico = as.factor(dico[-length(x)])
  } else {
    for (i in seq_along(x)) {
      if (x[i] >= median(x)) {
        dico[i] = "+"
      }  else {
        dico[i] = "-"
      }
    }
    dico = as.factor(dico)
  }

  return(dico)
}

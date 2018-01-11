#' @title Inverse logit
#' @description Calculates the probability (0, 1) using the inverse logit function.
#' @param x A vector of continuous values
#' @export
#' @return A vector of probabilities
#' @import stats
#' @examples
#' x = seq(-3,3,.5)
#' inv_logit(x)
#'
inv_logit <- function (x){
  exp(x) / (1 + exp(x))
}

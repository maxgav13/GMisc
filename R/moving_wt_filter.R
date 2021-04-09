#' @title Moving weighted average filter
#' @description Filters data using moving weighted averages of diferent window sizes.
#' @param x A numeric vector with the position values of the data to filter (a sequence)
#' @param y A numeric vector with the data to filter
#' @param k A numeric vector with the widths of the windows to use for the filter (must be an odd number)
#' @export
#' @return A list with two tibbles: one with the filtered results plus the original data, and one with the residuals
#' @details The weigths for the window lengths are calculated in a way that farther observations from the central (estimated) value are given a lesser weight than observations near de central value
#' @references Swan, A. R. H. & Sandilands, M. (1995). Introduction to Geological Data Analysis. Blackwell Science.
#' @examples
#' data(nautilus)
#' head(nautilus)
#' k = c(5, 7, 9)
#' moving_wt_filter(nautilus$x, nautilus$y, k)
#'
moving_wt_filter = function(x, y, k) {
  d = x # vector de x
  v = y # vector de y

  W = list(k5 = c(-3,12,17,12,-3),
           k7 = c(-2,3,6,7,6,3,-2),
           k9 = c(-21,14,39,54,59,54,39,14,-21))

  k = k # ventana de filtrado

  tukey = function(x) {pmax(1 - x ^ 2, 0) ^ 2}
  Wf = list()
  for (i in 1:length(k)) {
    Wf[[i]] = tukey(seq(-floor(k[i]/2), floor(k[i]/2)) / (floor(k[i]/2) + 1))
    Wf[[i]] = Wf[[i]]/sum(Wf[[i]])
    }

  wadj = list()
  filt = list()
  finalfilt = list()
  wf = list()
  for (i in seq_along(k)) {
    # win = W[[i]]
    win = Wf[[i]]
    wadj[[i]] = c(rep(NA, floor(k[i]/2)), v, rep(NA, floor(k[i]/2))) # agrega NAs en los extremos para facilitar los calculos
    filt[[i]] = zoo::rollapply(wadj[[i]], k[i], function(x) weighted.mean(x, w = win, na.rm = TRUE), fill = NA) # media movil
    finalfilt[[i]] = filt[[i]][!is.na(filt[[i]])] # remueve NAs
    wf[[i]] = v - finalfilt[[i]] # calcula datos residuales
  }
  finalfiltdf = tibble::tibble(data.frame(x = d, y = v, cbind.data.frame(finalfilt)))
  names(finalfiltdf) = c("x","y", paste("k",k, sep = "_"))
  wfdf = tibble::tibble(data.frame(x = d, cbind.data.frame(wf)))
  names(wfdf) = c("x", paste("k",k, sep = "_"))
  return(list(Filtered = finalfiltdf, Residual = wfdf))
}

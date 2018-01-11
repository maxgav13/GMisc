#' @title Moving average filter
#' @description Filters data using moving averages of diferent window sizes.
#' @param x A numeric vector with the position values of the data to filter (a sequence)
#' @param y A numeric vector with the data to filter
#' @param k A numeric vector with the widths of the windows to use for the filter (must be an odd number)
#' @export
#' @return A list with two data frames: one with the filtered results plus the original data, and one with the residuals
#' @import stats
#' @references Swan, A. R. H. & Sandilands, M. (1995). Introduction to Geological Data Analysis. Blackwell Science.
#' @importFrom zoo rollapply
#' @examples
#' data(nautilus)
#' head(nautilus)
#' k = c(3, 5, 11, 25)
#' moving_avg_filter(nautilus$x, nautilus$y, k)
#'
moving_avg_filter = function(x, y, k) {
  d = x # vector de x
  w = y # vector de y

  k = k # ventana de filtrado
  wadj = list()
  filt = list()
  finalfilt = list()
  wf = list()
  for (i in seq_along(k)) {
    wadj[[i]] = c(rep(NA, floor(k[i]/2)), w, rep(NA, floor(k[i]/2))) # agrega NAs en los extremos para facilitar los calculos
    filt[[i]] = rollapply(wadj[[i]], k[i], mean, na.rm = TRUE, fill = NA) # media movil
    finalfilt[[i]] = filt[[i]][!is.na(filt[[i]])] # remueve NAs
    wf[[i]] = w - finalfilt[[i]] # calcula anomalia residual
  }
  finalfiltdf = data.frame(x = d, y = w, cbind.data.frame(finalfilt))
  names(finalfiltdf) = c("x","y", paste("k",k, sep = "_"))
  wfdf = data.frame(x = d, cbind.data.frame(wf))
  names(wfdf) = c("x", paste("k",k, sep = "_"))
  return(list(Filtered = finalfiltdf, Residual = wfdf))
}

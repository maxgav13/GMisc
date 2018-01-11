#' @title T-statistic for layer boundary determination
#' @description Calculates the T-statistic for a perforation log from a Dynamic Probing Machine (DPM), and finds the locations where this statistic is significant to 0.95 and 0.99. The coefficient is used to find layer boundaries in a perforation log.
#' @param x A data frame containing the depth of perforation in the first column, and the SPT values in the seconds column
#' @param k The window length for the number of data points to include in the calculation of T-statistic. Always and odd number
#' @export
#' @return A list of data frames: original data plus the T-statistic, locations where the coefficient is significant to 0.95 and 0.99, and one with the degrees of freedom and the corresponding critical T-values for the 0.95 and 0.99 signficance
#' @references Mora, R. (2013). Uso de metodos estadisticos para la identifacion de capas de suelos volcanicos con el ensayo del cono de pentracion en los terrenos de la Universidad de Costa Rica, Montes de Oca, San Jose, Costa Rica. - Rev. Geol. Amer. Central, 49: 109-120.
#' @import stats
#' @details The example data given is intended to show the structure needed for input data. The user should follow this structure, which in general corresponds with a data frame with a sequence in the first column and the observed/measured values in the second column
#' @examples
#' testT = T_stat(DPM_data, k = 7)
#'
T_stat = function(x, k = 7) {
  Data = x
  names(Data) = c("Prof", "Blows")
  w = Data$Blows
  n = floor(k/2)
  wadj = c(rep(NA,n), w, rep(NA,n))

  Tstat = NULL

  for (i in (n+1):(length(wadj)-(n))) {
    S1 = wadj[(i-n):(i-1)]
    S2 = wadj[(i+1):(i+n)]
    diff = mean(S1, na.rm = T) - mean(S2, na.rm = T)
    sp = sqrt(var(S1, na.rm = T)/n + var(S2, na.rm = T)/n)
    sp2 = 1 / (sqrt(n/(2*n-1) * var(S1, na.rm = T) + n/(2*n-1) * var(S2, na.rm = T))) * sqrt((n*n)/(2*n))
    Tstat[i] = abs(diff / sp)
  }

  Tstat = ifelse(Tstat == Inf, max(Tstat[Tstat != Inf],na.rm = T) * 1.2, Tstat)
  # if (any(Tstat == Inf)) {
  #   Tstat[which(Tstat == Inf)] = max(Tstat[Tstat != Inf],na.rm = T) * 1.2
  # }

  Tstat = ifelse(is.nan(Tstat), NA, Tstat)

  DF = data.frame(k, Tstat)
  DF = DF[c((n+1):(nrow(DF)-1),NA),]
  row.names(DF) = 1:nrow(DF)
  Data$Tstat = DF$Tstat

  df = 2 * n - 2
  t95 = qt(.975, df)
  t99 = qt(.995, df)
  Stats = data.frame(df = df, t95 = round(t95,3), t99 = round(t99,3))

  Over95 = subset(Data, Tstat >= t95)
  Over99 = subset(Data, Tstat >= t99)

  return(list(Data = Data, Over95 = Over95, Over99 = Over99, Stats = Stats))
}

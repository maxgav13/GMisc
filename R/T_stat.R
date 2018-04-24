#' @title T-statistic for layer boundary determination
#' @description Calculates the T-statistic and Cohen's d effect size for a perforation log. The coefficient is used to find layer boundaries in a perforation log.
#' @param x A data frame containing the depth of perforation in the first column, and the value of interest in the second column
#' @param k The window length for the number of data points to include in the calculation of T-statistic. Always and even (par) number
#' @export
#' @return A ggplot and plotly objects showing the Cohen's d statistic and lines marking the Cohen's U3 values for 0.95, 0.99, and 0.999
#' @references Mora, R. (2013). Uso de metodos estadisticos para la identifacion de capas de suelos volcanicos con el ensayo del cono de pentracion en los terrenos de la Universidad de Costa Rica, Montes de Oca, San Jose, Costa Rica. - Rev. Geol. Amer. Central, 49: 109-120.
#' @import stats
#' @import ggplot2
#' @details The example data given is intended to show the structure needed for input data. The user should follow this structure, which in general corresponds with a data frame with a sequence in the first column and the observed/measured values in the second column
#' @examples
#' T_stat(DPM_data, k = 6)
#'
T_stat = function(x, k = 6) {
  Data = x
  nombres = names(Data)
  w = Data[[2]]
  n = floor(k/2)
  wadj = c(rep(NA,n), w, rep(NA,n))

  Tstat = NULL
  d = NULL

  for (i in (n+1):(length(wadj)-(n))) {
    S1 = wadj[(i-(n-1)):(i)]
    S2 = wadj[(i+1):(i+n)]
    diff = mean(S1, na.rm = T) - mean(S2, na.rm = T)
    sp = sqrt(var(S1, na.rm = T)/n + var(S2, na.rm = T)/n)
    spd = sqrt((var(S1, na.rm = T)*(n-1) + var(S2, na.rm = T)*(n-1))/(n + n - 2))
    Tstat[i] = abs(diff / sp)
    d[i] = round(abs(diff / spd),2)
  }

  Tstat = ifelse(Tstat == Inf, max(Tstat[Tstat != Inf],na.rm = T) * 1.2, Tstat)
  d = ifelse(d == Inf, max(d[d != Inf],na.rm = T) * 1.2, d)
  # if (any(Tstat == Inf)) {
  #   Tstat[which(Tstat == Inf)] = max(Tstat[Tstat != Inf],na.rm = T) * 1.2
  # }

  Tstat = ifelse(is.nan(Tstat), NA, Tstat)

  DF = data.frame(k, Tstat, d)
  DF = DF[c((n+1):nrow(DF)),]
  row.names(DF) = 1:nrow(DF)
  Data$Tstat = DF$Tstat
  Data$d = DF$d

  df = 2 * n - 2
  # d_crit = c(2.3, 2.88, 3.29, 3.92)
  d_crit = qnorm(c(.95,.99,.999))

  q = ggplot(Data, aes_string("d", nombres[1])) +
    geom_path(na.rm = T) +
    geom_vline(xintercept = d_crit, col = c("blue", "orange", "red")) +
    scale_y_reverse(name = "Depth (m)") +
    scale_x_continuous(name = "Cohen's d") +
    theme_bw()
  p = plotly::ggplotly(ggplot(Data, aes_string("d", nombres[1])) +
                         geom_path(size = 0.25, na.rm = T) +
                         geom_vline(size = 0.25, xintercept = d_crit, col = c("blue", "orange", "red")) +
                         scale_y_reverse(name = "Depth (m)") +
                         scale_x_continuous(name = "Cohen's d") +
                         theme_bw())

  return(list(GGPLOT=q, PLOTLY=p))
}

#' @title Hotelling T2-statistic for layer boundary determination
#' @description Calculates the T2-statistic and corresponding effect size (Mahalanobis distance) for a perforation log with more than 1 variable. The coefficient is used to find layer boundaries in a perforation log.
#' @param x A data frame containing the depth of perforation in the first column, and the variables of interest in the rest of the columns, for a CPTu test: point resistance (qc), sleeve friction (fs), and pore-water pressure (u)
#' @param k The window length for the number of data points to include in the calculation of T2-statistic. Always and even (par) number
#' @export
#' @return A ggplot and plotly objects showing the Mahalanobis D2 statistic and lines marking the critical values at 0.95, 0.99, and 0.999
#' @references Davis, J. C. (2002). Statistical and Data Analysis in Geology. 3rd ed. John Wiley & Sons.
#' @import stats
#' @import ggplot2
#' @details The example data given is intended to show the structure needed for input data. The user should follow this structure, which in general corresponds with a data frame with a sequence in the first column and the observed/measured values in the second, third, and fourth columns
#' @examples
#' T2_stat(CPTu_data, k = 50)
#'
T2_stat = function(x, k = 50) {
  Data = x
  v = ncol(Data)
  nombres = names(Data)
  w = Data[,-1]
  n = floor(k/2)
  n.2 = floor(k/4)
  m = v - 1
  NADF = as.data.frame(matrix(NA, ncol = m, nrow = n.2))
  names(NADF) = names(w)
  wadj = rbind.data.frame(NADF, w, NADF)

  T2stat = NULL
  f = NULL
  D2 = NULL

  for (i in floor(n+n.2/2):(nrow(wadj)-floor(n+n.2/2))) {
    s1 = wadj[(i-(n-1)):(i),]
    s2 = wadj[(i+1):(i+n),]
    S1 = cov(s1, use = "na.or.complete")
    S2 = cov(s2, use = "na.or.complete")
    Sp = ((n - 1) * S1 + (n - 1) * S2) / (n + n - 2)
    D = colMeans(s1, na.rm = T) - colMeans(s2, na.rm = T)
    D2[i] = round(as.numeric(D %*% solve(Sp) %*% D),2)
    T2stat[i] = (n * n) / (n + n) * D2[i]
    f[i] = (n + n - m - 1) / ((n + n - 2) * m) * T2stat[i]
  }

  DF1 = na.omit(data.frame(k, T2stat, D2, f))
  if (n.2 %% 2 == 0) {
    NADF2.a = data.frame(rep(NA,floor(n-n.2/2-1)),rep(NA,floor(n-n.2/2-1)),rep(NA,floor(n-n.2/2-1)),rep(NA,floor(n-n.2/2-1)))
  } else {
    NADF2.a = data.frame(rep(NA,floor(n-n.2/2-1)),rep(NA,floor(n-n.2/2-1)),rep(NA,floor(n-n.2/2-1)),rep(NA,floor(n-n.2/2-1)))
  }
    NADF2.b = data.frame(rep(NA,floor(n-n.2/2)),rep(NA,floor(n-n.2/2)),rep(NA,floor(n-n.2/2)),rep(NA,floor(n-n.2/2)))
  names(NADF2.a) = names(DF1); names(NADF2.b) = names(DF1)
  DF = rbind.data.frame(NADF2.a, DF1, NADF2.b)
  row.names(DF) = 1:nrow(DF)
  Data$T2stat = DF$T2stat
  Data$D2 = DF$D2
  Data$f = DF$f

  df1 = m
  df2 = n + n - m - 1
  chi = qchisq(c(0.95, 0.99, 0.999), df1)
  Stats = data.frame(k = k, df1 = df1, df2 = df2)

  q = ggplot(Data, aes_string("D2", nombres[1])) +
    geom_path(na.rm = T) +
    geom_vline(xintercept = chi, col = c("blue", "orange", "red")) +
    scale_y_reverse(name = "Depth (m)") +
    scale_x_continuous(name = "Mahalanobis D2") +
    theme_bw()
  p = plotly::ggplotly(ggplot(Data, aes_string("D2", nombres[1])) +
                         geom_path(size = 0.25, na.rm = T) +
                         geom_vline(size = 0.25, xintercept = chi, col = c("blue", "orange", "red")) +
                         scale_y_reverse(name = "Depth (m)") +
                         scale_x_continuous(name = "Mahalanobis D2") +
                         theme_bw())

  return(list(GGPLOT=q, PLOTLY=p))
}

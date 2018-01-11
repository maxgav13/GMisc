#' @title Hotelling T2-statistic for layer boundary determination
#' @description Calculates the T2-statistic for a perforation log from a Cone Penetration Test (CPTu), and finds the locations where this statistic is significant to 0.95, 0.99, and 0.995. The coefficient is used to find layer boundaries in a perforation log.
#' @param x A data frame containing the depth of perforation in the first column, and the variables of interest in the rest of the columns: point resistance (qc), sleeve friction (fs), and pore-water pressure (u)
#' @param k The window length for the number of data points to include in the calculation of T2-statistic. Always and odd number
#' @export
#' @return A list of data frames: original data plus the T2-statistic, locations where the coefficient is significant to 0.95, 0.99, and 0.995, and one with the degrees of freedom and the corresponding critical f-values for the 0.95, 0.99, and 0.995 signficance
#' @references Davis, J. C. (2002). Statistical and Data Analysis in Geology. 3rd ed. John Wiley & Sons.
#' @import stats
#' @details The example data given is intended to show the structure needed for input data. The user should follow this structure, which in general corresponds with a data frame with a sequence in the first column and the observed/measured values in the second, third, and fourth columns
#' @examples
#' testT2 = T2_stat(CPTu_data, k = 51)
#'
T2_stat = function(x, k = 51) {
  Data = x
  v = ncol(Data)
  names(Data) = c("Prof", "qc", "fs", "u")
  w = Data[,2:v]
  n = floor(k/2)
  n.2 = floor(k/4)
  m = v - 1
  NADF = data.frame(rep(NA,n.2),rep(NA,n.2),rep(NA,n.2))
  names(NADF) = names(w)
  wadj = rbind.data.frame(NADF, w, NADF)

  T2stat = NULL
  f = NULL
  D2 = NULL

  for (i in (n+n.2/2):(nrow(wadj)-(n+n.2/2))) {
    s1 = wadj[(i-n):(i-1),]
    s2 = wadj[(i+1):(i+n),]
    S1 = cov(s1, use = "na.or.complete")
    S2 = cov(s2, use = "na.or.complete")
    Sp = ((n - 1) * S1 + (n - 1) * S2) / (n + n - 2)
    D = colMeans(s1, na.rm = T) - colMeans(s2, na.rm = T)
    D2[i] = as.numeric(D %*% solve(Sp) %*% D)
    T2stat[i] = (n * n) / (n + n) * D2[i]
    f[i] = (n + n - m - 1) / ((n + n - 2) * m) * T2stat[i]
  }

  DF1 = na.omit(data.frame(k, T2stat, D2, f))
  if (n.2 %% 2 == 0) {
    NADF2.a = data.frame(rep(NA,n-n.2/2-1),rep(NA,n-n.2/2-1),rep(NA,n-n.2/2-1),rep(NA,n-n.2/2-1))
  } else {
    NADF2.a = data.frame(rep(NA,n-n.2/2),rep(NA,n-n.2/2),rep(NA,n-n.2/2),rep(NA,n-n.2/2))
  }
    NADF2.b = data.frame(rep(NA,n-n.2/2),rep(NA,n-n.2/2),rep(NA,n-n.2/2),rep(NA,n-n.2/2))
  names(NADF2.a) = names(DF1); names(NADF2.b) = names(DF1)
  DF = rbind.data.frame(NADF2.a, DF1, NADF2.b)
  row.names(DF) = 1:nrow(DF)
  Data$T2stat = DF$T2stat
  Data$D2 = DF$D2
  Data$f = DF$f

  df1 = m
  df2 = n + n - m - 1
  f95 = qf(.05, df1, df2, lower.tail = F)
  f99 = qf(.01, df1, df2, lower.tail = F)
  f99.5 = qf(.005, df1, df2, lower.tail = F)
  f99.9 = qf(.001, df1, df2, lower.tail = F)
  Stats = data.frame(k = k, df1 = df1, df2 = df2, f95 = round(f95,3), f99 = round(f99,3), f99.5 = round(f99.5,3), f99.9 = round(f99.9,3))

  Over95 = subset(Data, f >= f95)
  Over99 = subset(Data, f >= f99)
  Over99.5 = subset(Data, f >= f99.5)
  Over99.9 = subset(Data, f >= f99.9)

  return(list(Data = Data, Over95 = Over95, Over99 = Over99, Over99.5 = Over99.5, Over99.9 = Over99.9, Stats = Stats))
}

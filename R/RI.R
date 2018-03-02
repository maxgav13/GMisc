#' @title Intraclass Correlation Coefficient for layer boundary determination
#' @description Calculates the intraclass correlation coefficient (RI) for a perforation log, and shows the locations where this coefficient is higher than 0.7 and 0.8. The coefficient is used to find layer boundaries in a perforation log.
#' @param x A data frame containing the depth of perforation in the first column, and the value of interest in the second column
#' @param k The window length for the number of data points to include in the calculation of RI. Always and odd number
#' @export
#' @return A ggplot object showing the RI statistic and lines marking the critical values of 0.7 and 0.8
#' @references Mora, R. (2013). Uso de metodos estadisticos para la determinacion de capas homogeneas de suelos volcanicos en un sitio de las laderas del Volcan Irazu, Cartago, Costa Rica. - Rev. Geol. Amer. Central, 49: 101-108.
#' @import stats
#' @details The example data given is intended to show the structure needed for input data. The user should follow this structure, which in general corresponds with a data frame with a sequence in the first column and the observed/measured values in the second column
#' @examples
#' testRI = RI(DPM_data, k = 7)
#'
RI = function(x, k = 7) {
  Data = x
  nombres = names(Data)
  w = Data[[2]]
  n = floor(k/2)
  wadj = c(rep(NA,n), w, rep(NA,n))

  RI = NULL

  for (i in (n+1):(length(wadj)-n)) {
    S1 = var(wadj[(i-n):(i-1)], na.rm = TRUE)
    S2 = var(wadj[(i+1):(i+n)], na.rm = TRUE)
    SC = (n * (S1 + S2)) / (2 * n - 1)
    SB = var(wadj[(i-n):(i+n)], na.rm = TRUE)
    RI[i] = SB / (SB + SC)
  }
  DF = data.frame(k, RI)
  DF = DF[c((n+1):(nrow(DF)-1),NA),]
  row.names(DF) = 1:nrow(DF)
  Data$RI = DF$RI

  q = ggplot(Data, aes_string(nombres[1], "RI")) +
    geom_line(na.rm = T) +
    geom_hline(yintercept = c(.7,.8), col = c("blue", "red")) +
    coord_flip() + scale_x_reverse(name = "Depth [m]") +
    scale_y_continuous(name = "RI") +
    theme_bw()

  return(q)
}

#' Piper Data Preparation
#' @description It transforms hydrochemical data (cations and anions) so they can be plotted on \code{piper_diagram()}.
#'
#' @param data A dataframe with cations and anions, with the following names: Ca, Mg, Na, K, Cl, SO4, CO3, HCO3
#'
#' @return A dataframe with \code{x} and \code{y} coordinates and the rest of the original data
#' @export
#'
#' @examples
#'
#' d = data.frame(Group = c('A','A','B','B'),
#'                Ca = c(120,150,110,52.6),
#'                Mg = c(78,160,110,28),
#'                Na = c(210,590,340,51.6),
#'                K = c(4.2,2,3.6,2.3),
#'                HCO3 = c(181,181,189,151),
#'                CO3 = 0,
#'                Cl = c(220,744,476,72.2),
#'                SO4 = c(560,1020,584,126))
#'
#' piper_data_prep(d)
#'
piper_data_prep <- function(data) {

  if (!('ID' %in% names(data))) {
    data$ID = 1:nrow(data)
  }

  toPercent <- function(d) {
    d$Ca <- d$Ca/20
    d$Mg <- d$Mg/12
    d$Na <- d$Na/23
    d$K <- d$K/39
    d$Cl <- d$Cl/35
    d$SO4 <- d$SO4/48
    d$CO3 <- d$CO3/30
    d$HCO3 <- d$HCO3/61

    totalCations <- d$Ca + d$Mg + d$Na + d$K
    d$Ca <- 100 * (d$Ca/totalCations)
    d$Mg <- 100 * (d$Mg/totalCations)
    d$Na <- 100 * (d$Na/totalCations)
    d$K <- 100 * (d$K/totalCations)
    totalAnions <- d$Cl + d$SO4 + d$CO3 + d$HCO3
    d$Cl <- 100 * (d$Cl/totalAnions)
    d$SO4 <- 100 * (d$SO4/totalAnions)
    d$CO3 <- 100 * (d$CO3/totalAnions)
    d$HCO3 <- 100 * (d$HCO3/totalAnions)
    return(d)
  }

  data2 = toPercent(data)

  Mg = data2$Mg
  Ca = data2$Ca
  SO4 = data2$SO4
  Cl = data2$Cl

  name = rep(1:length(Mg),3)

  y1 <- Mg * 0.86603
  x1 <- 100*(1-(Ca/100) - (Mg/200))
  y2 <- SO4 * 0.86603
  x2 <- 120+(100*Cl/100 + 0.5 * 100*SO4/100)

  new_point <- function(x1, x2, y1, y2, grad=1.73206){
    b1 <- y1-(grad*x1)
    b2 <- y2-(-grad*x2)
    M <- matrix(c(grad, -grad, -1,-1), ncol=2)
    intercepts <- as.matrix(c(b1,b2))
    t_mat <- -solve(M) %*% intercepts
    data.frame(x=t_mat[1,1], y=t_mat[2,1])
  }
  np_list <- lapply(1:length(x1), function(i) new_point(x1[i], x2[i], y1[i], y2[i]))

  npoints <- do.call("rbind",np_list)

  data.frame(ID=data2$ID,
             x=c(x1, x2, npoints$x),
             y=c(y=y1, y2, npoints$y)) %>%
    dplyr::as_tibble() %>%
    dplyr::left_join(data, by = 'ID') %>%
    dplyr::mutate(ID = forcats::as_factor(ID)) %>%
    dplyr::mutate(across(where(is.character), forcats::as_factor))
}

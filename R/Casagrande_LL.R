#' @title Casagrande (flow curve) liquid limit test
#'
#' @description Fits a linear regression of water content vs log10(blows) from a Casagrande cup liquid limit test, estimates the liquid limit (LL) at N = 25 blows, and optionally computes plasticity index (PI), liquidity index (LI), and consistency index (CI).
#'
#' @param N numeric vector. Number of blows recorded for each trial.
#' @param W numeric vector. Water content (percent) corresponding to each value in \code{N}.
#' @param pl numeric or NULL. Plastic limit (percent) of the soil. If provided, plasticity index (PI) is computed. Default \code{NULL}.
#' @param w numeric or NULL. Natural water content (percent) of the soil. Required together with \code{pl} to compute liquidity index (LI) and consistency index (CI). Default \code{NULL}.
#' @param lang character. Label language; \code{'es'} for Spanish or \code{'en'} for English. Default \code{'en'}.
#'
#' @references Holtz, R. D., Kovacs, W. D. & Sheahan, T. C. (2011). An Introduction to Geotechnical Engineering. Prentice Hall.
#'
#' @details The function fits a model and predicts the liquid limit as the water content at N = 25.
#'
#' Returned list contents depend on which optional arguments are supplied:
#' \itemize{
#'   \item \code{pl = NULL, w = NULL}: \code{plot}, \code{ll}.
#'   \item \code{pl} supplied, \code{w = NULL}: \code{plot}, \code{ll}, \code{pl}, \code{pi}.
#'   \item Both \code{pl} and \code{w} supplied: \code{plot}, \code{ll}, \code{pl}, \code{pi}, \code{li}, \code{ci}.
#' }
#'
#' @return A named list containing a ggplot2 object (\code{plot}) with the flow curve and dashed reference lines, plus numeric scalars for the computed Atterberg indices as described in Details.
#'
#'
#' @examples
#' # Liquid limit only
#' Casagrande_LL(N = c(15, 20, 28, 35), W = c(42, 38, 34, 31))
#'
#' # With plastic limit
#' Casagrande_LL(N = c(15, 20, 28, 35), W = c(42, 38, 34, 31), pl = 22, lang = 'es')
#'
#' # With plastic limit and natural water content
#' Casagrande_LL(N = c(15, 20, 28, 35), W = c(42, 38, 34, 31), pl = 22, w = 30)
#'
#' @export
#'
#' @family soil mechanics
#'
Casagrande_LL = function(N, W, pl = NULL, w = NULL, lang = 'en') {
  x.lab = ifelse(lang == 'es', 'N\u00famero de golpes, N', 'Blows, N')
  y.lab = ifelse(
    lang == 'es',
    'Contenido de humedad, w (%)',
    'Water content, w (%)'
  )

  casagrande_cup = data.frame(N = N, w = W)

  x.max.cc = (max(casagrande_cup$N) %/% 10 + 2) * 10

  cc.mod = stats::lm(w ~ log10(N), data = casagrande_cup)
  cc.mod.r2 = round(summary(cc.mod)$r.squared, 3)
  ll_cc = round(stats::predict(cc.mod, newdata = data.frame(N = 25)), 2)[[1]]

  if (!is.null(pl) & is.null(w)) {
    pl_cc = pl
    pi_cc = (ll_cc - pl_cc) %>% round(2)
    ei_cc = (-73.52 + .392 * 97 + 1.015 * ll_cc + .976 * pi_cc) %>% round(1)
  } else if (!is.null(pl) & !is.null(w)) {
    pl_cc = pl
    pi_cc = (ll_cc - pl_cc) %>% round(2)
    li_cc = ((w - pl_cc) / pi_cc) %>% round(2)
    ci_cc = ((ll_cc - w) / pi_cc) %>% round(2)
  }

  llcc.data = tibble::tibble(
    x1 = c(25, 25),
    x2 = c(25, 0),
    y1 = c(min(casagrande_cup$w), ll_cc),
    y2 = c(ll_cc, ll_cc)
  )

  ll_gg = ggplot2::ggplot(casagrande_cup, ggplot2::aes(.data$N, .data$w)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_x_continuous(
      breaks = seq(10, x.max.cc, 10),
      minor_breaks = 10:x.max.cc,
      limits = c(10, x.max.cc),
      trans = 'log10',
      expand = c(0, 0)
    ) +
    ggplot2::geom_smooth(method = 'lm', se = F) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$x1,
        y = .data$y1,
        xend = .data$x2,
        yend = .data$y2
      ),
      llcc.data,
      arrow = ggplot2::arrow(length = ggplot2::unit(0.02, "npc")),
      col = 'red'
    ) +
    ggplot2::geom_label(
      ggplot2::aes(label = .data$ll_cc),
      tibble::tibble(N = 15, w = ll_cc, ll_cc = paste0('LL=', ll_cc, '%')),
      size = 3
    ) +
    ggplot2::labs(x = x.lab, y = y.lab, caption = bquote(R^2 == .(cc.mod.r2))) +
    ggplot2::theme_bw(base_size = 14)

  if (!is.null(pl) & is.null(w)) {
    return(list(plot = ll_gg, ll = ll_cc, pl = pl, pi = pi_cc, ei = ei_cc))
  } else if (!is.null(pl) & !is.null(w)) {
    return(list(
      plot = ll_gg,
      ll = ll_cc,
      pl = pl,
      pi = pi_cc,
      li = li_cc,
      ci = ci_cc
    ))
  } else if (is.null(pl) & is.null(w)) {
    return(list(plot = ll_gg, ll = ll_cc))
  }
}

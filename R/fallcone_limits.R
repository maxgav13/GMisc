#' @title Fall cone liquid limit and plastic limit test
#'
#' @description Fits a linear regression of log10(water content) vs log10(penetration) from a fall cone test, estimates the desired limit (LL or PL) at 20 mm penetration and, if asked for both the plastic limit (PL) at 2 mm penetration.
#'
#' @param P numeric vector. Penetration values (mm).
#' @param W numeric vector. Water content (percent) corresponding to each value in \code{P}.
#' @param limit character. Limit to compute; either \code{'l'} for liquid limit (LL), \code{'p'} for plastic limit (PL), or \code{'b'} for both. Default is \code{'l'}.
#' @param lang character. Label language; \code{'es'} for Spanish or \code{'en'} for English. Default \code{'en'}.
#'
#' @references Budhu, M. (2011). Soil mechanics and foundations (3rd ed). Wiley.
#' @references Das, B. M., & Sobhan, K. (2018). Principles of geotechnical engineering (Ninth edition). Cengage Learning.
#'
#' @details
#' The liquid limit (LL) is estimated at 20 mm penetration using a cone of 80g, while the plastic limit (PL) is estimated at 20 mm penetration for a cone of 240 g. If both limits are calculated the the plastic limit (PL) is estimated at 2 mm penetration. The function also computes the plasticity index (PI) as the difference between LL and PL when both limits are computed. The results are visualized in a log-log plot of water content vs penetration, with the fitted regression line and reference lines for the computed limits.
#'
#'
#' @returns A named list containing a ggplot2 object (\code{plot}) with the fall cone test results and dashed reference line(s), plus numeric scalars for the computed limit and if both limits are computed also the plasticity index is retuned.
#'
#' @export
#'
#' @family soil mechanics
#'
#' @examples
#' fallcone_limits(P = c(5.5, 7.8, 14.8, 22, 32),
#'                 W = c(39, 44.8, 52.5, 60.3, 67))
#'
#' fallcone_limits(P = c(5, 9, 15, 22),
#'                 W = c(10, 20, 33, 48),
#'                 limit = 'p',
#'                 lang = 'es')
#'
fallcone_limits = function(P, W, limit = 'l', lang = 'en') {
  x.lab = ifelse(lang == 'es', 'Penetraci\u00f3n (mm)', 'Penetration (mm)')
  y.lab = ifelse(
    lang == 'es',
    'Contenido de humedad, w (%)',
    'Water content, w (%)'
  )

  fall_cone = data.frame(p = P, w = W)

  y.max.fc = (max(fall_cone$w) %/% 10 + 2) * 10
  y.by = dplyr::if_else(y.max.fc > 100, 20, 10)
  x.max.fc = (max(fall_cone$p) %/% 10 + 2) * 10

  fc.mod = stats::lm(log10(w) ~ log10(p), fall_cone)
  fc.mod.r2 = round(summary(fc.mod)$r.squared, 3)
  ll_fc = round(10^(stats::predict(fc.mod, data.frame(p = 20))), 2)[[1]]

  C = 10^(stats::coef(fc.mod)[[1]])
  m = stats::coef(fc.mod)[[2]]
  pl_fc = (C * (2)^m) %>% round(2)

  if (limit == 'b') {
    # y.min.fc = ifelse(pl_fc %/% 10 - 1 < 0, 1, (pl_fc %/% 10 - 1) * 10)
    y.min.fc = pl_fc - 5
  } else if (limit == 'l') {
    y.min.fc = (min(fall_cone$w) %/% 10 - 1) * 10
  } else if (limit == 'p') {
    y.min.fc = max(1, (min(fall_cone$w) %/% 10 - 1) * 10)
  }

  pi_fc = (ll_fc - pl_fc) %>% round(2)

  # if (!is.null(w)) {
  #   li_fc = ((w - pl_fc) / pi_fc) %>% round(2)
  #   ci_fc = ((ll_fc - w) / pi_fc) %>% round(2)
  # }

  llfc.data = tibble::tibble(
    x1 = c(20, 20),
    x2 = c(20, 0),
    y1 = c(y.min.fc, ll_fc),
    y2 = c(ll_fc, ll_fc)
  )
  plfc.data = tibble::tibble(
    x1 = c(2, 2),
    x2 = c(2, 0),
    y1 = c(y.min.fc, pl_fc),
    y2 = c(pl_fc, pl_fc)
  )

  base_plot = ggplot2::ggplot(fall_cone, ggplot2::aes(.data$p, .data$w)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_x_continuous(
      # breaks = c(1, seq(10, x.max.fc, 10)),
      # minor_breaks = 1:20,
      # limits = c(1, x.max.fc),
      expand = ggplot2::expansion(mult = c(0.05, 0.1)),
      trans = 'log10'
    ) +
    ggplot2::scale_y_continuous(
      # breaks = c(1, seq(10, y.max.fc, 10)),
      limits = c(y.min.fc, y.max.fc),
      expand = ggplot2::expansion(mult = c(0, 0.1)),
      trans = 'log10'
    ) +
    ggplot2::geom_abline(
      slope = m,
      intercept = log10(C),
      col = 'blue',
      linewidth = 1
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$x1,
        y = .data$y1,
        xend = .data$x2,
        yend = .data$y2
      ),
      llfc.data,
      arrow = ggplot2::arrow(length = ggplot2::unit(0.02, "npc")),
      col = 'red'
    ) +
    ggplot2::labs(x = x.lab, y = y.lab, caption = bquote(R^2 == .(fc.mod.r2))) +
    ggplot2::theme_bw(base_size = 14)

  if (limit == 'l') {
    plot_gg = base_plot +
      ggplot2::geom_label(
        ggplot2::aes(label = .data$ll_fc),
        tibble::tibble(
          p = mean(fall_cone$p[1:3]),
          w = ll_fc,
          ll_fc = paste0('LL=', ll_fc, '%')
        ),
        size = 3
      )
  } else if (limit == 'p') {
    plot_gg = base_plot +
      ggplot2::geom_label(
        ggplot2::aes(label = .data$ll_fc),
        tibble::tibble(
          p = mean(fall_cone$p[1:3]),
          w = ll_fc,
          ll_fc = paste0('PL=', ll_fc, '%')
        ),
        size = 3
      )
  } else if (limit == 'b') {
    plot_gg = base_plot +
      ggplot2::geom_label(
        ggplot2::aes(label = .data$ll_fc),
        tibble::tibble(
          p = mean(fall_cone$p[1:3]),
          w = ll_fc,
          ll_fc = paste0('LL=', ll_fc, '%')
        ),
        size = 3
      ) +
      ggplot2::geom_segment(
        ggplot2::aes(
          x = .data$x1,
          y = .data$y1,
          xend = .data$x2,
          yend = .data$y2
        ),
        plfc.data,
        arrow = ggplot2::arrow(length = ggplot2::unit(0.02, "npc")),
        col = 'red'
      ) +
      ggplot2::geom_label(
        ggplot2::aes(label = .data$pl_fc),
        tibble::tibble(
          p = 2,
          w = pl_fc,
          pl_fc = paste0('PL=', pl_fc, '%')
        ),
        hjust = -0.05,
        size = 3
      )
  }

  if (limit == 'l') {
    return(list(plot = plot_gg, ll = ll_fc))
  } else if (limit == 'p') {
    return(list(plot = plot_gg, pl = ll_fc))
  } else if (limit == 'b') {
    return(list(plot = plot_gg, ll = ll_fc, pl = pl_fc, pi = pi_fc))
  }
}

#' @title Casagrande plasticity chart (LL vs PI)
#'
#' @description Create the Casagrande plasticity chart plotting Liquid Limit (LL) vs Plasticity Index (PI). The plot shows the A-line and U-line, a polygon representing typical classification zones,
#' and labels for standard regions used in soil plasticity interpretation.
#'
#' @param ll.max numeric. Maximum value for the liquid limit (LL) axis (x-axis). Default 110.
#' @param pi.max numeric. Maximum value for the plasticity index (PI) axis (y-axis). Default 90.
#' @param lang character. Label language; \code{'es'} for Spanish or
#'   \code{'en'} for English. Default \code{'en'}.
#'
#' @references Holtz, R. D., Kovacs, W. D. & Sheahan, T. C. (2011). An Introduction to Geotechnical Engineering. Prentice Hall.
#'
#' @details The function computes the A-line and U-line values for LL vs PI and draws a polygon approximating the CL-ML zone. It returns a ggplot2 object so the caller can further customize the plot (titles, themes, annotations) or save it.
#'
#' @family classification plots, soil mechanics
#'
#' @return A ggplot2 object (class 'gg' and 'ggplot').
#'
#' @examples
#' Casagrande_chart()
#' Casagrande_chart(ll.max = 80, pi.max = 60) + ggplot2::ggtitle('Casagrande chart')
#' soil_df = data.frame(ll = c(67, 58, 77, 51),
#'                      pi = c(39, 19, 22, 20),
#'                      sample = paste0('M', 1:4))
#' Casagrande_chart(lang = 'es') +
#' ggplot2::geom_point(ggplot2::aes(ll, pi, shape = sample, color = sample), soil_df, size = 3) +
#' ggplot2::scale_shape_manual(values = 1:4)
#'
#' @export
#'
Casagrande_chart = function(ll.max = 110, pi.max = 90, lang = 'en') {
  casagrande =
    tibble::tibble(
      ll = seq(0, ll.max, 1),
      A = dplyr::case_when(ll >= 25.5 ~ .73 * (ll - 20), .default = NA),
      U = dplyr::case_when(ll >= 16 ~ .9 * (ll - 8), .default = NA)
    ) %>%
    tidyr::pivot_longer(
      cols = c(.data$A, .data$U),
      names_to = 'Linea',
      values_to = 'pi'
    )

  limites_etiquetas = tibble::tibble(
    ll = c(10, 42.5, 60, 80, 100),
    pi = 5,
    label = c('L', 'M', 'H', 'V', 'E')
  )

  cl_ml = tibble::tibble(ll = c(16, 16, 29.3, 25.4, 12), pi = c(4, 7, 7, 4, 4))

  lab.x = ifelse(
    lang == 'es',
    'L\u00edmite l\u00edquido, LL',
    'Liquid limit, LL'
  )
  lab.y = ifelse(
    lang == 'es',
    '\u00cdndice de plasticidad, IP',
    'Plasticity index, PI'
  )

  gg_plasticidad =
    ggplot2::ggplot() +
    ggplot2::geom_polygon(ggplot2::aes(.data$ll, .data$pi), cl_ml, alpha = .2) +
    ggplot2::geom_line(
      ggplot2::aes(.data$ll, .data$pi, lty = .data$Linea),
      casagrande,
      show.legend = F
    ) +
    ggplot2::geom_vline(
      xintercept = c(35, 50, 70, 90),
      lty = 3,
      linewidth = .75
    ) +
    ggplot2::geom_label(
      ggplot2::aes(.data$ll, .data$pi, label = .data$label),
      limites_etiquetas,
      size = 3
    ) +
    ggplot2::coord_cartesian(
      xlim = c(0, ll.max),
      ylim = c(0, pi.max),
      expand = F
    ) +
    ggplot2::scale_linetype_manual(values = c(1, 5)) +
    ggplot2::scale_x_continuous(breaks = seq(0, ll.max, 20)) +
    ggplot2::scale_y_continuous(n.breaks = 10) +
    ggplot2::labs(x = lab.x, y = lab.y) +
    ggplot2::theme_bw(base_size = 14)

  return(gg_plasticidad)
}

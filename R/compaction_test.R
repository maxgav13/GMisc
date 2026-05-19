#' @title Proctor Compaction Test Analysis
#' @description
#' Analyses results from a standard or modified Proctor compaction test.
#' Fits a LOESS curve to find optimum moisture content and maximum dry unit
#' weight, builds zero-air-void (ZAV) lines for selected saturation levels,
#' and returns summary statistics plus two diagnostic plots.
#'
#' @param w Numeric vector. Water content values (percent) measured in the test.
#' @param gamma Numeric vector. Moist unit weight values (kN/m³) corresponding
#'   to each `w` value. Must be the same length as `w`.
#' @param Gs Numeric scalar. Specific gravity of soil solids. Default is `2.68`.
#' @param lang Character scalar. Language for plot labels. Either `"en"` (default)
#'   for English or `"es"` for Spanish.
#' @param S_lines Numeric vector. Saturation levels (percent) for which ZAV lines are
#'   drawn on the compaction curve plot. Default is `c(70, 80, 90, 100)`.
#' @param target_compaction Numeric scalar. Relative compaction target as a
#'   proportion (e.g. `0.95` for 95 percent). Used to draw the reference horizontal
#'   line on the compaction curve. Default is `0.95`.
#'
#' @return A list with:
#' \describe{
#'   \item{`proctor`}{Tibble with the input data plus computed `gamma_d`, void
#'     ratio `e`, and degree of saturation `S` for each test point.}
#'   \item{`optimum`}{One-row tibble with the optimum water content (`w`) and
#'     maximum dry unit weight (`gamma_d`) obtained from the LOESS fit.}
#'   \item{`opt_properties`}{Tibble with LOESS-predicted values of `gamma`,
#'     `gamma_d`, `e`, and `S` at the optimum water content.}
#'   \item{`compaction_plot`}{A `ggplot2` object showing the compaction curve,
#'     ZAV lines, optimum point, and the `target_compaction` reference line.}
#'   \item{`properties_plot`}{A faceted `ggplot2` object showing how
#'     `gamma`, `gamma_d`, `e`, and `S` vary with water content.}
#' }
#' @export
#'
#' @family soil mechanics
#'
#' @examples
#' w     <- c(5, 7, 9.5, 11.8, 14.1, 17)
#' gamma <- c(18.46, 19.64, 20.85, 21.03, 20.84, 20.54)
#'
#' res <- compaction_test(w, gamma, Gs = 2.68)
#' res$optimum
#' res$compaction_plot
#' res$properties_plot
#'
#' # Spanish labels
#' res_es <- compaction_test(w, gamma, Gs = 2.68, lang = "es")
#' res_es$compaction_plot
#'
compaction_test <- function(
  w,
  gamma,
  Gs = 2.68,
  S_lines = c(70, 80, 90, 100),
  target_compaction = 0.95,
  lang = c("en", "es")
) {
  lang <- match.arg(lang)

  if (length(w) != length(gamma)) {
    stop("`w` and `gamma` must have the same length.")
  }

  # ── Labels ─────────────────────────────────────────────────────────────────
  labels <- if (lang == "en") {
    list(
      w_axis = "Water content, w (%)",
      gamma_d_axis = expression(paste(
        "Dry unit weight, ",
        gamma[d],
        " (",
        kN / m^3,
        ")"
      )),
      sat_legend = "Saturation, S (%)",
      prop_labels = c(
        gamma = expression(paste(
          "Total unit weight, ",
          gamma,
          " (",
          kN / m^3,
          ")"
        )),
        gamma_d = expression(paste(
          "Dry unit weight, ",
          gamma[d],
          " (",
          kN / m^3,
          ")"
        )),
        e = expression(paste("Void ratio, e")),
        S = expression(paste("Saturation, S (%)"))
      )
    )
  } else {
    list(
      w_axis = "Contenido de humedad, w (%)",
      gamma_d_axis = expression(paste(
        "Peso unitario seco, ",
        gamma[d],
        " (",
        kN / m^3,
        ")"
      )),
      sat_legend = "Saturaci\u00f3n, S (%)",
      prop_labels = c(
        gamma = expression(paste(
          "Peso unitario h\u00famedo, ",
          gamma,
          " (",
          kN / m^3,
          ")"
        )),
        gamma_d = expression(paste(
          "Peso unitario seco, ",
          gamma[d],
          " (",
          kN / m^3,
          ")"
        )),
        e = expression(paste("Relaci\u00f3n de vac\u00edos, e")),
        S = expression(paste("Saturaci\u00f3n, S (%)"))
      )
    )
  }

  # ── 1. Core proctor table ──────────────────────────────────────────────────
  proctor <- tibble::tibble(
    w = w,
    gamma = gamma,
    gamma_d = gamma / (1 + w / 100),
    e = (Gs * 9.8) / .data$gamma_d - 1,
    S = (w / 100 * Gs) / .data$e
  )

  # ── 2. LOESS fit & optimum ─────────────────────────────────────────────────
  loess_fit <- suppressWarnings(stats::loess(gamma_d ~ w, proctor))
  loess_aug <- broom::augment(
    loess_fit,
    newdata = tibble::tibble(w = seq(min(proctor$w), max(proctor$w), 0.1))
  ) |>
    purrr::set_names(c("w", "gamma_d"))

  optimum <- loess_aug |> dplyr::slice_max(.data$gamma_d)

  # predicted values of all properties at optimum w
  opt_properties <- proctor |>
    tidyr::pivot_longer(2:tidyr::last_col()) |>
    dplyr::nest_by(.data$name) |>
    dplyr::mutate(
      loess_i = list(suppressWarnings(stats::loess(
        value ~ w,
        data = .data$data
      ))),
      w_opt_pred = broom::augment(
        .data$loess_i,
        newdata = tibble::tibble(w = optimum$w)
      ) |>
        dplyr::pull()
    ) |>
    dplyr::ungroup() |>
    dplyr::select(prop = .data$name, .data$w_opt_pred)

  # ── 3. ZAV lines ──────────────────────────────────────────────────────────
  zav_s <- tidyr::expand_grid(
    w = seq(floor(min(proctor$w)), ceiling(max(proctor$w)), 0.5),
    S = S_lines
  ) |>
    dplyr::mutate(
      gamma_d = (Gs / (1 + (w / 100 * Gs) / (.data$S / 100))) * 9.8,
      S = as.factor(.data$S)
    )

  # ── 4. Compaction curve plot ───────────────────────────────────────────────
  compaction_plot <- proctor |>
    ggplot2::ggplot(ggplot2::aes(.data$w, .data$gamma_d)) +
    ggplot2::geom_line(
      ggplot2::aes(col = .data$S),
      data = zav_s,
      linetype = 2,
      linewidth = 0.5
    ) +
    ggplot2::geom_smooth(se = FALSE) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_point(
      data = optimum,
      shape = 3,
      col = "red",
      size = 3
    ) +
    ggplot2::geom_hline(
      yintercept = optimum$gamma_d * target_compaction,
      col = "red"
    ) +
    ggplot2::scale_x_continuous(n.breaks = 10) +
    ggplot2::scale_y_continuous(n.breaks = 10) +
    ggplot2::scale_colour_viridis_d(
      begin = 0.2,
      end = 0.8,
      direction = -1,
      option = "B"
    ) +
    ggplot2::coord_cartesian(
      ylim = c(min(proctor$gamma_d) * 0.99, max(proctor$gamma_d) * 1.01),
      xlim = c(min(proctor$w) * 0.98, max(proctor$w) * 1.02)
    ) +
    ggplot2::labs(
      x = labels$w_axis,
      y = labels$gamma_d_axis,
      col = labels$sat_legend
    ) +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(legend.position = "top")

  # ── 5. Properties facet plot ───────────────────────────────────────────────
  properties_plot <- proctor |>
    tidyr::pivot_longer(2:tidyr::last_col()) |>
    dplyr::mutate(
      name = factor(
        .data$name,
        levels = c("gamma", "gamma_d", "e", "S"),
        labels = labels$prop_labels
      )
    ) |>
    ggplot2::ggplot(ggplot2::aes(.data$w, .data$value)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_smooth(se = FALSE) +
    ggplot2::geom_vline(xintercept = optimum$w, col = "red") +
    ggplot2::scale_x_continuous(n.breaks = 10) +
    ggplot2::scale_y_continuous(n.breaks = 5) +
    ggplot2::facet_wrap(
      ~name,
      scales = "free_y",
      ncol = 1,
      labeller = ggplot2::label_parsed
    ) +
    ggplot2::labs(x = labels$w_axis, y = NULL) +
    ggplot2::theme_bw(base_size = 14)

  list(
    proctor = proctor,
    optimum = optimum,
    opt_properties = opt_properties,
    compaction_plot = compaction_plot,
    properties_plot = properties_plot
  )
}

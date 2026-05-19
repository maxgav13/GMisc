#' @title In-situ vertical stress profile
#'
#' @description
#' Builds a depth profile of total stress, pore pressure, and effective stress,
#' and returns both the data and a plot.
#'
#' @param boundaries Numeric vector of layer boundaries (m), in increasing order. For example, \code{c(6, 12, 22, 30)}.
#' @param soils Character vector of soil names for each layer
#'   (length must be \code{length(boundaries)}).
#' @param unit_weights Numeric vector of unit weights (kN/m^3) for each layer
#'   (length must be \code{length(boundaries)}).
#' @param NF Numeric scalar. Water table depth (m). Use \code{NA_real_} to omit pore pressure.
#' @param step Depth increment (m) used to discretize the profile. Default is \code{0.5}.
#'
#' @return A list with:
#' \describe{
#'   \item{Profile}{A tibble with depth, layer, unit weight, total stress, pore pressure, and effective stress.}
#'   \item{Graph}{A ggplot2 object.}
#' }
#' @export
#'
#' @family soil mechanics
#'
#' @examples
#' boundaries <- c(6, 12, 22, 30)
#' soils <- c("Sansy silt", "Fine sand", "Clay", "Gravel")
#' unit_weights <- c(18.9, 20.8, 19, 18.5)
#'
#' res <- insitu_stress(boundaries, soils, unit_weights, NF = 12)
#' res$Profile
#' res$Graph
#'
insitu_stress <- function(
  boundaries,
  soils,
  unit_weights,
  NF = NA_real_,
  step = 0.5
) {
  if (
    length(soils) != length(boundaries) ||
      length(unit_weights) != length(boundaries)
  ) {
    stop(
      "`soils` and `unit_weights` must have length equal to `length(boundaries)`."
    )
  }

  if (is.unsorted(boundaries, strictly = TRUE)) {
    stop("`boundaries` must be strictly increasing.")
  }

  max_depth <- boundaries[length(boundaries)]

  profile <- tibble::tibble(
    Profundidad = seq(0, max_depth, by = step)
  ) |>
    dplyr::mutate(
      dh = c(0, diff(.data$Profundidad)),
      Suelo = cut(
        .data$Profundidad,
        breaks = c(0, boundaries),
        labels = soils,
        include.lowest = TRUE
      ),
      Gamma = cut(
        .data$Profundidad,
        breaks = c(0, boundaries),
        labels = unit_weights,
        include.lowest = TRUE
      ) |>
        as.character() |>
        as.numeric(),
      Total = 0
    ) |>
    dplyr::mutate(
      Total = cumsum(dplyr::case_when(
        .data$Profundidad == 0 ~ 0,
        .default = stats::lag(.data$Total) + .data$dh * .data$Gamma
      )),
      Presion_poro = dplyr::case_when(
        # is.na(NF) ~ NA_real_,
        dplyr::between(.data$Profundidad, 0, NF - step) ~ NA_real_,
        .data$Profundidad == NF ~ 0,
        .default = (.data$Profundidad - NF) * 9.8
      ),
      Efectivo = dplyr::if_else(
        is.na(.data$Presion_poro),
        .data$Total,
        .data$Total - .data$Presion_poro
      )
    )

  plot_data <- profile |>
    tidyr::pivot_longer(
      .data$Total:.data$Efectivo,
      names_to = "Stress",
      values_to = "value"
    ) |>
    dplyr::mutate(
      Stress = factor(
        .data$Stress,
        levels = c("Total", "Presion_poro", 'Efectivo'),
        labels = c("Total", "Pore pressure", "Effective")
      )
    )

  profile <- profile |>
    dplyr::select(-.data$dh) |>
    purrr::set_names(c(
      "Depth",
      "Soil",
      "Unit_Weight",
      "Total_Stress",
      "Pore_Pressure",
      "Effective_Stress"
    ))

  if (is.na(NF)) {
    p <- plot_data |>
      tidyr::drop_na() |>
      ggplot2::ggplot(ggplot2::aes(
        .data$value,
        .data$Profundidad,
        col = .data$Stress
      )) +
      ggplot2::geom_line(linewidth = 0.5) +
      ggplot2::geom_point(size = 1) +
      # ggplot2::scale_y_reverse(limits = c(max_depth, 0)) +
      ggplot2::coord_transform(y = 'reverse', ylim = c(0, max_depth)) +
      ggplot2::labs(x = "Stress (kPa)", y = "Depth (m)") +
      ggplot2::scale_color_manual(values = c("red", "darkgreen")) +
      ggplot2::theme_bw(base_size = 14) +
      ggplot2::theme(legend.position = "top")
  } else {
    nf_marker <- tibble::tibble(
      value = stats::quantile(profile$Total_Stress, na.rm = TRUE, probs = 0.75),
      Profundidad = NF - step
    )

    p <- plot_data |>
      ggplot2::ggplot(ggplot2::aes(
        .data$value,
        .data$Profundidad,
        col = .data$Stress
      )) +
      ggplot2::geom_hline(
        yintercept = NF,
        col = "lightblue",
        linewidth = 0.5,
        linetype = 2
      ) +
      ggplot2::geom_point(
        data = nf_marker,
        ggplot2::aes(.data$value, .data$Profundidad),
        col = "lightblue",
        shape = 6,
        size = 4,
        stroke = 1,
        inherit.aes = FALSE
      ) +
      ggplot2::geom_line(linewidth = 0.5) +
      ggplot2::geom_point(size = 1) +
      # ggplot2::scale_y_reverse(limits = c(max_depth, 0)) +
      ggplot2::coord_transform(y = 'reverse', ylim = c(0, max_depth)) +
      ggplot2::labs(x = "Stress (kPa)", y = "Depth (m)") +
      ggplot2::scale_color_manual(values = c("red", "blue", "darkgreen")) +
      ggplot2::theme_bw(base_size = 14) +
      ggplot2::theme(legend.position = "top")
  }

  list(Profile = profile, Graph = p)
}

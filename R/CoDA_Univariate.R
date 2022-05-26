#' @title CoDA Univariate Statistics
#' @description Calculates univariate statistics for the parts of a given composition, using Compositional Data Analysis (CoDA) principles.
#' @param data A dataframe of observations for a given composition. Entries must be non-zero and positive.
#' @param ppm_vars A character vector with the names of the parts of the data that are in parts-per-million (ppm).
#' @export
#' @importFrom ggplot2 .data
#' @return A tibble with univariate statistics (mean, median, sd-ilr, mad-ilr) for the given composition, in their original scale (percent and/or ppm). For a more detailed explanation check the reference.
#' @references Filzmoser, P., Hron, K. & Reimann, C. (2009). Univariate statistical analysis of environmental (compositional) data: Problems and possibilities. Science of The Total Environment, 407(23), 6100-6108. 10.1016/j.scitotenv.2009.08.008.
#' @name CoDA_Univariate
#' @examples
#'
#' data("Aar", package = 'compositions')
#' d1 = Aar %>% dplyr::select(SiO2,Al2O3,MnO)
#' d2 = Aar %>% dplyr::select(SiO2,Al2O3,Ba,Pb)
#' CoDA_Univariate(d1) # with no variables in ppm
#' CoDA_Univariate(d2, ppm_vars = c('Ba','Pb')) # with Ba and Pb variables in ppm
#'
CoDA_Univariate = function(data, ppm_vars = NULL) {

  xbar = function(x) {
    (exp(sqrt(2)*x)/(exp(sqrt(2)*x)+1))*100
  }

  data = data.frame(data)

  if (is.null(ppm_vars)) {

    dat.ilr = matrix(0,
                     nrow = nrow(data),
                     ncol = ncol(data)) %>%
      dplyr::as_tibble(.name_repair = 'unique') %>%
      purrr::set_names(paste0('ilr_', names(data)))

    for (i in 1:ncol(data)) {
      dat.ilr[[i]] = sqrt(.5)*log(data[[i]]/(100-data[[i]]))
    }

    dat.ilr.long = dat.ilr %>%
      tidyr::pivot_longer(dplyr::everything(),
                          names_to = 'variable',
                          values_to = 'value') %>%
      dplyr::mutate(variable = forcats::as_factor(.data$variable))

    center = dat.ilr %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), list(Mean=mean,Median=stats::median))) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), xbar)) %>%
      tidyr::pivot_longer(dplyr::everything(),
                          names_to = c('space','variable','stat'),
                          names_sep = '_',
                          values_to = 'value') %>%
      tidyr::pivot_wider(id_cols = .data$variable,
                         names_from = .data$stat,
                         values_from = .data$value)

    dispersion = dat.ilr %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), list(SD=stats::sd,MAD=stats::mad))) %>%
      tidyr::pivot_longer(dplyr::everything(),
                          names_to = c('space','variable','stat'),
                          names_sep = '_',
                          values_to = 'value') %>%
      tidyr::pivot_wider(id_cols = .data$variable,
                         names_from = c(.data$stat,.data$space),
                         values_from = .data$value)

    res = center %>%
      dplyr::left_join(dispersion, by = 'variable')

  } else {

    data = data %>%
      dplyr::mutate(dplyr::across(dplyr::contains(ppm_vars), ~./10000))

    dat.ilr = matrix(0,
                     nrow = nrow(data),
                     ncol = ncol(data)) %>%
      dplyr::as_tibble() %>%
      purrr::set_names(paste0('ilr_', names(data)))

    for (i in 1:ncol(data)) {
      dat.ilr[[i]] = sqrt(.5)*log(data[[i]]/(100-data[[i]]))
    }

    dat.ilr.long = dat.ilr %>%
      tidyr::pivot_longer(dplyr::everything(),
                          names_to = 'variable',
                          values_to = 'value') %>%
      dplyr::mutate(variable = forcats::as_factor(.data$variable))

    center = dat.ilr %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), list(Mean=mean,Median=stats::median))) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), xbar)) %>%
      dplyr::mutate(dplyr::across(dplyr::contains(ppm_vars), ~.*10000)) %>%
      tidyr::pivot_longer(dplyr::everything(),
                          names_to = c('space','variable','stat'),
                          names_sep = '_',
                          values_to = 'value') %>%
      tidyr::pivot_wider(id_cols = .data$variable,
                         names_from = .data$stat,
                         values_from = .data$value)

    dispersion = dat.ilr %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), list(SD=stats::sd,MAD=stats::mad))) %>%
      tidyr::pivot_longer(dplyr::everything(),
                          names_to = c('space','variable','stat'),
                          names_sep = '_',
                          values_to = 'value') %>%
      tidyr::pivot_wider(id_cols = .data$variable,
                         names_from = c(.data$stat,.data$space),
                         values_from = .data$value)

    res = center %>%
      dplyr::left_join(dispersion, by = 'variable')

  }

  return(res)

}


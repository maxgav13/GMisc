#' @title Effects plots with confidence and prediction intervals
#' @description Calculates and plots the confidence and prediction intervals for the terms used on a linear model.
#' @param model Linear model of type \code{lm()}
#' @param sig.level Significance level to use for the intervals (Default is 0.95)
#' @param pred.fill Fill color for the prediction interval region
#' @param conf.fill Fill color for the confidence interval region
#' @param line.col Line color for the fitted values
#' @param points.col Color for the data points
#' @param alpha Transparency value to use for confidence and prediction regions (Default is 0.5)
#' @param plot.data A logical indicating either to plot the data points or not. Does not apply for the interaction effect plot, only indivual terms (Default is FALSE)
#' @export
#' @return A list of plots of effects
#' @import stats
#' @import broom
#' @import dplyr
#' @import ggplot2
#' @import stringr
#' @details As of now this function can only take numeric (continuous) variables, which can be present in the linear model as polynomial, logarithmic, and/or interactions. The function estimates the effect of each term in the model while holding the rest constant at their mean value. It can only deal with models with ONE (1) interaction and ONE (1) logarithmic term. The interaction effect is plotted using the 5 major quantiles (0, 25, 50, 75, 100) of the second term in the interaction
#' @examples
#' m1 = lm(mpg ~ wt + hp, mtcars)
#' m2 = lm(mpg ~ wt + I(wt^2) + hp, mtcars)
#' m3 = lm(mpg ~ wt * hp, mtcars)
#' m4 = lm(mpg ~ wt, mtcars)
#' m5 = lm(mpg ~ log(wt), mtcars)
#' m6 = lm(mpg ~ log(wt) * hp, mtcars)
#' intervalos(m1)
#' intervalos(m2)
#' intervalos(m3)
#' intervalos(m4)
#' intervalos(m5)
#' intervalos(m6)
#'
intervalos = function(model, sig.level = 0.95, pred.fill = "skyblue", conf.fill = "darkblue",
                      line.col = "red", points.col = "black", alpha = 0.5, plot.data = FALSE) {
  m = model
  model = m$model
  model = model %>% select(-(starts_with("I")))

  a = 1 - sig.level
  tcrit = qt(1-a/2, df.residual(m))

  variables = attributes(m$terms)$dataClasses
  terminos = attributes(m$terms)$term.labels

  if (isTRUE(any(str_detect(terminos, "log")) == T)) {
    logterm = terminos[str_detect(terminos, "log")]
    logterm = ifelse(length(logterm)>2, logterm[1], logterm)
    logvar = str_extract_all(logterm, boundary("word"))[[1]][2]
    names(model)[names(model) == logterm] = logvar
    if (str_detect(logterm, "log10") == T) {
      model[[logvar]] = 10^(model[[logvar]])
    } else {
      model[[logvar]] = exp(model[[logvar]])
    }
  }

  nombres = names(model)
  nom.y = nombres[1]
  nom.x = nombres[2:length(nombres)]

  avgs = as.data.frame(t(colMeans(model)))
  avgs = as.data.frame(avgs[,2:ncol(avgs)])
  names(avgs) = nom.x

  secuencias = list()
  for (i in 1:ncol(avgs)) {
    secuencias[[i]] = data.frame(seq(min(select(model, i+1)),max(select(model, i+1)),length.out = 50))
    names(secuencias[[i]]) = nom.x[i]
  }

  pred.data = list()
  for (i in 1:ncol(avgs)) {
    pred.data[[i]] = data.frame(secuencias[[i]], avgs[-i])
  }

  predicciones = list()
  for (i in 1:ncol(avgs)) {
    predicciones[[i]] = augment(m, newdata = pred.data[[i]]) %>% mutate(.se.pred = sqrt(.se.fit^2 + sigma(m)^2),
                                                                        ci.lwr = .fitted - tcrit * .se.fit,
                                                                        ci.upr = .fitted + tcrit * .se.fit,
                                                                        pred.lwr = .fitted - tcrit * .se.pred,
                                                                        pred.upr = .fitted + tcrit * .se.pred)
  }
  names(predicciones) = nom.x

  gg = list()
  for (i in 1:ncol(avgs)) {
    gg[[i]] = ggplot(predicciones[[i]], aes_string(x = nom.x[i])) +
      geom_ribbon(aes(ymin = pred.lwr, ymax = pred.upr), fill = pred.fill, alpha = alpha) +
      geom_ribbon(aes(ymin = ci.lwr, ymax = ci.upr), fill = conf.fill, alpha = alpha) +
      geom_line(aes(y = .fitted), col = line.col, size = 1) +
      {if (plot.data == T) geom_point(data = model,  aes_string(x = nom.x[i], y = nom.y), col = points.col, size = 2)} +
      labs(y = nom.y, x = nom.x[i], title = paste("Effect of", nom.x[i])) +
      theme_bw() + geom_rug(data = model,  aes_string(x = nom.x[i], y = nom.y), sides = "b")
  }
  names(gg) = nom.x

  if (isTRUE(any(str_detect(terminos, ":")) == T)) {
    name.int = terminos[str_detect(terminos, ":")]
    splits = unlist(str_split(name.int, ":"))

    if (isTRUE(any(str_detect(terminos, "log")) == T)) {
      logterm.int = logterm[1]
      splits[splits == logterm.int] = logvar
    }

    x1 = seq(min(model[[splits[1]]]), max(model[[splits[1]]]), length.out = 50)
    x2 = quantile(model[[splits[2]]])

    df = expand.grid(x1, x2)
    names(df) = splits

    pred.int = augment(m, newdata = df) %>% mutate(.se.pred = sqrt(.se.fit^2 + sigma(m)^2),
                                                   ci.lwr = .fitted - tcrit * .se.fit,
                                                   ci.upr = .fitted + tcrit * .se.fit,
                                                   pred.lwr = .fitted - tcrit * .se.pred,
                                                   pred.upr = .fitted + tcrit * .se.pred)
    gg.int = ggplot(pred.int, aes_string(x = splits[1])) +
      geom_ribbon(aes(ymin = pred.lwr, ymax = pred.upr), fill = "skyblue", alpha = alpha) +
      geom_ribbon(aes(ymin = ci.lwr, ymax = ci.upr), fill = "darkblue", alpha = alpha) +
      geom_line(aes(y = .fitted), col = "red", size = 1) +
      labs(y = nom.y, x = nom.x[1], title = paste("Interaction effect of", nom.x[1], "&", nom.x[2])) +
      theme_bw() + facet_wrap(c(nom.x[2]), labeller = "label_both")

    predicciones[[name.int]] = pred.int
    gg[[name.int]] = gg.int
  }

  return(gg)
}

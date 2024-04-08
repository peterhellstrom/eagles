
## Functions for derivatives of GAM models ##
# by Gavin Simpson
# Source: https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

# Replaced by gratia-package 2018
# See examples and further info:
# https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30
# https://gavinsimpson.github.io/gratia/articles/gratia.html
# https://fromthebottomoftheheap.net/
# http://www.fromthebottomoftheheap.net/2011/06/12/additive-modelling-and-the-hadcrut3v-global-mean-temperature-series/
# https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
# https://www.r-bloggers.com/2018/10/introducing-gratia/
# https://m-clark.github.io/generalized-additive-models/technical.html

#' @export
signifD <- function(x, d, upper, lower, eval = 0) {
  miss <- upper > eval & lower < eval
  incr <- decr <- x
  want <- d > eval
  incr[!want | miss] <- NA
  want <- d < eval
  decr[!want | miss] <- NA
  list(incr = incr, decr = decr)
}

# gam_predict:
# m is an object fitted with mgcv::gam
# n_pts_predict is the number of data points used for prediction
# output_format is long (default) or wide
# Dependencies: https://github.com/gavinsimpson/gratia/blob/master/R/derivatives.R

#' @export
gam_predict <- function(m, n_pts_predict = 100, output_format = "long") {

  cn <- attr(m$terms , "term.labels")
  m_deriv <- gratia::derivatives(object = m, n = n_pts_predict)
  p_x <- tibble::tibble(m_deriv$data) |>
    rlang::set_names(cn)

  #p_x <- data.frame(
  #  seq(min(m$model[,cn]), max(m$model[,cn]), length = n_pts_predict)) |>
  #  tibble::as_tibble() |>
  #  rlang::set_names(cn)

  p <- predict(m, newdata = p_x, type = "response", se.fit = TRUE) |>
    tibble::as_tibble() |>
    rlang::set_names(c("predict", "se.predict"))

  if (nrow(m_deriv) == 0) {
    # For linear models fitted with lm() [i.e. without smoothing term], e.g. y ~ x
    # Included for comparison of linear and gam-estimated trends
    # Plotting this will only work if output_format = "long"
    p <- dplyr::bind_cols(p_x, p)
    p <- p |>
      dplyr::bind_cols(
        tibble::tibble(incr = NA_real_, decr = NA_real_)
      )
  } else {
    # If a model with smoothing terms, e.g. y ~ s(x) is fitted to the data
    S_deriv <- signifD(
      p[["predict"]],
      m_deriv$derivative, m_deriv$upper, m_deriv$lower,
      eval = 0
    ) |>
      tibble::as_tibble()

    p <- dplyr::bind_cols(p_x, p, S_deriv)
  }

  if (output_format == "long") {
    p <- p |>
      tidyr::pivot_longer(
        cols = c(predict, incr:decr),
        values_to = "predict",
        names_to = "variable") |>
      dplyr::arrange(variable, cn) |>
      dplyr::mutate(
        variable = factor(variable, levels = c("predict", "decr", "incr"))
      ) |>
      dplyr::select(all_of(cn), predict, se.predict, variable)

    # p <- p |> dplyr::filter(!is.na(value))
  }

  p

}

# NOTE: Make this function work for nested data,
# object is an mgcv::gam-object fitted to data

# (shouldn't it be possible to just extract the data
# part from the fitted gam-object?)
# x and y is variables used to map variables to aesthetics

# This function works as a geom, e.g.
# ggplot(data, aes(x,y)) + geom_trend_gam(object)

# NOTE: function works when using either aes_string("x", "y") or aes(x, y)

# ToDo: This approach doesn't work well for grouped data or multiple datasets,
# due to the need for an already existing object.
# A more flexible approach is too call ggplot(data, mapping) + geom_smooth(method = "gam")
# save this plot and work with the saved object, e.g. extract data with ggplot_build().
# However gratia::derivatives needs a fitted object...
# gam_predict should perhaps be called in a separate function outside this function,
# adding the necessary columns to existing data?
# This is the approach I have used with gam_fit_trends
# Re-work the function below, move "p-section" to gam_fit_trends and make this
# function a strict geom function without dependencies.

#' @export
geom_trend_gam <- function(
    ..., rect.params = list(),
    fit.params = list(), ci.params = list(),
    fit.decr.params = list(), fit.incr.params = list(),
    hline.params = list(), point.params = list(),
    add_items = c("ci", "fit", "rect", "hline",
                  "fit.decr", "fit.incr", "point"),
    object,
    n_pts_predict = 300
) {

  p <- gam_predict(
    m = object, n_pts = n_pts_predict,
    output_format = "wide") |>
    dplyr::mutate(
      ymin = predict - 1.96 * se.predict,
      ymax = predict + 1.96 * se.predict) |>
    as.data.frame()

  # se.predict * qt(0.95 / 2 + .5, object$df.residual)

  params <- list(...)

  rect.params <- modifyList(params, rect.params)
  ci.params <- modifyList(params, ci.params)
  hline.params <- modifyList(params, hline.params)
  fit.params <- modifyList(params, fit.params)
  fit.decr.params <- modifyList(params, fit.decr.params)
  fit.incr.params <- modifyList(params, fit.incr.params)
  point.params <- modifyList(params, point.params)

  rect <- do.call("geom_rect", modifyList(
    list(
      data = data.frame(xmin = -Inf, xmax = Inf, ymin = 0.97, ymax = 1.75),
      mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "palegreen3", alpha = 0.2, inherit.aes = FALSE),
    rect.params)
  )

  ci <- do.call("geom_ribbon", modifyList(
    list(
      data = p,
      mapping = aes(p[,1], ymin = ymin, ymax = ymax),
      fill = "grey60",
      alpha = 0.2, inherit.aes = FALSE),
    ci.params)
  )

  hline <- do.call("geom_hline", modifyList(
    list(
      yintercept = 1.31, size = 1,
      color = "orange", linetype = "dashed"),
    hline.params)
  )

  fit <- do.call("geom_line", modifyList(
    list(
      data = p, mapping = aes(p[,1], predict),
      color = "darkgrey", size = 1, alpha = 1, na.rm = TRUE),
    fit.params)
  )

  fit.decr <- do.call("geom_line", modifyList(
    list(
      data = p, mapping = aes(p[,1], decr),
      color = "red", size = 1.5, alpha = 1, lineend = "round",
      na.rm = TRUE),
    fit.decr.params)
  )

  fit.incr <- do.call("geom_line", modifyList(
    list(
      data = p, mapping = aes(p[,1], incr),
      color = "blue", size = 1.5, alpha = 1, lineend = "round",
      na.rm = TRUE),
    fit.incr.params)
  )

  point <- do.call("geom_point", modifyList(
    list(
      size = 3, fill = "steelblue", color = "black", shape = 21,
      alpha = 1, na.rm = TRUE),
    point.params)
  )

  all_items <- c("rect", "ci", "hline",
                 "fit", "fit.decr", "fit.incr", "point")

  if("all" %in% add_items) {
    add_items <- all_items
  }

  out_list <- c(rect, ci, hline, fit, fit.decr, fit.incr, point)
  names(out_list) <- all_items

  out_list <- out_list[add_items]

  out_list
}


# input argument model is a function!
#' @export
gam_fit_trends <- function(data, model) {

  # Created nested data and fit model
  # for each item in list-column
  m_p <- data |>
    tidyr::nest() |>
    dplyr::mutate(
      m = purrr::map(data, {{ model }} ),
      s = purrr::map(m, gam_predict),
      glance = purrr::map(m, broom::glance)
    ) |>
    tidyr::unnest(glance) |>
    dplyr::mutate(
      t = purrr::map(m, broom::tidy)
    ) |>
    tidyr::unnest(t)
}

# OBS! Endast testad på HELCOM-data!
# Add call to geom_trend_gam in here instead?
#' @export
plot_gam_fit_trends <- function(
    object, x, y, group, p_value_cutoff,
    xlab = NULL, ylab = NULL
) {

  obs_data <- object |>
    unnest(cols = c( {{ group }} , data))

  if (missing(p_value_cutoff)) {
    pred_data <- object |>
      unnest(cols = c( {{ group }}, s))
  } else {
    pred_data <- object |>
      filter(p.value < p_value_cutoff) |>
      unnest(cols = c( {{ group }} , s))
  }

  f_group <- enquo(group)

  p_test <- obs_data |>
    ggplot(
      aes(x = {{x}}, y = {{y}}, group = {{ group }} )) +
    geom_line(
      data = pred_data,
      mapping = aes(x = {{x}}, y = predict, group = variable,
                    color = variable, size = variable),
      na.rm = TRUE) +
    scale_size_manual(values = c(1, 1.5, 1.5)) +
    scale_colour_manual(values = c("darkgrey", "red", "blue")) +
    geom_point(
      size = 1.25, alpha = 1,
      shape = 21, fill = "steelblue", na.rm =TRUE) +
    facet_wrap(facets = vars(!!f_group), ncol = 2, scales = "free_y") +
    labs(x = xlab, y = ylab) +
    theme(
      legend.position = "none")

  p_test

}

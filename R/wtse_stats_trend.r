# DISPLAY TREND
# y = response variable, x = predictor variable (e.g. time/year)
# model = object fitted with gam, gamm, glm, lm (NOTE: models with polynomial terms does not work)
# reference levels = c(min, mean, max)
# npts = number of points used in significance testing?
# alpha = 1 - significance level (e.g. 1 - 0.95 = 0.05), used to obtain conficence intervals
# pch = point character symbol for time series points, pch_col=color of point character symbol
# pch_bg = optional background color of point character symbols (used e.g. if pch=21)
# add = FALSE/TRUE, add_points = FALSE/TRUE, add_line = FALSE/TRUE
# ref_line = FALSE/TRUE: draw reference level lines, as specified by argument reference
# ref_col = color of reference lines, ref_lty = line type vector for reference level lines,
# ref_lwd = line width vector for reference lines
# trend_col =
# trend_lty =
# trend_lwd
# signif =
# ci = FALSE/TRUE
# main = , xlab = , ylab =
# ... optional arguments can be passed to main plot

#' @export
wtse_trend <- function(
    y, x,
    model,
    reference = c(0, 0, 0),
    npts = 500,
    alpha = 0.05,
    pch = 21,
    pch_col = "black",
    pch_bg = "steelblue",
    pch_cex = 1,
    add = FALSE,
    add_points = TRUE,
    add_line = TRUE,
    ref_line = TRUE,
    ref_col = "darkgrey",
    ref_lty = c(3, 2, 3),
    ref_lwd = c(1.5, 1.5, 1.5),
    ref_polygon = FALSE,
    ref_polygon.col = "yellow",
    trend_col = "black",
    trend_lty = 1,
    trend_lwd = 1.5,
    signif = TRUE,
    signif_lwd = 2,
    signif_col = c("red", "blue"),
    ci = TRUE,
    main = "White-tailed sea eagle",
    xlab = "Year",
    ylab = "Variable",
    ...
) {

  # If response variable has multiple columns, calculate proportions
  # This assumes that the response variable is binomial, and further that the
  # number of "successes" is supplied in the first columns.
  if (class(y)[1] == "matrix") y <- y[,1] / rowSums(y)

  # Predict model
  # Add support for different object classes AND covariates

  # Extraction of model terms does NOT work for:
  # polynomial terms and LOESS-objects
  if (class(model)[1] == "gamm") {
    m <- model$gam
  } else {
    m <- model
  }

  cn <- attr(m$terms , "term.labels")

  if (class(m)[1] == "loess") {
    # predict_newdata <- with(m, data.frame(seq(min(m[[cn]]), max(m[[cn]]), length = npts)))
    predict_newdata <- with(
      m,
      data.frame(
        seq(min(m[["x"]]), max(m[["x"]]), length = npts)
      )
    )
  } else {
    predict_newdata <- with(
      m$model,
      data.frame(
        seq(min(m$model[,cn]), max(m$model[,cn]), length = npts)
      )
    )
  }
  names(predict_newdata) <- cn

  # Predict model
  p <- predict(m, newdata = predict_newdata, type = "response")

  # Confidence intervals
  p_ci <- predict(m, newdata = predict_newdata, se = TRUE, type = "response")

  # For GAM-models, find "significant" parts
  if (class(model)[1] == "gam" | class(model)[1] == "gamm") {
    # calculate first derivative of model
    model_deriv <- gratia::derivatives(model, n = npts)

    S <- with(
      model_deriv,
      signifD(
        p, .derivative, .upper_ci, .lower_ci, eval = 0
      )
    )

  } else {
    cat("significance only available for gam or gamm objects fitted with mgcv", "\n")
  }

  # Create main plot of time series, if add=FALSE
  # if add=TRUE, the data is plotted in the currently active graphical device
  if (add == FALSE) {
    plot(
      x, y,
      type = "n", bty = "l",
      main = main,
      xlab = xlab, ylab = ylab,
      ...
    )
  }

  if (ref_polygon) {
    plot.range <- par("usr")
    polygon(
      x = c(
        plot.range[1],
        plot.range[1],
        plot.range[2],
        plot.range[2],
        plot.range[1]
      ),
      y = c(
        reference[1],
        reference[3],
        reference[3],
        reference[1],
        reference[1]
      ),
      col = ref_polygon.col,
      border = NA
    )
  }

  if (ref_line) {
    abline(
      h = reference,
      lty = ref_lty,
      lwd = ref_lwd,
      col = ref_col
    )
  }

  if (add_line) {
    lines(
      p ~ get(cn),
      data = predict_newdata,
      lty = trend_lty,
      lwd = trend_lwd,
      col = trend_col
    )
    if (signif) {
      lines(
        S$incr ~ get(cn),
        data = predict_newdata,
        lwd = signif_lwd,
        col = signif_col[2]
      )
      lines(
        S$decr ~ get(cn),
        data = predict_newdata,
        lwd = signif_lwd,
        col = signif_col[1]
      )
    }
    if (ci) {
      lcl <- p_ci$fit - 1.96 * p_ci$se.fit
      ucl <- p_ci$fit + 1.96 * p_ci$se.fit
      lines(
        lcl ~ get(cn),
        data = predict_newdata,
        lty=2
      )
      lines(
        ucl ~ get(cn),
        data = predict_newdata,
        lty=2
      )
    }
  }

  if (add_points) {
    points(
      y ~ x,
      pch = pch,
      col = pch_col,
      bg = pch_bg,
      cex = pch_cex
    )
  }
}

# Example (not run)
# x <- 1964:2016

# y <- c(0.73076923, 0.35483871, 0.30434783, 0.43750000, 0.33333333, 0.32142857, 0.24242424, 0.11111111, 0.17241379, 0.08333333, 0.22727273, 0.29629630,
# 0.18750000, 0.13793103, 0.20571429, 0.21621622, 0.19444444, 0.17142857, 0.25000000, 0.23684211, 0.40336134, 0.28125000, 0.58823529, 0.42500000,
# 0.51219512, 0.67058824, 0.66315789, 0.82723404, 0.83333333, 0.85510071, 1.18979964, 1.10886417, 1.10025316, 1.25544794, 1.15530630, 1.14230769,
# 1.24987703, 0.99597110, 1.19802079, 0.94410737, 1.19699113, 1.13809351, 1.10346560, 1.13274336, 1.10245310, 1.19985024, 0.92413071, 1.04643180,
# 1.11993522, 1.16038732, 1.18300654, 1.08522883, 1.06553911)

# m1 <- gam(y ~ s(x, bs = "cs"), family = gaussian())
# m2 <- loess(y ~ x)

# wtse_trend(y = y, x = x, model = m1, ylab = "Productivity", xlab = "")
# wtse_trend(y = y, x = x, model = m2, ylab = "Productivity", xlab = "", signif = FALSE)

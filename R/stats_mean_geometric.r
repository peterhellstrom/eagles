# Calculate geometric mean
#' @export
mean_geometric <- function(x, ...) exp(mean(log(x), ...))

# see discussion (and improvements to my simple function above) here:
# https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in

# Calculate geometric mean confidence interval
# See also: https://stats.stackexchange.com/questions/285173/how-to-calculate-confidence-interval-for-a-geometric-mean

#' @export
ci <- function(x,
               type = c("geometric", "arithmetic", "harmonic"),
               dist = c("t", "norm"),
               alpha = 0.05,
               range = FALSE,
               na.rm = TRUE) {

	type <- match.arg(type)
	dist <- match.arg(dist)

	if (na.rm) x <- x[!is.na(x)]
	if (range) rng <- range(x, na.rm = na.rm)

	if (type == "geometric") {
		x <- x[x > 0]
		x <- log(x)
	} else if (type == "harmonic") {
		x <- x[x > 0]
		x <- 1/x
	}

	n <- length(x)
	xbar <- mean(x)

	if (dist == "t") {
		quantiles <- qt(c(alpha/2, 1 - alpha/2), df = n - 1)
	} else {
		quantiles <- qnorm(c(alpha/2, 1 - alpha/2))
	}
	se <- sqrt(var(x)/n)
	cl <- xbar + quantiles * se

	if (type == "geometric") {
		cl <- exp(cl)
		xbar <- exp(xbar)
	} else if (type == "harmonic") {
		cl <- rev(1/(xbar + quantiles * se))
		xbar <- 1/xbar
	}

	if (range) {
		out <- data.frame(
		  n = n,
		  mean = xbar,
		  cil = cl[1],
		  ciu = cl[2],
		  min = rng[1],
		  max = rng[2])
	} else {
		out <- data.frame(
		  n = n,
		  mean = xbar,
		  cil = cl[1],
		  ciu = cl[2])

		names(out) <- c("n", "mean", "cil", "ciu")
		out
	}
}

#' @export
summary_geometric <- function(x, na.rm = TRUE, alpha = 0.05) {

  # remove NA-values if argument na.rm = TRUE
	if (na.rm) x <- x[!is.na(x)]

	n <- length(x)
	log_x <- log(x)
	mean_log_x <- mean(log_x)
	log_sd <- sd(log_x)
	log_se <- log_sd / sqrt(n)

	if (n <= 1) {
		cil <- c(NA, NA)
	} else {
	  quantiles <- qt(c(alpha/2, 1 - alpha/2), df = n - 1)
		cil <- mean_log_x + quantiles * log_se
		cil <- exp(cil)
	}

	out <- data.frame(
	  'n' = n,
	  'mean_geometric' = exp(mean_log_x),
	  'sd' = log_sd,
	  'ci_lower' = cil[1],
	  'ci_upper' = cil[2],
	  'min' = min(x),
	  'max' = max(x))

	out
}

#' @export
summary2 <- function(x, na.rm = TRUE) {

  if (na.rm) x <- x[!is.na(x)]

  n <- length(x)

	out <- c(
	  'n' = n,
	  'mean_arithmetic' = mean(x),
	  'mean_geometric' = geom_mean(x),
	  'sd' = sd(x),
	  'cv' = 100*sd(x) / mean(x),
	  'se' = sd(x)/sqrt(n),
	  'min' = min(x),
	  'max' = max(x))

	out
}

# # Test functions:
#
# # Generate some sample data
# # lognormal data
# x <- rlnorm(100, 5, 2)
# hist(x)
#
# mean_geometric(x)
# exp(mean(log(x)))
#
# exp(ci(log(x), "arithmetic"))
# ci(x, "geometric")
# ci(x, "harmonic")
#
# map_dfr(.x = c("arithmetic", "geometric", "harmonic"),
#         ~ ci(x, type = .x, range = TRUE))
#
# summary_geometric(x)
# # Test with grouped data frame
#
# z <- tibble(
#   grp = c(1, 1, 2, 2),
#   a = c(4, 7, 12, 7),
#   b = c(1, 5, 10, 9)) %>%
#   pivot_longer(
#     cols = c(a, b),
#     values_to = "value",
#     names_to = "sub_grp") %>%
#   group_by(grp, sub_grp) %>%
#   arrange(grp, sub_grp)
#
# z %>%
#   summarize(summary_geometric(value))

# NOTE: The new variable `>0` is ignored in the indicator function below.
# Should be updated!

#' @export
calculate_indicator <- function(.data) {
  if (dplyr::is_grouped_df(.data)) {
    return(dplyr::do(.data, calculate_indicator(.)))
  }
  .data %>%
    mutate(
      checked = rowSums(select(., grep("[0-3]", names(.)))),
      unproductive = `0C` + `0G`,
      productive = checked - unproductive,
      prop_productive = productive / checked,
      nestlings_climbed = `1` + `2` * 2 + `3` * 3,
      nestlings_ground = `>=1` + `>=2` * 2 + `>=3` * 3,
      broods_climbed = `1` + `2` + `3`,
      broods_ground = `>=1` + `>=2` + `>=3`,
      brood_size_climbed =  nestlings_climbed / broods_climbed,
      brood_size_ground =  nestlings_ground / broods_ground,
      productivity = prop_productive * brood_size_climbed,
      status = ifelse(productivity < 0.97 | brood_size_climbed < 1.64 | prop_productive < 0.59, "sub-GES", "GES"))
}

# GAM-models for WTSE-data
#' @export
gam_prop_productive <- function(df) {
  mgcv::gam(cbind(productive, unproductive) ~ s(Year, bs = "cs", k = -1),
            family = binomial("logit"), method = "REML", data = df) }

#' @export
gam_brood_size <- function(df) {
  mgcv::gam(brood_size_climbed ~ s(Year, bs = "cs", k = -1),
            family = gaussian(), method = "REML", data = df) }

#' @export
gam_productivity <- function(df) {
  mgcv::gam(productivity ~ s(Year, bs = "cs", k = -1),
            family = gaussian(), method = "REML", data = df) }


# .x = data.frame with data, columns names of data frame matches possible breeding outcomes,
# as defined by outcomes which must be a factor with "correct" order of levels
# n_boot = Number of bootstrap samples.
# This is a non-parametric bootstrap approach, sample with replacement

#' @export
wtse_boot <- function(.x,
                      outcomes = c("0C", "1", "2", "3", "0G", ">0", ">=1", ">=2", ">=3"),
                      n_boot = 1000) {

	out <- vector("list", nrow(.x))

	for (i in seq_len(nrow(.x))) {
		.x_counts <- .x[i, colnames(.x) %in% outcomes]
		.x_bootstrap_vector <- rep(outcomes, .x_counts)
		.x_bootstrap_samples <- replicate(n_boot, sample(.x_bootstrap_vector, replace = TRUE))
		.x_bootstrap_counts <- lapply(seq_len(ncol(.x_bootstrap_samples)), function(j) {
		  table(factor(.x_bootstrap_samples[,j], levels = outcomes)) } )
		.x_bootstrap_counts <- as.data.frame(do.call(rbind, .x_bootstrap_counts))
		m <- calculate_indicator(.x_bootstrap_counts)
		out[[i]] <- m
	}
	out
}

#' @export
wtse_productivity <- function(dd, stats.brood = TRUE,
                              bootstrap.brood = TRUE, n.boot = 10000,
                              replace = TRUE) {

  # Andel reproducerande par (med aktiva bon)
  repr.par <- with(dd, H1 + H2 + H3 + Hmin1 + Hmin2 + Hmin3)
  impr.par <- with(dd, K0)
  koll.par <- with(dd, repr.par + impr.par)
  and.repr.par <- with(dd, repr.par / koll.par)

  # Kullstorlek
  pull.sakra <- with(dd, H1 + H2*2 + H3*3)
  pull.sakra.n <- with(dd, H1 + H2 + H3)
  pull.alla <- with(dd, H1 + H2*2 + H3*3 + Hmin1 + Hmin2*2 + Hmin3*3)
  pull.alla.n <- with(dd, repr.par)
  pull.korr <- with(dd, pull.sakra + ((pull.sakra / pull.sakra.n) * (Hmin1 + Hmin2)))

  kullstorlek <- with(dd, pull.sakra / pull.sakra.n)

  # Produktivitet
  produktivitet <- with(dd, pull.alla / koll.par)
  produktivitet.korr <- with(dd, pull.korr / koll.par)

  out <- cbind(dd,
               repr.par, impr.par, koll.par, and.repr.par,
               pull.sakra, pull.sakra.n, pull.alla, pull.alla.n, pull.korr, kullstorlek,
               produktivitet, produktivitet.korr)

  # Extras on kullstorlek
  if (bootstrap.brood) {
    kullstorlek.boot <- t(sapply(1:nrow(dd), function(i)
      brood_size_boot(x=c(dd$H1[i], dd$H2[i], dd$H3[i]), n.boot=n.boot, replace=replace, plot=FALSE)))
    colnames(kullstorlek.boot) <- c("ks.q0.025", "ks.q0.5", "ks.q0.975")
    out <- cbind(out, kullstorlek.boot)
  }

  if (stats.brood) {
    # ks.sd, ks.se, ks.cv
    ks.sd <- with(dd, sqrt((H1*(1-kullstorlek)^2 + H2*(2-kullstorlek)^2 + H3*(3-kullstorlek)^2) / (pull.sakra.n - 1)))
    ks.se <- ks.sd / sqrt(pull.sakra.n)
    ks.cv <- ks.sd / kullstorlek
    out <- cbind(out, ks.sd, ks.se, ks.cv)
  }

  out
}

# Brood size, batches with 25 = set size to 25 in sample(x.bs, size=25, replace=TRUE)
#' @export
brood_size_boot <- function(x = c(24, 58, 9), n.boot = 10000,
                            q = c(0.025, 0.5, 0.975),
                            min = 15, size = sum(x),
                            replace = TRUE, plot = TRUE) {

  if (length(x) != length(c(1, 2, 3))) stop("Invalid input vector, must be of length 3")
  if (is.na(sum(x))) return(c(NA, NA, NA))
  if (sum(x) < min) {
    #stop("Too small sample for bootstrapping")
    return(c(NA, as.numeric((x[1] + x[2]*2 + x[3]*3) / sum(x)), NA))
  }
  # Bootstrap
  x.bs <- rep(c(1, 2, 3), x)
  z.bs <- replicate(n.boot, mean(sample(x.bs, size = size, replace = replace)))
  z <- quantile(z.bs, q)
  if (plot) {
    # Draw histogram
    hist(z.bs, breaks = 50, freq = FALSE, col = "steelblue",
         xlab = "Brood size", ylab = "Density", main = "Reference level, WTE brood size", font.lab = 2)
    abline(v = quantile(z.bs, c(0.025, 0.5, 0.975)), lty = 2, lwd = 2, col = c(1,2,1))
  }
  z
}

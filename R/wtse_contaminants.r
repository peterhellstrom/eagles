# Egg volume = (0.0373 x Length[mm] x Width[mm]) - 35.3; Stickel et al. (1973)
#' @export
egg_vol <- function(egg_length, egg_width,
                    vol_type = c("outer", "inner"),
                    shell_thickness = NULL,
                    a = 0.0373, b = 35.3) {

	vol_type <- match.arg(vol_type)
	if (vol_type == "outer") {
		shell_thickness <- 0
	} else if (vol_type == "inner" && is.null(shell_thickness)) {
		shell_thickness <- NA
	}

	(a * (egg_length - 2*shell_thickness) * (egg_width - 2*shell_thickness)) - b
}
#egg_vol(72, 58.85, "outer")
#egg_vol(72, 58.85, "inner", 0.6)
#egg_vol(72, 58.85, "inner")
#egg_vol(72, 58.85, "inner", 0)

# Egg shell index = (1000 * ShellWeight[g]) / (Length[mm] * Width[mm])
#' @export
egg_shell_index <- function(egg_length, egg_width, shell_weight) {
  (1000 * shell_weight) / (egg_length * egg_width) }

# Egg desiccation index = (EggWeight[g] - ShellWeight[g]) / [Inner] Egg Volume
#' @export
egg_desicc_index <- function(egg_weight, shell_weight, egg_volume) {
  (egg_weight - shell_weight) / egg_volume }

# Fettinnehåll i gram: EggFatG = (fprc * (EggWeight - ShellWeight)) / 100
#' @export
egg_fat_weight <- function(egg_weight, shell_weight, fprc) {
  (fprc * (egg_weight - shell_weight)) / 100 }

# Ursprunglig fett%: EggFatPrc = 100 * EggFatG / [Inner] Egg Volume
#' @export
egg_fprc_orig <- function(fat_weight, egg_volume) {
  100 * fat_weight / egg_volume }

# Embryonal development correction factor
# difficult if embryo has not been measured as a a numeric value,
# and just given as a symbolic value or in range
#' @export
egg_embryo_corr <- function(embryo_length, step = 75) {
	x <- readr::parse_number(embryo_length)
	ifelse(x >= step, step * (1/x), 1) }

# egg_embryo_corr(c(0, 30, 75, NA, 100, 110, ">85.45", "100m45", 1))
# Check: should this return NA or 1 if embryo.length is NA OR 0?
# Also, check variable embryo (Yes/No) and embryo.length = the latter should be 0 if embryo is No?
# What happens when multiplying measured values - or multiply only values where ecf < 1?

# Empirical support for the correction? Check relationship between FPRC and embryo length...
#curve(ifelse(x>=75, 75*(1/x), 1), from=0, to=160, n=500, ylim=c(0,1), bty="l",
#	xlab="Embryo length", ylab="Correction factor", font.lab=2, las=1)
#abline(v=75, lty=2, col="red")

# Transform measured values from fresh weight basis to lipid weight basis
# Ignore specified numeric value (e.g. -99.99), zeros and NAs
#' @export
fresh_to_lipid <- function(fw, FPRC, missing = -99.99) {
  ifelse(!fw %in% missing & !is.na(fw) & fw != 0, 100 * fw / FPRC, fw) }

#lipid_to_fresh <- function(lw, FPRC, missing=-99.99)

#blq <- function(z, missing = -99.99) {
	# Should also check that "" or NA is NA
	# 1) Change all values equal to -99.99 to NA
	#z[z == missing] <- NA
	# 2) Find all negative values (excluding -99.99) that is not NA
	#inds <- z < 0 & z != missing & !is.na(z)
	# 3) Use half the absolute value for all negative values
	#z[inds] <- abs(z[inds] / 2)
	#z
#}

#' @export
fix_missing <- function(x, missing = -99.99) {
	x[x %in% missing] <- NA
	x
}

#' @export
fix_below_loq <- function(x, missing = -99.99, rm.missing = TRUE) {
	if (rm.missing) {
		x <- fix_missing(x, missing)
	}
	inds <- x < 0 & !x %in% missing & !is.na(x)
	x[inds] <- abs(x[inds] / 2)
	x
}

# fix_below_loq_2 does not transform -99.99 (or another given value) to NA
# also, blq and fix_below_loq is faster
#' @export
fix_below_loq_2 <- function(x, missing = -99.99) {
  ifelse(x < 0 & !x %in% missing & !is.na(x), abs(x / 2), x) }

# take sum of a variable containing e.g. -99.99, i.e. treat a numeric value as NA
#' @export
sum_below_loq <- function(x, na.rm = TRUE, missing = -99.99) {
	z <- x[!is.na(x) & !x %in% missing]
	sum(abs(z[z < 0] / 2), na.rm = na.rm) + sum(z[z >= 0], na.rm = na.rm)
}

#' @export
sum_below_loq_2 <- function(x, missing = -99.99, na.rm = TRUE) {
  sum(fix_below_loq_2(ifelse(!x %in% missing & !is.na(x), x, NA)), na.rm = na.rm) }

# Examples:
#x <- c(5,-2,2,NA,-14,-99.99, -9)
#fix_missing(x)
#fix_missing(x, missing=c(-99.99, -9))
#fix_below_loq(x, rm.missing=TRUE)
#fix_below_loq(x, missing=c(-99.99, -9))
#fix_below_loq(x, rm.missing=FALSE)

#fix_below_loq(x)
#fix_below_loq_2(x)
#fix_below_loq_2(x, missing=c(-99.99, -9))

#sum_below_loq(x, missing=c(-99.99, -9))
#sum_below_loq_2(x, missing=c(-99.99, -9))

#' @export
sum_vars <- function(x, pattern, na.rm = TRUE, fix_loq = TRUE) {
	z <- x[grep(pattern, names(x))]
	if (fix_loq) {
		z <- z %>% mutate_at(vars(1:ncol(z)), funs(fix_below_loq(.)))
	}
	rowSums(z, na.rm = na.rm)
}

#' @export
round_dd <- function(x, outformat = c("dm", "dms"), digits = 0) {
    # input: x = latitude or longitude in decimal degrees
    deg <- as.integer(x)
    mins <- 60 * (x - deg)
    sec <- 60 * (mins - as.integer(mins))

    if (outformat == "dm") {
        mins <- round(mins, digits)
		tst <- mins < 60
		mins <- ifelse(tst, mins, 0)
		deg <- ifelse(tst, deg, deg + 1)
        paste0(deg, str_pad(mins, width = 2, pad = 0))
    } else if (outformat == "dms") {
		sec <- round(sec, digits)
		tst <- abs(sec - 60) > sqrt(.Machine$double.eps)
		sec <- ifelse(tst, sec, 0)
		mins <- ifelse(tst, mins, mins + 1)
		tst <- mins < 60
		mins <- ifelse(tst, mins, 0)
		deg <- ifelse(tst, deg, deg + 1)
        paste0(deg, stringr::str_pad(as.integer(mins), width = 2, pad = 0), stringr::str_pad(as.integer(sec), width = 2, pad = 0))
	}
}

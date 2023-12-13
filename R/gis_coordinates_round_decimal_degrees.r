#' @export
round_dd <- function(
    x,
    outformat = c("dm", "dms"),
    digits = 0) {
  # input: x = latitude or longitude in decimal degrees

  outformat <- match.arg(outformat)

  deg <- as.integer(x)
  mins <- 60 * (x - deg)
  sec <- 60 * (mins - as.integer(mins))

  if (outformat == "dm") {

    mins <- round(mins, digits)

    tst <- mins < 60
    mins <- dplyr::if_else(tst, mins, 0)
    deg <- dplyr::if_else(tst, deg, deg + 1)

    # deg <- dplyr::case_when(
    #   abs(deg) < 10 ~ stringr::str_pad(deg, width = 2, pad = 0),
    #   TRUE ~ as.character(deg)
    # )
    #
    # glue::glue(
    #   "{deg}{stringr::str_pad(mins, width = 2, pad = 0)}"
    # )

    stringr::str_c(
      stringr::str_pad(deg, width = 2, pad = 0),
      stringr::str_pad(mins, width = 2, pad = 0)
    )

  } else if (outformat == "dms") {

    sec <- round(sec, digits)

    tst <- abs(sec - 60) > sqrt(.Machine$double.eps)
    sec <- dplyr::if_else(tst, sec, 0)
    mins <- dplyr::if_else(tst, mins, mins + 1)
    tst <- mins < 60
    mins <- dplyr::if_else(tst, mins, 0)
    deg <- dplyr::if_else(tst, deg, deg + 1)

    # glue::glue(
    #   "{deg}{stringr::str_pad(as.integer(mins), width = 2, pad = 0)}{stringr::str_pad(as.integer(sec), width = 2, pad = 0)}"
    # )

    stringr::str_c(
      stringr::str_pad(deg, width = 2, pad = 0),
      stringr::str_pad(as.integer(mins), width = 2, pad = 0),
      stringr::str_pad(as.integer(sec), width = 2, pad = 0)
    )

  }
}
# This function does not currently deal well with negative decimal degrees.

# degrees should be returned with at least two digits, possible also three for longitude values
# round_dd(4.2, "dm") # Should return 0412, NOT 412
# round_dd(64.110602, "dm")
# round_dd(64.110602, "dms")
# round_dd(-50.55, "dm")
# round_dd(-50.55, "dms")
# round_dd(-50, "dm")
# round_dd(-15, "dm")
# round_dd(-151.01, "dm")
#
# round_dd(151.01, "dm")
# round_dd(151.01, "dms")

# celestial::deg2dms(-50.55)
# Should create function that back-transforms output from round_dd()!

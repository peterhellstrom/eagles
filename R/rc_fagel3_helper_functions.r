parse_ring_rc <- function(
    .x, pattern = "[a-zA-Z]+",
    width = 8, side = "left", pad = " ") {

  series <- stringr::str_extract(.x, pattern) |>
    stringr::str_trim()

  series_sequence <- stringr::str_remove(.x, pattern) |>
    stringr::str_trim()

  r_rc <- dplyr::case_when(
    is.na(series) ~ stringr::str_pad(
      series_sequence,
      width = width,
      side  = side,
      pad   = pad),
    TRUE ~ stringr::str_c(
      series,
      stringr::str_pad(
        series_sequence,
        side  = side,
        pad   = pad,
        width = width - nchar(series)
      )
    )
  )

  r_rc
}

# parse_ring_rc(c("N5431", "E21003", "9158957", "N543A", "ABX333", "N 2310"))

#' @export
coords_rc <- function(
    latitude,
    longitude,
    outformat = c("dm", "dms"),
    pad_output = TRUE,
    width = 6,
    pad = " ",
    side = "right",
    sep = " ") {

  lat_hemisphere <- dplyr::case_when(
    latitude >= 0 ~ "N",
    TRUE ~ "S"
  )

  lon_hemisphere <- dplyr::case_when(
    longitude >= 0 ~ "E",
    TRUE ~ "W"
  )

  round_input <- purrr::map_chr(
    c(latitude, longitude),
    \(x) round_dd(abs(x), outformat)
  )

  if (pad_output) {
    round_input <- purrr::map_chr(
      round_input,
      \(x) stringr::str_pad(
        x, width = width, pad = pad, side = side
      )
    )
  }

  stringr::str_c(
    round_input,
    c(lat_hemisphere, lon_hemisphere),
    collapse = sep
  )

}

# coords_rc(64.110602, 15.46875, "dm")
# coords_rc(64.110602, 15.46875, "dms")
# coords_rc(-64.110602, -15.46875, "dms")

#' @export
wtse_fagel_lanskod <- Vectorize(function(x, direction = c("from", "to")) {
	direction <- match.arg(direction)
	if (direction == "from") {
		switch(as.character(x), "1" = "AC", "2" = "BD", "3" = "M", "4" = "O", "B" = "AB", x)
	} else if (direction == "to") {
		switch(as.character(x), "AC" = "1", "BD" = "2", "M" = "3", "O" = "4", "AB" = "B", x)
	}
}, "x")

# wtse_fagel_lanskod(c("AC", "BD", "Z", "NO"), direction = "to")

#' @export
# Funktion som skapar ringnummer
gen_rc <- function(series, start, n, rc_format = TRUE) {
  if (rc_format) {
    sprintf(
      paste0(series, "   %s"),
      sprintf("%04d", seq(start, length = n))
    )
  } else {
    sprintf(
      paste0(series, "%s"),
      sprintf("%04d", seq(start, length = n))
    )
  }
}


# gen_rc("X", 0300, 100, TRUE)

# Går också att använda tidyr::full_seq
# stringr::str_c("N", stringr::str_pad(tidyr::full_seq(c(9900, 9925), 1), width = 7, pad = " "))
# glue::glue("N{stringr::str_pad(tidyr::full_seq(c(9900, 9925), 1), width = 7, pad = ' ')}")

#' @export
gen_ring_seq <- function(
    letter_start, num_start, num_end,
    width = 4, pad = "0") {
  stringr::str_c(
    letter_start,
    str_pad(num_start:num_end, width = width, pad = pad)
  )
}

#' @export
get_central <- function(.color2, .ring) {
  .first_pos <- stringr::str_sub({{.ring}}, 1, 1)

  dplyr::case_when(
    {{.color2}} == "ALU" & {{.first_pos}} == "E" ~ "SFH",
    {{.color2}} == "ALU" & {{.first_pos}} %in% c("X", "N", "P") ~ "SVS",
    {{.color2}} %in% c("ALU/RÖD", "ALU/SVART", "RÖD", "RÖD/SVART", "RÖD/ALU") ~ "SFH",
    {{.color2}} %in% c("ALU/BLÅ", "GRÖN", "SVART") ~ "SVS",
    {{.color2}} %in% c("BLÅ") ~ "NOS"
  )
}

#' @export
import_from_fagel3 <- function(
    con,
    sql_expr,
    as.is = TRUE,
    na_vec = c("", " ", "  "),
    clean_ring = FALSE,
    clean_names = TRUE,
    ...) {

  x <- RODBC::sqlQuery(con, sql_expr, as.is = TRUE) |>
    tibble::as_tibble() |>
    # dplyr::mutate(dplyr::across(tidyselect::where(is.character), \(x) na_if(x, "")))
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.character),
        \(x) dplyr::if_else(x %in% na_vec, NA_character_, x)
      )
    )

  if ("Datum" %in% names(x)) {
    x <- x |>
      dplyr::mutate(
        Datum = lubridate::ymd(str_sub(Datum, 1, 10))
      )
  }

  if (clean_ring) {
    if ("Ring" %in% names(x)) {
      x <- x |>
        # dplyr::mutate(Ring = stringr::str_replace_all(Ring, regex("\\s*"), ""))
        dplyr::mutate(
          Ring = stringi::stri_replace_all_charclass(Ring, "\\p{WHITE_SPACE}", "")
        )
    }
  }

  if (clean_names) {
    x <- x |>
      janitor::clean_names(...)
  }

  x
}

# To do: Add (optional) groups for seconds (and rename function to a more general name)
#' @export
parse_province_midpt <- function(
    .x,
    pattern = "(?<latd>[0-9]{2})(?<latm>[0-9]{2})(?<lath>[A-Z]{1})(?<lond>[0-9 ]+)(?<lonm>[0-9]{2})(?<lonh>[A-Z]{1})",
    col_names = c("lat_dd", "lon_dd")) {

  .x |>
    stringr::str_match(pattern) |>
    data.frame() |>
    dplyr::rename("text_dm" = "V1") |>
    dplyr::mutate(
      dplyr::across(c(latd:latm, lond:lonm), as.integer),
      lat_dd = latd + latm/60,
      lon_dd = lond + lonm/60
    ) |>
    dplyr::mutate(
      lat_dd = dplyr::case_when(
        lath == "S" ~ -lat_dd,
        lath == "N" ~ lat_dd,
        TRUE ~ NA_real_),
      lon_dd = dplyr::case_when(
        lonh == "W" ~ -lon_dd,
        lonh == "E" ~ lon_dd,
        TRUE ~ NA_real_
      )
    ) |>
    dplyr::select(lat_dd, lon_dd) |>
    rlang::set_names(col_names)
}

# parse_province_midpt(
#   c("6455N 1800E", "1203S 01201W"),
#   col_names = c("lat", "lon")
# )

# Skapa redovisningsfil
# OBS! Saknar "huvud" med system- och databasuppgifter, samt sortering skiljer sig!
# Fagel3 sorterar inte efter v, w utan w sorteras "inom" v
#
# "Kortnamn" i  redovisningsfil samt motsvarande tabell i RingDb.mdb:
# M = Märkare
# H = Medhjälpare
# S = Signaturer
# L = Lokaler
# C = Kontr
# K = Kullar
# R = Ringon
# E = Ringar

#' @export
fagel3_redovisn_fil <- function(
    mnr = 0658,
    year_filter = 2022,
    .markare = markare,
    .medhjalpare = medhjalpare,
    .signaturer = signaturer,
    .lokaler = lokaler,
    .kontr = kontr,
    .kullar = kullar,
    .ringon = ringon,
    .ringar = ringar,
    export = TRUE) {

  x <- dplyr::bind_rows(
    df_c(
      markare,
      .prefix = "M"),
    df_c(
      medhjalpare |>
        dplyr::filter(mnr == mnr) |>
        dplyr::select(-lop_nr) |>
        dplyr::arrange(efternamn, fornamn),
      .prefix = "H"),
    df_c(
      signaturer |>
        dplyr::filter(mnr == mnr) |>
        dplyr::arrange(signatur),
      .prefix = "S"),
    df_c(
      lokaler |>
        dplyr::filter(mnr == mnr) |>
        dplyr::arrange(lokal) |>
        dplyr::mutate(
          dplyr::across(
            latitud:longitud,
            \(x) str_replace(round(x, 5), "\\.", ",")
          )
        ),
      .prefix = "L"),
    df_c(
      kontr |>
        dplyr::select(-lop_nr) |>
        dplyr::filter(
          stringr::str_detect(datum, year_filter)
        ),
      .prefix = "C"),
    df_c(
      kullar |>
        dplyr::filter(
          stringr::str_detect(datum, year_filter)
        ) |>
        dplyr::arrange(id_kull),
      .prefix = "K"),
    df_c(
      ringon |>
        dplyr::filter(
          stringr::str_detect(datum, year_filter)
        ) |>
        dplyr::arrange(ring),
      .prefix = "R"),
    df_c(
      ringar |>
        dplyr::filter(mnr == mnr) |>
        dplyr::select(-lager_id) |>
        dplyr::arrange(f_num) |>
        dplyr::mutate(
          slut = dplyr::case_when(
            slut == 1 ~ "True",
            TRUE ~ "False")
        ),
      .prefix = "E"))

  if (export) {
    out_file <- glue::glue("{mnr}år-{year_filter}")
    out_file <- stringr::str_c(add_timestamp(out_file), ".txt")
    readr::write_delim(
      x,
      out_file,
      quote = "none",
      col_names = FALSE)
  }

  x
}

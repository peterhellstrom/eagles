# This function uses the structure of project rings
# from 2012 and onwards, i.e. with a leading country letter.
# Other ways of generating series were used in 2008-2011,
# when more than 1000 rings were made yearly. Investigate and adapt
# the function to allow for these years as well!

#' Title
#'
#' @param country
#' @param .v
#'
#' @returns
#' @export
#'
#' @examples
proj_ring_gen <- function(
    country,
    .v = LETTERS[-c(9, 15, 17, 18, 21, 25)]) {

  # S000-S999, Rings 1-1000
  pt0 <- paste0(country, sprintf("%03d", 0:999))

  # base sequence for the numeric part of the series combined with a letter
  b <- sort(c(outer(.v, sprintf("%02d", 0:99), paste0)))
  b_s <- "(\\w+)(\\w+)(\\w+)"

  # LETTER at third position (first column in second row) x[2,1].
  # Rings 1001-3000
  pt1 <- paste0(country, gsub(b_s, "\\2\\1\\3", b))
  # LETTER at second position (second column in first row) x[1,2].
  # Rings 3001-5000
  pt2 <- paste0(country, gsub(b_s, "\\1\\2\\3", b))
  # LETTER at fourth position (second column in second row) x[2,2].
  # Rings 5001-7000
  pt3 <- paste0(country, gsub(b_s, "\\2\\3\\1", b))

  c(pt0, pt1, pt2, pt3)
}


#' Title
#'
#' @param .x
#' @param .v
#' @param seq_start
#'
#' @returns
#' @export
#'
#' @examples
proj_ring_numeric <- function(
    .x,
    .v = LETTERS[-c(9, 15, 17, 18, 21, 25)],
    seq_start = 1
) {

  # text_part <- str_replace_all(.x, "[:digit:]", "")
  # Assume that first position ALWAYS is the country code,
  # so exclude str_sub(.x, 1, 1)
  serial_part <- stringr::str_sub(.x, 2)
  text_part <- stringr::str_extract(serial_part, paste0(.v, collapse = "|"))
  num_part <- as.numeric(stringr::str_replace_all(.x, "[:alpha:]", ""))

  serial_num_part <- 100 * (base::match(text_part, .v) - 1)
  serial_num_part <- tidyr::replace_na(serial_num_part, 0) + num_part

  .p <- stringr::str_locate_all(.x, "[:alpha:]")
  .z <- purrr::map_chr(
    seq_along(.p), ~ stringr::str_c(.p[[.x]][,1], collapse = "")
  )

  text_num_part <- dplyr::case_when(
    .z == "1" ~ 0,
    .z == "13" ~ 1000,
    .z == "12" ~ 3000,
    .z == "14" ~ 5000,
    TRUE ~ NA_real_
  )

  (text_num_part + serial_num_part) + seq_start

}

# Create lagged data set, and test if "previous row" is identical to
# current.

#' Title
#'
#' @param .x
#' @param ...
#' @param group_variable
#'
#' @returns
#' @export
#'
#' @examples
add_ring_group <- function(.x, ..., group_variable = grp) {
  .x |>
    dplyr::group_by(...) |>
    dplyr::mutate(
      {{ group_variable }} := dplyr::cur_group_id()
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      {{ group_variable }} :=
        base::cumsum(
          {{ group_variable }} != dplyr::lag( {{ group_variable }}, default = 1)
        ) + 1
    )
}

#' Title
#'
#' @param .x
#' @param ...
#' @param ring
#' @param group_variable
#'
#' @returns
#' @export
#'
#' @examples
add_ring_group_by_number <- function(.x, ..., ring, group_variable = grp) {
  .x |>
    dplyr::group_by(...) |>
    dplyr::mutate(
      ring_series = stringr::str_sub( {{ ring }} , 1, 1),
      ring_serial = readr::parse_number( {{ ring }} ),
      {{ group_variable }} :=
        base::cumsum(
          (ring_serial - dplyr::lag(ring_serial, default = 1)) != 1
        )
    )
}

# Format site codes to common format

#' Title
#'
#' @param .x
#' @param pattern
#'
#' @returns
#' @export
#'
#' @examples
wtse_sites_str <- function(.x, pattern = "([0-9]{1,3})") {
  .x |>
    # remove all spaces
    stringr::str_replace_all("[[:blank:]]", "") |>
    # Extract and pad numeric part
    stringr::str_replace_all(
      pattern,
      \(x) stringr::str_pad(x, width = 3, pad = "0")
    ) |>
    # remove any trailing inland "i"
    stringr::str_replace("([i]{1}$)", "") |>
    stringr::str_to_upper() |>
    stringr::str_trim()
}

#' Title
#'
#' @param .x
#'
#' @returns
#' @export
#'
#' @examples
wtse_sites_str_old <- function(.x) {
  .x |>
    stringr::str_replace_all("[[:blank:]]", "") |>
    stringr::str_sub(2) |>
    stringr::str_remove_all("^0+(?!$|([A-Za-z]{1}))") |>
    stringr::str_replace_all("\\/0+", "/") |>
    stringr::str_c(
      stringr::str_sub(.x, 1, 1), ...=_
    )
}

# x <- tibble::tibble(lokalkod = c("E2/4", " H7b   ", "C98i", "C98 i", "C9 8 i", "C3B2", "X4", "X004", "x4b", "X4b", "C3/15"))
# x |>
#   dplyr::mutate(lokalkod_test = wtse_sites_str(lokalkod) |> wtse_sites_str(.))
# OBS! Denna funktion gör inga förändringar för koder som C315 och C917!
#
# .x <- c("B1", "B11", "B11B", "B111", "E2/4", "C3/15", "C3B2", "B1 ", "B102i")
# wtse_sites_str(.x)
#
# Change "the other way", i.e. remove zeros
# .x <- c("B020", "B020B", "B001", "B011", "B011B", "B111", "E002/004", "C003/015", "C003B2", "B001 ", "B100", "B101", "B102i")
# wtse_sites_str_old(.x)
#
# Extract numeric part
# .x |> stringr::str_extract_all("[[:digit:]]+")
#
# Extract characters:
# 0*(\\d+)

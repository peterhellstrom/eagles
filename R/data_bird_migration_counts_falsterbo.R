#' @export
month_switch <- function(month) {
  sapply(
    month, switch,
    "januari" = 1,
    "februari" = 2,
    "mars" = 3,
    "april" = 4,
    "maj" = 5,
    "juni" = 6,
    "juli" = 7,
    "augusti" = 8,
    "september" = 9,
    "oktober" = 10,
    "november" = 11,
    "december" = 12)
}

#' @export
reshape_species_year <- function(x) {

  names_x <- names(x)
  names(x) <- c(names(x)[[1]], "count")

  x <- x |>
    dplyr::mutate(month = names_x[[1]]) |>
    dplyr::relocate(month) |>
    dplyr::rename(day = names_x[1]) |>
    dplyr::mutate(
      day = strtoi(day),
      count = dplyr::na_if(count, "-"),
      count = stringr::str_replace_all(count, "\\s+", ""),
      count = strtoi(count))
  x
}

#' @export
falsterbo_species_year <- function(species, year, convert_date = TRUE) {

  url <- glue::glue(
    "https://www.falsterbofagelstation.se/strack/art-ar/?lang=sv&art={utils::URLencode(species)}&year={year}"
  )

  x <- url |>
    xml2::read_html() |>
    rvest::html_elements("table") |>
    rvest::html_table()

  x_counts <- purrr::map_dfr(x, reshape_species_year) |>
    dplyr::mutate(species = species, year = year) |>
    dplyr::relocate(species, year)

  if (convert_date) {
    x_counts <- x_counts |>
      dplyr::mutate(
        month = month_switch(month),
        date = stringr::str_c(year, month, day, sep = "-"),
        date = lubridate::ymd(date)) |>
      dplyr::relocate(date, .before = count)
  }

  x_counts |>
    dplyr::filter(!is.na(day) & !is.na(date))
}

# Q: Deal with 0 and NA?
#' @export
falsterbo_species_all_years <- function(species, replace_na = TRUE) {

  url <- glue::glue(
    "https://www.falsterbofagelstation.se/strack/art-alla-ar/?lang=sv&art={utils::URLencode(species)}"
  )

  x <- url |>
    xml2::read_html() |>
    rvest::html_elements("table") |>
    rvest::html_table()

  x <- x[-1]

  x <- purrr::map(
    x, \(x) x |>
      dplyr::mutate(
        Summa = stringr::str_replace_all(Summa, "\\s+", ""),
        Summa = strtoi(Summa)))

  x <- x |>
    dplyr::bind_rows() |>
    dplyr::arrange(År) |>
    dplyr::rename(year = År, count = Summa) |>
    dplyr::mutate(species = species) |>
    dplyr::relocate(species)

  if (replace_na) {
    x <- x |>
      dplyr::mutate(count = tidyr::replace_na(count, 0))
  }
  x
}

#' @export
falsterbo_year <- function(year) {

  url <- glue::glue(
    "https://www.falsterbofagelstation.se/strack/ar/?lang=sv&year={year}"
  )

  x <- url |>
    xml2::read_html() |>
    rvest::html_elements("table") |>
    rvest::html_table(
      trim = TRUE,
      na.strings = c(" ", "-"),
      convert = TRUE
    )

  x <- x[[1]] |>
    dplyr::mutate(year = year) |>
    dplyr::relocate(year)

  x |>
    dplyr::mutate(
      dplyr::across(augusti:tidyselect::last_col(), \(x) dplyr::na_if(x, "-")),
      dplyr::across(augusti:tidyselect::last_col(), \(x) stringr::str_replace_all(x, "\\s+", "")),
      dplyr::across(augusti:tidyselect::last_col(), strtoi)
    ) |>
    dplyr::rename(
      species = Art,
      count = Summa,
      average = `Medeltal per år`
    )
}

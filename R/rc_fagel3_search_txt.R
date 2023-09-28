#' @export
rc_data_search_txt <- function(
    pattern,
    path_pattern = ".*RCdata\\.txt$",
    fixed = FALSE,
    markers = FALSE) {

  x <- sifr::sif(
    pattern = pattern, path_pattern = path_pattern,
    fixed = fixed, markers = markers)

  x_tbl <- x |>
    tibble::as_tibble()

  x_tbl

}

#' @export
rc_ringon_search_txt <- function (
    species_code,
    year = NULL,
    pattern_sites = "^L\\|",
    fixed = FALSE,
    markers = FALSE,
    empty_ringondb = "W:/projects/data/ringing/RingDb_tom_2018-10-15.mdb"
) {

  pattern_broods <- glue::glue("^K.*{species_code}")
  pattern_ringon <- glue::glue("^R.*{species_code}")
  path_pattern <- dplyr::case_when(
    !is.null(year) ~ stringr::str_c(".*år\\-", year, "\\.txt$"),
    TRUE ~ ".*år\\-.*\\.txt$",
  )

  broods <- sifr::sif(
    pattern_broods, path_pattern = path_pattern,
    fixed = FALSE, markers = FALSE)

  ringon <- sifr::sif(
    pattern_ringon, path_pattern = path_pattern,
    fixed = FALSE, markers = FALSE)

  broods_tbl <- broods |>
    tibble::as_tibble() |>
    dplyr::filter(!stringr::str_detect(path, "old"))

  ringon_tbl <- ringon |>
    tibble::as_tibble() |>
    dplyr::filter(!stringr::str_detect(path, "old"))

  sites_files <- ringon_tbl |>
    dplyr::select(path) |>
    dplyr::distinct() |>
    dplyr::mutate(
      dir = dirname(path),
      file = stringr::str_c(".*", basename(path))
    )

  # Only read last file
  # But could be later versions, for instance if ringer did
  # not ring any birds of species x in, say, 2021 - but in 2022.
  # It's still the 2021 file that get's imported here.
  sites_files <- sites_files |>
    dplyr::arrange(dir, path) |>
    dplyr::slice_tail(by = dir)

  sites_tbl <- purrr::map_dfr(
    sites_files$file,
    \(x) {
      sifr::sif(
        pattern = pattern_sites, path_pattern = x,
        fixed = FALSE, markers = FALSE) |>
        tibble::as_tibble()
    }
  )

  con <- RODBC::odbcConnectAccess2007(empty_ringondb)
  k <- RODBC::sqlQuery(con, "SELECT * FROM Kullar") |> names()
  r <- RODBC::sqlQuery(con, "SELECT * FROM Ringon") |> names()
  l <- RODBC::sqlQuery(con, "SELECT * FROM Lokaler") |> names()
  close(con)

  broods_sep <- broods_tbl |>
    dplyr::select(contents) |>
    tidyr::separate_wider_delim(
      contents, "|",
      names = c(NA, k, NA),
      too_few = "debug") |>
    dplyr::mutate(
      Datum = as.Date(Datum),
      dplyr::across(
        tidyselect::where(is.character), \(x) dplyr::na_if(x, "")
      )
    )

  ringon_sep <- ringon_tbl |>
    dplyr::select(contents) |>
    tidyr::separate_wider_delim(
      contents, "|",
      names = c(NA, r, NA),
      too_few = "debug"
    )

  sites_sep <- sites_tbl |>
    dplyr::select(contents) |>
    tidyr::separate_wider_delim(
      contents, "|",
      names = c(NA, l, NA),
      too_few = "debug"
    )

  sites_sep <- sites_sep |>
    dplyr::mutate(
      dplyr::across(
        Latitud:Longitud,
        \(x) stringr::str_replace(x, ",", ".") |>
          as.numeric()
      ),
      dplyr::across(
        tidyselect::where(is.character),
        \(x) dplyr::na_if(x, "")
      )
    )

  sites_sep <- sites_sep |>
    dplyr::semi_join(ringon_sep, join_by(Lokal))

  out <- list(
    broods = broods,
    ringon = ringon,
    broods_tbl = broods_tbl,
    ringon_tbl = ringon_tbl,
    broods_sep = broods_sep,
    ringon_sep = ringon_sep,
    sites_files = sites_files,
    sites_tbl = sites_tbl,
    sites_sep = sites_sep
  )

  out
}

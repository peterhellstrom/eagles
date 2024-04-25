#' @export
get_nvrid_wkt <- function(
    nvrid,
    beslutsstatus = "Gällande",
    crs = 3006,
    crs_to = NULL,
    map = FALSE,
    fill = TRUE,
    base_url = "https://geodata.naturvardsverket.se/naturvardsregistret/rest/v3/omrade"
) {

  wkt_str <- glue::glue("{base_url}/{nvrid}/{beslutsstatus}/wkt")
  wkt_resp <- purrr::map_chr(wkt_str, \(x) readLines(x, warn = FALSE))

  xy <- sf::st_as_sfc(wkt_resp, crs = crs) |>
    sf::st_as_sf() |>
    dplyr::mutate(
      nvrid = nvrid,
      beslutsstatus = beslutsstatus)

  sf::st_geometry(xy) <- "geom"

  if (!is.null(crs_to)) {
    xy <- xy |> sf::st_transform(crs_to)
  }

  if (map) {
    # Leaflet
    m <- leaflet::leaflet(
      data = xy |> sf::st_transform(4326)) |>
      leaflet::addTiles() |>
      leaflet::addPolygons(fill = fill)
    # Mapview
    # m <- mapview::mapview(xy)
    m
  } else {
    xy
  }
}

#' @export
nv_rest_api <- function(
    str_parameters,
    ...,
    base_url = "https://geodata.naturvardsverket.se/naturvardsregistret/rest/v3",
    remove_atom_link = FALSE,
    convert_unix_timestamp = TRUE) {

  p <- with(list(...), glue::glue(str_parameters))
  rest_url <- glue::glue("{base_url}/{p}")

  x <- purrr::map_dfr(
    rest_url,
    \(x) jsonlite::fromJSON(txt = x)
  ) |>
    tibble::as_tibble()

  if (convert_unix_timestamp) {
    # UNIX timestamp is in milliseconds
    x <- x |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::contains("datum") & tidyselect::where(is.numeric),
          \(x) lubridate::as_datetime(x/1000, tz = "Europe/Stockholm") |>
            lubridate::as_date()
          # \(x) anytime::anydate(x/1000)
        )
      )
  }

  if (remove_atom_link) {
    x <- x |>
      dplyr::select(-atom.link)
  }

  x |>
    dplyr::distinct()
}

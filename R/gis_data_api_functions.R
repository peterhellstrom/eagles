#' @export
get_nvrid_wkt <- function(
    nvrid, beslutsstatus = "Gällande",
    crs = 3006,
    crs_to = NULL,
    map = FALSE, fill = TRUE) {

  wkt_str <- glue("https://geodata.naturvardsverket.se/naturvardsregistret/rest/v3/omrade/{nvrid}/{beslutsstatus}/wkt")
  wkt_resp <- sapply(wkt_str, readLines, warn = FALSE)

  xy <- st_as_sfc(wkt_resp, crs = crs) %>%
    st_as_sf() %>%
    mutate(
      nvrid = nvrid,
      beslutsstatus = beslutsstatus)

  st_geometry(xy) <- "geom"

  if (!is.null(crs_to)) xy <- xy %>% st_transform(crs_to)

  if (map) {
    # Leaflet
    m <- leaflet(data = xy %>% st_transform(4326)) %>%
      addTiles() %>%
      addPolygons(fill = fill)
    # Mapview
    #m <- mapview(xy)
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
    remove_atom_link = FALSE) {

  p <- with(list(...), glue::glue(str_parameters))
  rest_url <- glue::glue("{base_url}/{p}")

  #x_raw <- lapply(rest_url, jsonlite::fromJSON)
  #x <- x_raw %>% tibble::as_tibble()
  x <- map_dfr(rest_url, ~ jsonlite::fromJSON(txt = .x)) %>%
    tibble::as_tibble()

  if (remove_atom_link) {
    x %>% select(-atom.link)
  }

  x %>% distinct()
}

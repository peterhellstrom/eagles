# Höjddata ----
#' @export
extract_ruta_100 <- function(x) {
  stringr::str_c(
    stringr::str_sub(x, 1, 2),
    stringr::str_sub(x, 5, 5),
    sep = "_")
}

#' @export
extract_coords_from_ruta_5 <- function(x) {

  ymin <- stringr::str_c(
    stringr::str_sub(x, 1, 3),
    stringr::str_sub(x, 8, 8),
    "000")

  xmin <- stringr::str_c(
    stringr::str_sub(x, 5, 6),
    stringr::str_sub(x, 9, 9),
    "000")

  data.frame(
    xmin = as.numeric(xmin),
    ymin = as.numeric(ymin))
}

# https://download-ver.lantmateriet.se/hojdmodell/wcs/v1?service=WCS&version=2.0.1&request=DescribeCoverage&coverageid=hojdgrid_1m
#' @export
lm_hojdmodell_url <- function(
    crs, xmin, ymin, xmax, ymax,
    url_base = "https://download-ver.lantmateriet.se/hojdmodell/wcs/v1") {

  httr::modify_url(
    url = url_base,
    query = list(
      service = "WCS",
      version = "2.0.1",
      request = "GetCoverage",
      coverageid = "hojdgrid_1m",
      subset = glue::glue("y,epsg:{crs}({ymin},{ymax})"),
      subset = glue::glue("x,epsg:{crs}({xmin},{xmax})"),
      format = "image/tiff"
    )
  )
}

# Note: use str_c() instead of file.path()
# to create paths, only way (?) if we want to include
# the optional argument main_dir = NULL
#' @export
lm_hojdmodell_prepare <- function(
    .x, ruta = ruta_5, dx = 5000, dy = 5000,
    main_dir = NULL) {

  .x |>
    dplyr::mutate(
      ruta_100 = extract_ruta_100({{ruta}}),
      crs = 3006,
      extract_coords_from_ruta_5({{ruta}}),
      xmax = xmin + dx,
      ymax = ymin + dy) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      file_dl = stringr::str_c(
        main_dir,
        ruta_100,
        stringr::str_c(ruta_5, ".tif"),
        sep = "/"),
      url_dl = lm_hojdmodell_url(
        crs = crs,
        xmin = xmin, ymin = ymin,
        xmax = xmax, ymax = ymax)
    )
}

#' @export
lm_hojdmodell_prepare_sf <- function(
    .x, dx = 5000, dy = 5000, crs = 3006) {
  purrr::map2(
    .x$xmin, .x$ymin,
    \(x, y) grid_cell(x, y, delta_x = dx, delta_y = dy)) |>
    sf::st_sfc(crs = crs) |>
    sf::st_sf(.x, geometry = _)
}

#' @export
lm_hojdmodell_download <- function(
    .x,
    user = Sys.getenv("lm_atkomst_ver_user"),
    password = Sys.getenv("lm_atkomst_ver_pwd")) {

  # Create download directories
  dl_dirs <- .x |>
    dplyr::select(file_dl) |>
    dplyr::mutate(file_dl = dirname(file_dl)) |>
    dplyr::distinct() |>
    dplyr::pull()

  fs::dir_create(dl_dirs)

  # Download data
  # Does remotes::download has an overwrite option?
  purrr::walk2(
    .x = .x$url_dl,
    .y = .x$file_dl,
    \(x,y) {
      remotes:::download(
      path = y,
      url = x,
      basic_auth = list(
        user = user,
        password = password))
    }
  )
}

# create virtual raster (vrt) from downloaded tif-files
#' @export
lm_hojdmodell_vrt <- function(
    .data,
    main_dir,
    out_vrt) {

  .data |>
    hojdmodell_prepare(main_dir = main_dir) |>
    dplyr::filter(file_exists(file_dl)) |>
    dplyr::pull(file_dl) |>
    gdalbuildvrt(out_vrt) |>
    gdalinfo(mm = TRUE, stats = TRUE, hist = TRUE)
}

# Retrieve token ----

# 1) consumer_key and consumer_secret are generated in the API portal for a
# specified app.
# 2) then stored locally as environment variables; consumer_key and consumer_secret
# use e.g. cmd in Windows, setx variable value /M
#' @export
lm_get_token <- function(
    url = "https://api.lantmateriet.se/token",
    consumer_key = Sys.getenv("consumer_key"),
    consumer_secret = Sys.getenv("consumer_secret")
) {

  # basic_auth <- glue::glue("Basic {RCurl::base64(glue::glue('{consumer_key}:{consumer_secret}'))}") |>
  #   rlang::set_names("Authorization")

  consumer_key_secret <- paste(consumer_key, consumer_secret, sep = ":")
  basic_auth <- c("Authorization" = paste("Basic", RCurl::base64(consumer_key_secret), sep = " "))

  httr::POST(
    url = url,
    config = httr::add_headers(
      .headers = basic_auth),
    body = 'grant_type=client_credentials')
}

# httr::content(lm_get_token())$access_token

add_headers_token <- function(token, sep = " ") {
  c('Authorization' = paste("Bearer", token, sep = sep))
}

# Markhöjd Direkt ----
#' @export
lm_markhojd_point <- function(
    east,
    north,
    altitude_only = FALSE,
    as = "text",
    url = "https://api.lantmateriet.se/distribution/produkter/hojd/v1/rest/api/hojd/3006",
    token = httr::content(lm_get_token())$access_token) {

  out <- httr::GET(
    url = file.path(url, east, north),
    config = httr::add_headers(
      .headers = add_headers_token(token)
    )
  )

  if (!altitude_only) {
    httr::content(out, as = as)
  } else {
    httr::content(out)$geometry$coordinates[[3]]
  }
}

#' @export
lm_markhojd_geometry <- function(
    geometry,
    encode = "raw",
    as = "text",
    url = "https://api.lantmateriet.se/distribution/produkter/hojd/v1/rest/api/hojd",
    token = httr::content(lm_get_token())$access_token) {

  out <- httr::POST(
    url = url,
    config = httr::add_headers(
      .headers = c(
        add_headers_token(token),
        'Accept' = 'application/json')
    ),
    httr::content_type_json(),
    body = geometry,
    encode = encode)

  httr::content(out, as = as)
}

# Ortnamn Direkt ----

#' @export
lm_ortnamn_kriterier <- function(
    query,
    as = "text",
    url = "https://api.lantmateriet.se/distribution/produkter/ortnamn/v2.1",
    token = httr::content(lm_get_token())$access_token) {

  out <- httr::GET(
    url = httr::modify_url(file.path(url, "kriterier"), query = query),
    config = httr::add_headers(
      .headers = add_headers_token(token)
    )
  )

  httr::content(out, as = as)
}

# two different id columns - why?
#' @export
lm_ortnamn_unnest_parsed <- function(x, crs = 3006) {
  tibble::tibble(x = x)[5,] |>
    tidyr::unnest(x) |>
    tidyr::unnest_wider(x) |>
    tidyr::unnest_wider(properties, names_sep = "_") |>
    tidyr::unnest(properties_placering) |>
    tidyr::unnest_wider(properties_placering) |>
    tidyr::unnest_wider(punkt, names_sep = "_") |>
    tidyr::unnest_wider(punkt_coordinates, names_sep = "_") |>
    dplyr::select(-type, -bbox, -geometry, -punkt_type) |>
    dplyr::rename(namn = properties_namn, sprak = properties_sprak) |>
    sf::st_as_sf(
      coords = c("punkt_coordinates_1", "punkt_coordinates_2"),
      crs = crs) |>
    dplyr::group_by(properties_id)
}

# type
# crs
## type
# bbox
# totaltAntal
# features
## type
## bbox
## id
## geometry
## properties
### id
### namn
### sprak
### placering
#### lankod
#### lannamn
#### kommunkod
#### kommunnamn
#### sockenstadkod
#### sockenstadnamn
#### namntyp
#### punkt
##### coordinates
##### type

# Wrapper function
#' @export
lm_ortnamn_coords <- function(
    easting,
    northing,
    crs = 3006,
    token = httr::content(lm_get_token())$access_token) {

  lm_ortnamn_kriterier(
    token = token,
    query = list(
      punkt = stringr::str_c(northing, easting, sep = ","),
      punktSrid = crs),
    as = "parsed") |>
    lm_ortnamn_unnest_parsed()
}

library(tidyverse)
library(sf)
library(rvest)
library(xml2)
library(httr)
library(jsonlite)
library(geojsonsf)
library(geojson)
library(mapview)
library(eagles)

# https://en.wikipedia.org/wiki/Geography_Markup_Language

# Retrieve token ----

# 1) consumer_key and consumer_secret are generated in the API portal for a
# specified app.
# 2) then stored locally as environment variables; consumer_key and consumer_secret
# use e.g. cmd in Windows, setx variable value /M

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

# https://www.lantmateriet.se/globalassets/geodata/geodatatjanster/pb_ortnamn_direkt_v2.1_2.3.pdf
# https://www.lantmateriet.se/globalassets/geodata/geodatatjanster/tb_ortnamn-direkt_v2.1.0_1.0.pdf

## GET /{id} ----

tmp_ortnamn_id <- function(
    id,
    url = "https://api.lantmateriet.se/distribution/produkter/ortnamn/v2.1",
    token = httr::content(eagles::lm_get_token())$access_token
    ) {

  out <- httr::GET(
    url = file.path(url, id),
    config = httr::add_headers(
      .headers = eagles:::add_headers_token(token),
      'Accept' = 'application/json'
    )
  )

  x_parsed <- httr::content(out, as = "parsed")
  x_text <- httr::content(out, as = "text")
  x_text_2 <- jsonlite::fromJSON(x_text)

  p_crs <- x_text_2$crs$properties$name

  p_1 <- x_text_2$features$properties[1:3] |>
    as_tibble()

  p_2 <- x_text_2$features$properties$placering[[1]] |>
    as_tibble() |>
    unnest_wider(punkt) |>
    unnest_wider(coordinates, names_sep = "_") |>
    unnest_wider(coordinates_1, names_sep = "_") |>
    rename(easting = coordinates_1_1, northing = coordinates_1_2)

  p_1_2 <- p_1 |> bind_cols(p_2)

  p_1_2_sf <- p_1_2 |>
    sf::st_as_sf(
      coords = c("easting", "northing"),
      crs = as.numeric(str_sub(p_crs, -4)),
      remove = FALSE)

  p_1_2_sf
}


tmp_ortnamn_id(3723317)

ids <- c(3541911, 4341383, 3723315, 3723317, 4505739, 3837565, 3547081, 3909951)

map(ids, tmp_ortnamn_id) |>
  bind_rows()

map_dfr(ids, tmp_ortnamn_id)

map(ids, tmp_ortnamn_id) |>
  list_rbind() |>
  st_sf()

sf::st_bbox(p_1_2_sf)
x_text_2$bbox

mapview(p_1_2_sf)

x |> html_attr("gml:Point")
x |> html_nodes("*")

xml_name(x)
xml_children(x)
xml_text(x)
xml_find_all(x, "//Punkt")

xml_structure(x)

glimpse(x$features[[1]]$properties$placering)

str(x$features)

str(x)

tibble(
  data = x$features[[1]]$properties[1:3]
) |>
  unnest_longer(data)

x_placering <- tibble(
  data = x$features[[1]]$properties$placering
  ) |>
  unnest_wider(data) |>
  unnest_wider(punkt) |>
  unnest_wider(coordinates, names_sep = "_") |>
  rename(easting = coordinates_1, northing = coordinates_2) |>
  st_as_sf(coords = c("easting", "northing"), crs = 3006)

mapview(x_placering)

eagles::lm_basemaps(x_placering)

geojson_sf(x)
writeLines(x, "tmp.json")

write_xml(x, "tmp.gml")
b <- st_read("tmp.gml")
b

## POST /

## GET /kriterier ----
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

## GET /referens/fritext ----

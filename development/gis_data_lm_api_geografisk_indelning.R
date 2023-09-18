library(tidyverse)
library(sf)
library(httr)
library(jsonlite)

lm_geografisk_indelning <- function(
    as = "text",
    url = "https://api.lantmateriet.se/distribution/produkter/geografiskindelning/v2/Län/25?includeData=oversiktligGeometri&srid=3006",
    token = httr::content(lm_get_token())$access_token) {

  out <- httr::GET(
    url = url,
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

# GET -----
base_url <- "https://api.lantmateriet.se/distribution/produkter/geografiskindelning/v2"
url <- "Län/25?includeData=oversiktligGeometri&srid=3006"
token <- httr::content(eagles::lm_get_token())$access_token

out <- httr::GET(
  url = file.path(base_url, url),
  config = httr::add_headers(
    .headers = eagles:::add_headers_token(token),
    'Accept' = 'application/json'
  )
)

out
httr::content(out, as = "text")
b <- httr::content(out, as = "text")
b_parsed <- jsonlite::fromJSON(b)
str(b_parsed)

str(b_parsed$features$properties)

b_parsed$features$properties |>
  as_tibble() |>
  unnest_wider(lansyta) |>
  unnest_wider(coordinates, names_sep = "_") |>
  unnest_wider(coordinates_1, names_sep = "_") |>
  unnest_wider(coordinates_1_1, names_sep = "_")

dplyr::glimpse(b)
writeLines(b, "tmp.gml")

# replacing null geometries with empty geometries

b_sf <- sf::read_sf("tmp.json") # No geometry column (but coordinates end up in column lansyta)
b_sf <- sf::read_sf("tmp.gml") # Works
b_sf <- sf::read_sf(b) # Does not work
g_geoj_sf <- geojsonsf::geojson_sf(b)
b1 <- jsonlite::fromJSON(b)
dplyr::glimpse(b1)
sf::st_read(b1$features$properties)
glimpse(b1)
# POST ----

geometry <- '"
{
 "geometri": {
 "type": "Polygon",
 "crs": {
 "type": "name",
 "properties": {
 "name": "urn:ogc:def:crs:EPSG::3006"
 }
 },
 "coordinates": [
 [ [618174, 6728548], [618153, 6728423], [618270, 6728395],
 [618296, 6728525], [618174, 6728548] ]
 ]
 },
 "buffer": 50
}
"'

url <- "Län/geometri?includeData=detaljeradUtanEnklaverGeometri"

out <- httr::POST(
  url = file.path(base_url, url),
  config = httr::add_headers(
    .headers = c(
      eagles:::add_headers_token(token),
      'Accept' = 'application/json')
  ),
  # httr::content_type_json(),
  body = geometry,
  encode = "text")

httr::content(out, as = "text")

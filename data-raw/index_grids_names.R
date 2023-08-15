library(tidyverse)

# https://debuggingdata.com/post/r/regular-expressions-look-arounds/

# Formatting functions ----

# remove leading zero
# convert last capital letter to lower
# insert space before number within string
format_ekoruta <- function(.x) {
  .x |>
    stringr::str_remove("^0+") |>
    stringr::str_replace("([A-Z])$", tolower) |>
    stringr::str_replace("(?<=[A-Z])(?=[0-9])", " ")
}

format_fastighetsblad <- function(.x) {
  .x |>
    stringr::str_replace("(?<=[A-Z])(?=[0-9])", " ") |>
    stringr::str_replace("(?<= [0-9]).(?=[A-Z])", tolower)
}

format_fastighetsruta <- function(.x) {
  .x |>
    format_fastighetsblad() |>
    stringr::str_remove("N$|S$") |>
    sub("([0-9]{2})([A-Z])\\s([0-9]{1})([a-z])", "\\1\\3 \\2\\4", x = _) |>
    stringr::str_replace(" ", "_") |>
    stringr::str_replace_all(
      c(as.character(2:9) |> rlang::set_names(LETTERS[(2:9) + 1]),
        as.character(0:9) |>  rlang::set_names(letters[(0:9) + 1])))
}

grid_cell_file <- "data-raw/index_grids_names.xlsx"
readxl::excel_sheets(grid_cell_file)

# Storrutor ----
storrutor <- readxl::read_excel(
  grid_cell_file,
  sheet = "Storrutor_50km_RT90", col_types = "text") |>
  dplyr::mutate(
    ruta = stringr::str_remove(ruta_id, "^0+"),
    eagles::index_rt90(ruta_id, 50000)) |>
  dplyr::relocate(ruta, .after = ruta_id)

# Ekorutor ----
format_ekoruta(c("01C7H", "10I0C", "10J0A"))

## Transform back to original format ----
format_ekoruta(c("01C7H", "10I0C", "10J0A")) |>
  stringr::str_remove("\\s+") |>
  stringr::str_pad(width = 5, pad = "0", side = "left") |>
  toupper()

# expected return value c(6135000, 1335000), not c(6135000, 1175000)
eagles::index_rt90("01C7h", 5000)
eagles::index_rt90("01C7H", 5000)

ekorutor <- readxl::read_xlsx(
  grid_cell_file,
  sheet = "Ekorutor_5km_RT90", col_types = "text") |>
  # unite(col = "namn", namn:alternativt_namn, sep = " / ", na.rm = TRUE) |>
  dplyr::select(-alternativt_namn) |>
  dplyr::mutate(
    ruta = format_ekoruta(ruta_id),
    eagles::index_rt90(ruta_id, 5000)) |>
  dplyr::relocate(ruta, .after = ruta_id)

# Fastighetsblad ----
.x <- c("61D3GN", "61E3AN")
format_fastighetsblad(.x)
format_fastighetsruta(.x)

fastighetsblad <- readxl::read_xlsx(
  grid_cell_file,
  sheet = "Fastighet_SWEREF99_TM",
  col_types = "text") |>
  dplyr::mutate(
    blad = format_fastighetsblad(blad_id),
    ruta = format_fastighetsruta(blad_id),
    ruta_del = stringr::str_sub(blad_id, -1)) |>
  dplyr::relocate(blad, .after = blad_id) |>
  tidyr::separate(
    ruta, into = c("northing", "easting"),
    sep = "_", remove = FALSE) |>
  dplyr::mutate(
    northing = stringr::str_pad(
      northing,
      width = 7, pad = "0", side = "right"),
    easting = stringr::str_pad(
      easting,
      width = 6, pad = "0", side = "right"),
    dplyr::across(northing:easting, as.numeric),
    northing = dplyr::case_when(
      ruta_del == "N" ~ northing + 5000,
      .default = northing)) |>
  dplyr::relocate(ruta_del, .after = ruta) |>
  dplyr::arrange(ruta, northing)

# Check that grid are "spatiall correct" by creating mapviews ----
## Convert to spatial objects ----
storrutor_sf <- storrutor |>
  dplyr::mutate(
    geometry = purrr::map2(
      easting, northing,
      \(x,y) eagles::grid_cell(x, y, 50000, 50000))) |>
  sf::st_as_sf(crs = 3021)

ekorutor_sf <- ekorutor |>
  dplyr::mutate(geometry = purrr::map2(
    easting, northing,
    \(x,y) eagles::grid_cell(x, y, 5000, 5000))) |>
  sf::st_as_sf(crs = 3021)

fastighetsblad_sf <- fastighetsblad |>
  dplyr::mutate(geometry = purrr::map2(
    easting, northing,
    \(x,y) eagles::grid_cell(x, y, 10000, 5000))) |>
  sf::st_as_sf(crs = 3006)

mapview::mapview(storrutor_sf)
mapview::mapview(ekorutor_sf)
mapview::mapview(fastighetsblad_sf)

# Export package data ----
use_data(storrutor, overwrite = TRUE)
use_data(ekorutor, overwrite = TRUE)
use_data(fastighetsblad, overwrite = TRUE)


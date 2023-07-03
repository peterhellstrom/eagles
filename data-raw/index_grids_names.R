#library(tidyverse)
# https://debuggingdata.com/post/r/regular-expressions-look-arounds/

grid_cell_file <- "inst/extdata/index_grids_RT90_names.xlsx"
readxl::excel_sheets(grid_cell_file)

# Storrutor ----
storrutor <- readxl::read_excel(
  grid_cell_file,
  sheet = "Storrutor_50km_RT90", col_types = "text") |>
  # eagles::storrutor |>
  # dplyr::select(-northing, -easting, -ruta) |>
  dplyr::mutate(
    ruta = stringr::str_remove(ruta_id, "^0+"),
    eagles::index_rt90(ruta_id, 50000)) |>
  dplyr::relocate(ruta, .after = ruta_id)

# Ekorutor ----
eagles:::format_ekoruta(c("01C7H", "10I0C", "10J0A"))

## Transform back to original format ----
eagles:::format_ekoruta(c("01C7H", "10I0C", "10J0A")) |>
  stringr::str_remove("\\s+") |>
  stringr::str_pad(width = 5, pad = "0", side = "left") |>
  toupper()

eagles::index_rt90("01C7H", 5000)
# expected return value c(6135000, 1335000),
# not c(6135000, 1175000)
eagles::index_rt90("01C7h", 5000)

ekorutor <- readxl::read_xlsx(
  grid_cell_file,
  sheet = "Ekorutor_5km_RT90", col_types = "text") |>
  # unite(col = "namn", namn:alternativt_namn, sep = " / ", na.rm = TRUE) |>
  dplyr::select(-alternativt_namn) |>
  # eagles::ekorutor |>
  # dplyr::select(-northing, -easting, -ruta) |>
  dplyr::mutate(
    ruta = eagles:::format_ekoruta(ruta_id),
    eagles::index_rt90(ruta_id, 5000)) |>
  dplyr::relocate(ruta, .after = ruta_id)

# Fastighetsblad ----
.x <- c("61D3GN", "61E3AN")
eagles:::format_fastighetsblad(.x)
eagles:::format_fastighetsruta(.x)

fastighetsblad <- readxl::read_xlsx(
  grid_cell_file,
  sheet = "Fastighet_SWEREF99_TM", col_types = "text") |>
  # eagles::fastighetsblad |>
  # dplyr::select(-blad, -northing, -easting, -ruta, -ruta_del) |>
  dplyr::mutate(
    blad = eagles:::format_fastighetsblad(blad_id),
    ruta = eagles:::format_fastighetsruta(blad_id),
    ruta_del = stringr::str_sub(blad_id, -1)) |>
  dplyr::relocate(blad, .after = blad_id) |>
  tidyr::separate(ruta, into = c("northing", "easting"), sep = "_", remove = FALSE) |>
  dplyr::mutate(
    northing = stringr::str_pad(northing, width = 7, pad = "0", side = "right"),
    easting = stringr::str_pad(easting, width = 6, pad = "0", side = "right"),
    dplyr::across(northing:easting, as.numeric),
    northing = dplyr::case_when(
      ruta_del == "N" ~ northing + 5000,
      TRUE ~ northing)) |>
  dplyr::relocate(ruta_del, .after = ruta) |>
  dplyr::arrange(ruta, northing)

# Check that objects are equal ----
all.equal(storrutor, eagles::storrutor)
all.equal(ekorutor, eagles::ekorutor)
all.equal(fastighetsblad, eagles::fastighetsblad)


# Check that grid are "spatiall correct" by creating mapviews ----
## Convert to spatial objects ----
storrutor_sf <- eagles::storrutor |>
  eagles::index_to_sf(
    x = easting, y = northing,
    deltax = 50000, deltay = 50000, crs = 3021)

ekorutor_sf <- eagles::ekorutor |>
  eagles::index_to_sf(
    x = easting, y = northing,
    deltax = 5000, deltay = 5000, crs = 3021)

fastighetsblad_sf <- eagles::fastighetsblad |>
  eagles::index_to_sf(
    x = easting, y = northing,
    deltax = 10000, deltay = 5000, crs = 3006)

mapview::mapview(storrutor_sf)
mapview::mapview(ekorutor_sf)
mapview::mapview(fastighetsblad_sf)

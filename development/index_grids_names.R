#library(tidyverse)
# https://debuggingdata.com/post/r/regular-expressions-look-arounds/

# Storrutor ----
eagles::storrutor |>
  dplyr::select(-northing, -easting, -ruta) |>
  dplyr::mutate(
    ruta = stringr::str_remove(ruta_id, "^0+"),
    eagles::index_rt90(ruta_id, 50000)) |>
  dplyr::relocate(ruta, .after = ruta_id)

# Ekorutor ----
eagles:::format_ekoruta(c("01C7H", "10I0C", "10J0A"))

eagles::ekorutor |>
  dplyr::select(-northing, -easting, -ruta) |>
  dplyr::mutate(
    ruta = eagles:::format_ekoruta(ruta_id),
    eagles::index_rt90(ruta_id, 5000)) |>
  dplyr::relocate(ruta, .after = ruta_id)

# Fastighetsblad ----
.x <- c("61D3GN", "61E3AN")
eagles:::format_fastighetsblad(.x)
eagles:::format_fastighetsruta(.x)

eagles::fastighetsblad |>
  dplyr::select(-blad, -northing, -easting, -ruta, -ruta_del) |>
  dplyr::mutate(
    blad = eagles:::format_fastighetsblad(blad_id),
    ruta = eagles:::format_fastighetsruta(blad_id),
    ruta_del = stringr::str_sub(blad_id, -1)) |>
  dplyr::relocate(blad, .after = blad_id) |>
  tidyr::separate(ruta, into = c("northing", "easting"), sep = "_", remove = FALSE) |>
  dplyr::mutate(
    northing = stringr::str_pad(northing, width = 7, pad = "0", side = "right"),
    easting = stringr::str_pad(easting, width = 6, pad = "0", side = "right"),
    across(northing:easting, as.numeric)) |>
  dplyr::relocate(ruta_del, .after = ruta)

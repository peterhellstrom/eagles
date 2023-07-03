# Källa: bladindelningen med namn fanns i Lantmäteriets mjukvara Kartex (som avvecklades 2015)
#' @export
indexrutor_namn <- function(file_path = "inst/extdata/index_grids_RT90_names.xlsx") {

  if (!file.exists(file_path)) stop("Excel source file is missing or not available!")

  storrutor <- readxl::read_excel(
    path = file_path,
    sheet = "Storrutor_50km_RT90",
    col_types = "text")

  storrutor <- storrutor |>
    dplyr::mutate(index_rt90(ruta_id, .grid_size = 50000)) |>
    dplyr::rename_with(tolower) |>
    dplyr::mutate(ruta = stringr::str_replace(ruta_id, "^0", "")) |>
    dplyr::relocate(ruta, .after = ruta_id)

  ekorutor <- readxl::read_excel(
    path = file_path,
    sheet = "Ekorutor_5km_RT90",
    col_types = "text") |>
    dplyr::select(-alternativt_namn)

  ekorutor <- ekorutor |>
    dplyr::mutate(index_rt90(ruta_id, .grid_size = 5000)) |>
    dplyr::rename_with(tolower) |>
    dplyr::mutate(ruta = eagles:::format_ekoruta(ruta_id)) |>
    dplyr::relocate(ruta, .after = ruta_id)

  fastighetsblad <- readxl::read_excel(
    path = file_path,
    sheet = "Fastighet_SWEREF99_TM",
    col_types = "text")

  fastighetsblad <- fastighetsblad %>%
    dplyr::mutate(
      blad = eagles:::format_fastighetsblad(blad_id),
      ruta = eagles:::format_fastighetsruta(blad_id),
      ruta_del = str_sub(blad_id, -1)) |>
    tidyr::separate(ruta, into = c("northing", "easting"),
                    sep = "_", remove = FALSE) |>
    dplyr::relocate(blad, .after = blad_id) |>
    dplyr::relocate(ruta_del, .after = ruta) |>
    dplyr::mutate(
      northing = stringr::str_pad(northing, width = 7, pad = "0", side = "right"),
      easting = stringr::str_pad(easting, width = 6, pad = "0", side = "right"),
      across(northing:easting, as.numeric),
      northing = case_when(
        ruta_del == "N" ~ northing + 5000,
        ruta_del == "S" ~ northing,
        TRUE ~ NA)) |>
    dplyr::arrange(ruta, northing)

  list(storrutor = storrutor,
       ekorutor = ekorutor,
       fastighetsblad = fastighetsblad)
}

#' @export
index_to_sf <- function(.data, x, y, delta_x, delta_y, crs, remove = TRUE) {
  .out <- .data |>
    dplyr::rowwise() |>
    dplyr::mutate(geometry = list(grid_cell({{x}}, {{y}}, delta_x, delta_y))) |>
    sf::st_as_sf(sf_column_name = "geometry") |>
    sf::st_set_crs(crs) |>
    dplyr::ungroup()

  if (remove) {
    .out <- .out |>
      dplyr::select(-{{x}}, -{{y}})
  }
  .out
}

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
    eagles:::format_fastighetsblad() |>
    stringr::str_remove("N$|S$") |>
    sub("([0-9]{2})([A-Z])\\s([0-9]{1})([a-z])", "\\1\\3 \\2\\4", x = _) |>
    stringr::str_replace(" ", "_") |>
    stringr::str_replace_all(
      c(as.character(2:9) |> rlang::set_names(LETTERS[(2:9) + 1]),
        as.character(0:9) |>  rlang::set_names(letters[(0:9) + 1])))
}

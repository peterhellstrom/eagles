# Källa: bladindelningen med namn fanns i Lantmäteriets mjukvara Kartex (som avvecklades 2015)
# Dependencies: tidyverse, readxl
#' @export
indexrutor_namn <- function(file_path = "inst/extdata/index_grids_RT90_names.xlsx") {

  if (!file.exists(file_path)) stop("Excel source file is missing or not available!")

    storrutor <- readxl::read_excel(
      path = file_path,
      sheet = "Storrutor_50km_RT90",
      range = "A1:B253")

    storrutor <- storrutor %>%
      mutate(index_rt90(ruta_id, .grid_size = 50000)) %>%
      rename_with(tolower) %>%
      mutate(ruta = str_replace(ruta_id, "^0", "")) %>%
      relocate(ruta, .after = ruta_id)

    ekorutor <- readxl::read_excel(
      path = file_path,
      sheet = "Ekorutor_5km_RT90",
      range = "A1:B19193")

    ekorutor <- ekorutor %>%
      mutate(index_rt90(ruta_id, .grid_size = 5000)) %>%
      rename_with(tolower) %>%
      mutate(
        #ruta = str_c(str_sub(ruta_id, 1, 3), " ", tolower(str_sub(ruta_id, 4, 5))),
        ruta = str_c(str_sub(ruta_id, 1, 3) %>%
                       str_replace(., "^0", ""), " ",
                     tolower(str_sub(ruta_id, 4, 5)))) %>%
      relocate(ruta, .after = ruta_id)

    fastighetsblad <- readxl::read_excel(
      path = file_path,
      sheet = "Fastighet_SWEREF99_TM")

    fastighetsblad <- fastighetsblad %>%
      mutate(
        ruta = str_c(str_sub(blad, 1, 2), str_sub(blad, 5, 5), "_",
                     str_sub(blad, 3, 3), str_sub(blad, 6, 6)),
        ruta = str_replace_all(
          ruta,
          c(as.character(2:9) %>% setNames(LETTERS[(2:9) + 1]),
            as.character(0:9) %>% setNames(letters[(0:9) + 1]))),
        ruta_del = str_sub(blad, -1)) %>%
      separate(ruta, into = c("northing", "easting"), remove = FALSE) %>%
      relocate(ruta_del, .after = ruta) %>%
      mutate(
        northing = as.numeric(northing) * 10000,
        easting = as.numeric(easting) * 1000,
        northing = case_when(
          ruta_del == "N" ~ northing + 5000,
          ruta_del == "S" ~ northing,
          TRUE ~ NA)) %>%
      arrange(ruta, northing)

    list(storrutor = storrutor,
         ekorutor = ekorutor,
         fastighetsblad = fastighetsblad)
}

#' @export
index_to_sf <- function(data, x, y, deltax, deltay, crs) {
  data %>%
    rowwise() %>%
    mutate(geometry = list(grid_cell({{x}}, {{y}}, deltax, deltay))) %>%
    st_as_sf(sf_column_name = "geometry") %>%
    st_set_crs(crs) %>%
    select(-{{x}}, -{{y}})
}

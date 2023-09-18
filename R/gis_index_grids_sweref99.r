# https://www.lantmateriet.se/sv/geodata/vara-produkter/oppna-data/indexrutor/
# https://www.lantmateriet.se/sv/geodata/gps-geodesi-och-swepos/Referenssystem/Tvadimensionella-system/SWEREF-99-projektioner/
# Broken links:
# https://www.lantmateriet.se/sv/Kartor-och-geografisk-information/gps-geodesi-och-swepos/Referenssystem/Inforande-av-SWEREF-99-och-RH-2000/Infoblad/
# https://www.lantmateriet.se/globalassets/kartor-och-geografisk-information/gps-och-geodetisk-matning/info_blad-11.pdf

#' @export
quadrant_cardinal <- function(.y, .x, sep = "") {
  stringr::str_c(
    dplyr::if_else(.y == 0, "S", "N"),
    dplyr::if_else(.x == 0, "V", "O"),
    sep = sep)
}

#' @export
sweref99_add_prefix <- function(.x, crs) {

  crs_codes <- c(
    "TM", "1200", "1330", "1500", "1630", "1800", "1415", "1545",
    "1715", "1845", "2015", "2145", "2315") |>
    rlang::set_names(3006:3018)

  dplyr::case_when(
    crs == 3006 ~ .x,
    crs != 3006 ~ stringr::str_c(
      as.character(crs_codes[as.character(crs)]),
      .x, sep = "_"),
    TRUE ~ NA)
}

#' @export
sweref99_index <- function(
    .x,
    .y = NULL,
    .grid_size,
    crs = 3006) {

  # Allowed grid sizes
  gs <- c(
    100, 50, 25, 10, 5, 2.5, 1, 0.5, 0.25,
    0.1, 0.05, 0.025, 0.01, 0.005, 0.0025, 0.001) * 1000

  g <- .grid_size

  stopifnot(all(g %in% gs))

  # Extract coordinates if input is an sf-object
  if (class(.x)[1] == "sf") {
    .xy <- st_extract_pt_coords(.x)
    .x <- .xy$.x
    .y <- .xy$.y
  }

  # round up to nearest "parent" main grid cell, i.e. c(100000, 10000, 1000, 100, 10, 1)
  g_up <- round_up(g)
  g_type <- g / g_up # 1 = main grid cell, 0.5 = quarter-cell, 0.25 = sixteenth-cell
  index_main <- stringr::str_c(
    floor(.y / g_up),
    floor(.x / g_up), sep = "_"
  )

  # Add prefix for local projection zones EPSG:3007 ==> EPSG:3018
  if (any(crs != 3006)) {
    stopifnot(all(crs %in% 3006:3018))
    index_main <- sweref99_add_prefix(index_main, crs)
  }

  # Calculate position in quarter or sixteenth cell
  # (.value %% g_up / g_up) is the position in a main grid cell, scaled in the interval 0-1.
  # Round this value to {g_type} and multiply with 10 (for quarter) or 100 (for sixteenth).
  # Handle sixteenth-values by padding with extra leading zeros.
  dplyr::case_when(
    g_type == 1 ~ index_main,
    g_type == 0.5 ~ stringr::str_c(
      index_main,
      stringr::str_c(
        10 * round_any(.y %% g_up / g_up, g_type, floor),
        10 * round_any(.x %% g_up / g_up, g_type, floor)
      ),
      sep = "_"),
    g_type == 0.25 ~ stringr::str_c(
      index_main,
      stringr::str_c(
        str_pad(100 * round_any(.y %% g_up / g_up, g_type, floor), width = 2, pad = "0"),
        str_pad(100 * round_any(.x %% g_up / g_up, g_type, floor), width = 2, pad = "0")
      ),
      sep = "_")
  )
}

#' @export
add_sweref99_index <- function(
    .data,
    .grid_size = c(100000, 50000, 25000, 10000, 5000, 2500, 1000, 500),
    crs = sf::st_crs(.data)$epsg,
    .prefix = "grid") {

  .data |>
    dplyr::bind_cols(
      purrr::map(
        .grid_size, \(x) sweref99_index(.data, .grid_size = x, crs = crs)
      ) |>
        rlang::set_names(
          stringr::str_c(
            .prefix, "_", stringr::str_replace(.grid_size / 1000, "\\.", "\\_")
          )
        ) |>
        dplyr::bind_cols()
    )
}

# dsn <- "F:/Maps/Ortnamn/GSD-Ortnamn.gpkg"
# x <- st_read(dsn, query = "SELECT * FROM ortnamn LIMIT 2500;")
#
# x %>%
#   add_sweref99_index(.prefix = "ruta")
#
# x %>%
#   add_sweref99_index(.grid_size = c(10000, 5000))

#' @export
sweref99_index_alphanum <- function(
    .x, .y = NULL,
    .grid_size,
    crs = 3006,
    caps = FALSE, space = TRUE,
    fastighetsblad = FALSE) {

  gs <- c(1000, 5000, 10000, 50000, 100000)

  g <- .grid_size

  stopifnot(all(g %in% gs))

  # Extract coordinates if input is an sf-object
  if (class(.x)[1] == "sf") {
    .xy <- st_extract_pt_coords(.x)
    .x <- .xy$.x
    .y <- .xy$.y
  }

  an_100 <- stringr::str_c(
    floor(.y / 100000),
    LETTERS[floor(1 + (.x / 100000))]
  )

  # Add prefix for local projection zones EPSG:3007 ==> EPSG:3018
  if (base::any(crs != 3006)) {
    stopifnot(base::all(crs %in% 3006:3018))
    an_100 <- sweref99_add_prefix(an_100, crs)
  }

  an_10 <- stringr::str_c(
    floor((.y %% 100000) / 10000),
    letters[floor(1 + ((.x %% 100000) / 10000))]
  )

  if (.grid_size == 100000) {
    out <- an_100
  } else if (.grid_size == 50000) {
    an_50 <- quadrant_cardinal(
      floor((.y %% 100000) / 50000),
      floor((.x %% 100000) / 50000)
    )
    out <- stringr::str_c(an_100, an_50, sep = " ")
  } else if (.grid_size == 10000) {
    if (fastighetsblad == FALSE) {
      out <- stringr::str_c(an_100, an_10, sep = " ")
    } else {
      out <- stringr::str_c(
        an_100, an_10,
        dplyr::if_else(floor((.y %% 10000) / 5000) == 0, "S", "N"),
        sep = " ")
    }
  } else if (.grid_size == 5000) {
    an_5 <- quadrant_cardinal(
      floor((.y %% 10000) / 5000),
      floor((.x %% 10000) / 5000)
    )
    out <- stringr::str_c(an_100, an_10, an_5, sep = " ")
  } else if (.grid_size == 1000) {
    an_1 = stringr::str_c(
      floor((.y %% 10000) / 1000),
      letters[floor(1 + ((.x %% 10000) / 1000))]
    )
    out <- stringr::str_c(an_100, an_10, an_1, sep = " ")
  } else {
    out <- NA
  }

  if (caps) out <- toupper(out)
  if (space == FALSE) out <- gsub(" ", "", out)

  out
}

# Examples
# sweref99_index_alphanum(667533, 6559904, .grid_size = 5000)
# map(c(100000, 50000, 10000, 5000, 1000),
#     ~ sweref99_index_alphanum(667533, 6559904, .grid_size = .x))
#
# .data <- st_sf(id = 1:2,
#                name = c("Bu001", "Ph005"),
#                geom = st_sfc(
#                  st_point(c(667533, 6559904)),
#                  st_point(c(625549, 6594338))), crs = 3847)
#
# gs <- c(100000, 50000, 10000, 5000, 1000) %>%
#   setNames(c("100km", "50km", "10km", "5km", "1km"))
#
# bind_cols(.data, map_dfr(gs, ~ sweref99_index_alphanum(.data, .grid_size = .x)))

# xy <- tibble(x = c(667533, 625549), y = c(6559904, 6594338)) %>%
#   mutate(id = row_number()) %>%
#   st_as_sf(coords = c("x", "y"), crs = 3006)
#
# sweref99_index_alphanum(xy, .grid_size = 100000)
# sweref99_index_alphanum(xy, .grid_size, 10000, fastighetsblad = TRUE)
#
# xy %>%
#   #st_transform(3011) %>%
#   mutate(
#     `100km_alfanum` = sweref99_index_alphanum(., .grid_size = 100000),
#     `50km_alfanum` = sweref99_index_alphanum(., .grid_size = 50000),
#     `10km_alfanum` = sweref99_index_alphanum(., .grid_size = 10000),
#     `5km_alfanum` = sweref99_index_alphanum(., .grid_size = 5000),
#     `1km_alfanum` = sweref99_index_alphanum(., .grid_size = 1000))

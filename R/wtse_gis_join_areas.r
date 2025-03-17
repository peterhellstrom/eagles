# Note that the admin_areas_load() and admin_areas_join() functions
# have been generalized in st_join_n() and st_join_n_loop().

#' Title
#'
#' @param db
#' @param layer
#' @param import_to_sf
#'
#' @returns
#' @export
#'
#' @examples
admin_areas_load <- function(
    db = Sys.getenv("gpkg_naturvard"),
    layer = c(
      "admin_kommun_fastk",
      "admin_lan_fastk",
      "admin_distrikt",
      "admin_landskap_fran_distrikt",
      "admin_landskap_artportalen",
      "admin_rapportomrade_artportalen"
    ),
    import_to_sf = c(
      "kommun",
      "lan",
      "distrikt",
      "landskap",
      "landskap_ap",
      "rapportomrade"
    )
) {

  purrr::walk2(
    import_to_sf,
    layer,
    \(x, y) base::assign(
      x,
      # sf::read_sf(dsn = db, layer = y, quiet = TRUE),
      sf::read_sf(dsn = db, layer = y),
      envir = .GlobalEnv
    )
  )
}

# Do not name function arguments the same thing as variables,
# and vice versa!
# This will result in the following error
# Error in dplyr::select(lan, LanID = lankod) :
# promise already under evaluation: recursive default argument reference or earlier problems?

#' Title
#'
#' @param .x
#' @param lan_sf
#' @param kommun_sf
#' @param distrikt_sf
#' @param rapportomrade_sf
#' @param id
#'
#' @returns
#' @export
#'
#' @examples
admin_areas_join <- function(
    .x,
    lan_sf = lan,
    kommun_sf = kommun,
    distrikt_sf = distrikt,
    rapportomrade_sf = rapportomrade,
    id = FALSE) {

  if (id == FALSE) {
    xy_join <- .x |>
      sf::st_join(
        lan |>
          dplyr::select(Lan = lanbok)
      ) |>
      sf::st_join(
        kommun |>
          dplyr::select(Kommun = kommunnamn)
      ) |>
      sf::st_join(
        distrikt |>
          dplyr::select(
            Landskap = landskapbok,
            Distrikt = distriktsnamn)
      ) |>
      # sf::st_join(landskap_ap |>
      #           dplyr::select(Landskap_AP = area_name)) |>
      sf::st_join(
        rapportomrade |>
          dplyr::select(Rapportomrade = area_name)
      )
  } else {
    xy_join <- .x |>
      sf::st_join(
        lan |>
          dplyr::select(LanID = lankod)
      ) |>
      sf::st_join(
        kommun |>
          dplyr::select(KommunID = kommunkod)
      ) |>
      sf::st_join(
        distrikt |>
          dplyr::select(
            LandskapID = landskapskod,
            DistriktID = distriktskod)
      ) |>
      sf::st_join(
        rapportomrade |>
          dplyr::select(RapportomradeID = area_id)
      )
  }
  xy_join
}

#' Title
#'
#' @param .x
#' @param .y
#' @param .x_group
#' @param .y_ID
#' @param .y_NAME
#' @param OmradeTypID
#' @param MetodAvstand
#' @param Metod
#' @param MetodAndelArea
#' @param left
#' @param geometry_field
#'
#' @returns
#' @export
#'
#' @examples
wtse_join_areas <- function(
    .x,
    .y,
    .x_group = LokalID,
    .y_ID, .y_NAME,
    OmradeTypID,
    MetodAvstand = 0,
    Metod = st_intersects,
    MetodAndelArea = 1,
    left = FALSE,
    geometry_field = geom
  ) {

  if (MetodAvstand == 0) {

    Metod_str <- enexpr(Metod)

    x <- sf::st_join(
      .x,
      .y,
      join = Metod,
      left = left) |>
      dplyr::mutate(
        OmradeTypID = OmradeTypID,
        Metod = dplyr::as_label(Metod_str),
        MetodAvstand = MetodAvstand,
        MetodAndelArea = MetodAndelArea
      ) |>
      dplyr::group_by(
        {{ .x_group }}
      ) |>
      dplyr::mutate(
        MetodAntalOmraden = n()
      ) |>
      dplyr::select(
        {{ .x_group }},
        OmradeTypID,
        OmradeID = {{ .y_ID }},
        OmradeNamn = {{ .y_NAME }},
        Metod,
        MetodAvstand,
        MetodAntalOmraden,
        MetodAndelArea
      ) |>
      dplyr::mutate(
        OmradeID = as.character(OmradeID)
      )
  } else {

    st_agr(.x) = "constant"
    st_agr(.y) = "constant"

    x <- sf::st_intersection(
      .x |>
        sf::st_buffer(MetodAvstand), .y) |>
      dplyr::mutate(
        area = sf::st_area( {{ geometry_field }} ),
        OmradeTypID = OmradeTypID
      ) |>
      dplyr::group_by(
        {{ .x_group }}
      ) |>
      dplyr::select(
        {{ .x_group }},
        OmradeTypID,
        OmradeID = {{ .y_ID }},
        OmradeNamn = {{ .y_NAME }},
        area
      ) |>
      dplyr::arrange(OmradeID) |>
      dplyr::mutate(
        OmradeID = as.character(OmradeID),
        Metod = "st_buffer",
        MetodAvstand = MetodAvstand,
        MetodAntalOmraden = dplyr::n(),
        MetodTotalArea = sum(area),
        MetodAndelArea = as.numeric(area / MetodTotalArea)
      ) |>
      dplyr::select(
        -area, -MetodTotalArea
      )
  }

  x |>
    sf::st_drop_geometry() |>
    tibble::as_tibble()
}

#' @export
admin_areas_load <- function(
    db = "E:/Maps/Naturvard/Naturvard.gpkg",
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

  walk2(.x = import_to_sf,
        .y = layer,
        .f = ~ assign(
          .x,
          sf::st_read(dsn = db, layer = .y, quiet = TRUE),
          envir = .GlobalEnv)
  )
}

#' @export
admin_areas_join <- function(
    .x,
    lan,
    kommun,
    distrikt,
    rapportomrade,
    id = FALSE) {
  if(!id) {
    xy_join <- .x |>
      sf::st_join(
        lan |> dplyr::select(Lan = lanbok)) |>
      sf::st_join(
        kommun |> dplyr::select(Kommun = kommunnamn)) |>
      sf::st_join(
        distrikt |>
          dplyr::select(
            Landskap = landskapbok,
            Distrikt = distriktsnamn)) |>
      # sf::st_join(landskap_ap |>
      #           dplyr::select(Landskap_AP = area_name)) |>
      sf::st_join(
        rapportomrade |> dplyr::select(Rapportomrade = area_name))
  } else {
    xy_join <- .x |>
      sf::st_join(
        lan |> dplyr::select(LanID = lankod)) |>
      sf::st_join(
        kommun |> dplyr::select(KommunID = kommunkod)) |>
      sf::st_join(
        distrikt |>
          dplyr::select(
            LandskapID = landskapskod,
            DistriktID = distriktskod)) |>
      sf::st_join(
        rapportomrade |> dplyr::select(RapportomradeID = area_id))
  }
  xy_join
}

#' @export
wtse_join_areas <- function(
    .x, .y,
    .x_group = LokalID,
    .y_ID, .y_NAME,
    OmradeTypID,
    MetodAvstand = 0,
    Metod = st_intersects,
    MetodAndelArea = 1,
    left = FALSE) {

  if (MetodAvstand == 0) {

    Metod_str <- enexpr(Metod)

    x <- sf::st_join(
      .x,
      .y,
      join = Metod,
      left = left) |>
      dplyr::mutate(
        OmradeTypID = OmradeTypID,
        Metod = as_label(Metod_str),
        MetodAvstand = MetodAvstand,
        MetodAndelArea = MetodAndelArea) |>
      dplyr::group_by(
        {{.x_group}}
      ) |>
      dplyr::mutate(
        MetodAntalOmraden = n()
      ) |>
      dplyr::select(
        {{.x_group}},
        OmradeTypID,
        OmradeID = {{.y_ID}},
        OmradeNamn = {{.y_NAME}},
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
        # "Hard coded" geometry column name,
        # this should be avoided. Not possible to use dot-notation!
        area = sf::st_area(geometry),
        OmradeTypID = OmradeTypID) |>
      dplyr::group_by(
        {{.x_group}}
      ) |>
      dplyr::select(
        {{.x_group}},
        OmradeTypID,
        OmradeID = {{.y_ID}},
        OmradeNamn = {{.y_NAME}},
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

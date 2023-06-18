#' @export
admin_areas_load <- function(
    db = "E:/Maps/Naturvard/Naturvard.gpkg",
    layer = c("admin_kommun_fastk", "admin_lan_fastk",
          "admin_distrikt", "admin_landskap_fran_distrikt",
          "admin_landskap_artportalen", "admin_rapportomrade_artportalen"),
    import_to_sf = c("kommun", "lan", "distrikt", "landskap", "landskap_ap", "rapportomrade")) {

  walk2(.x = import_to_sf, .y = layer,
        .f = ~ assign(.x, st_read(dsn = db, layer = .y, quiet = TRUE),
                      envir = .GlobalEnv))
}

#' @export
admin_areas_join <- function(.x, id = FALSE) {
  if(!id) {
    xy_join <- .x %>%
      st_join(lan %>%
                select(Lan = lanbok)) %>%
      st_join(kommun %>%
                select(Kommun = kommunnamn)) %>%
      st_join(distrikt %>%
                select(Landskap = landskapbok,
                       Distrikt = distriktsnamn)) %>%
      # st_join(landskap_ap %>%
      #           select(Landskap_AP = area_name)) %>%
      st_join(rapportomrade %>%
                select(Rapportomrade = area_name))
  } else {
    xy_join <- .x %>%
      st_join(lan %>%
                select(LanID = lankod)) %>%
      st_join(kommun %>%
                select(KommunID = kommunkod)) %>%
      st_join(distrikt %>%
                select(LandskapID = landskapskod,
                       DistriktID = distriktskod)) %>%
      st_join(rapportomrade %>%
                select(RapportomradeID = area_id))
  }
  xy_join
}

#' @export
wtse_join_areas <- function(.x, .y, .x_group = LokalID,
                .y_ID, .y_NAME,
                OmradeTypID,
                MetodAvstand = 0,
                Metod = st_intersects,
                MetodAndelArea = 1,
                left = FALSE) {

  if (MetodAvstand == 0) {

    Metod_str <- enexpr(Metod)

    x <- st_join(.x, .y, join = Metod, left = left) %>%
      mutate(OmradeTypID = OmradeTypID,
             Metod = as_label(Metod_str),
             MetodAvstand = MetodAvstand,
             MetodAndelArea = MetodAndelArea) %>%
      group_by({{.x_group}}) %>%
      mutate(MetodAntalOmraden = n()) %>%
      select({{.x_group}},
             OmradeTypID,
             OmradeID = {{.y_ID}},
             OmradeNamn = {{.y_NAME}},
             Metod,
             MetodAvstand,
             MetodAntalOmraden,
             MetodAndelArea) %>%
      mutate(OmradeID = as.character(OmradeID))
  } else {

    st_agr(.x) = "constant"
    st_agr(.y) = "constant"

    x <- st_intersection(
      .x %>% st_buffer(MetodAvstand), .y) %>%
      mutate(area = st_area(.),
             OmradeTypID = OmradeTypID) %>%
      group_by({{.x_group}}) %>%
      select({{.x_group}},
             OmradeTypID,
             OmradeID = {{.y_ID}},
             OmradeNamn = {{.y_NAME}},
             area) %>%
      arrange(OmradeID) %>%
      mutate(
        OmradeID = as.character(OmradeID),
        Metod = "st_buffer",
        MetodAvstand = MetodAvstand,
        MetodAntalOmraden = n(),
        MetodTotalArea = sum(area),
        MetodAndelArea = as.numeric(area / MetodTotalArea)) %>%
      select(-area, -MetodTotalArea)
  }

  x %>%
    st_drop_geometry() %>%
    as_tibble()
}

#' Title
#'
#' @param area_field
#' @param area_id
#' @param kontakt_id
#' @param wtse_sites
#' @param dsn
#'
#' @return
#' @export
#'
#' @examples
wtse_sites_get_access <- function(
    area_field,
    area_id,
    kontakt_id,
    wtse_sites = sites,
    dsn = "Havsorn_Data"
) {

  on.exit(DBI::dbDisconnect(con))
  con <- DBI::dbConnect(odbc::odbc(), dsn)

  # View all sites in survey sub-areas ----
  all_sites <- DBI::dbGetQuery(
    con,
    glue::glue(
      "SELECT {area_field}, LokalID, Lokalkod, Lokalnamn FROM tLokaler WHERE {area_field} IN ({str_c(area_id, collapse = ',')}) ORDER BY {area_field}, Lokalkod"
    )
  ) |>
    tibble::as_tibble()

  # View all site codes listed for KontaktID ----
  all_sites_contact <- DBI::dbGetQuery(
    con,
    glue::glue(
      "SELECT * FROM tLokalerKontakter WHERE KontaktID IN ({kontakt_id})"
    )
  ) |>
    tibble::as_tibble()

  all_sites_contact <- all_sites_contact |>
    dplyr::left_join(
      wtse_sites |>
        dplyr::select(LokalID, RegionID, LanID, KommunID, Lokalkod, Lokalnamn),
      dplyr::join_by(LokalID)
    )

  # all_sites |>
  #   dplyr::anti_join(all_sites_contact, dplyr::join_by(LokalID)) |>
  #   dplyr::arrange(Lokalkod) |>
  #   print(n = Inf)

  has_access_not_in_set <- all_sites_contact |>
    dplyr::anti_join(
      all_sites,
      dplyr::join_by(LokalID)
    ) |>
    dplyr::arrange(Lokalkod)

  str_sql_has_access <-
    glue::glue(
      "SELECT tLokaler.{area_field}, tLokaler.LokalID, tLokaler.Lokalkod, tLokaler.Lokalnamn, tLokalerKontakter.KontaktID, tLokalerKontakter.AnsvarID, tLokalerKontakter.Aktiv
FROM tLokaler INNER JOIN tLokalerKOntakter ON tLokaler.LokalID = tLokalerKontakter.Lokalid
WHERE (((tLokaler.{area_field}) In ({str_c(area_id, collapse = ',')})) AND ((tLokalerKontakter.KontaktID)={str_c(kontakt_id, collapse = ',')}))
ORDER BY tLokaler.{area_field}, tLokaler.Lokalkod;"
    )

  has_access <- DBI::dbGetQuery(con, str_sql_has_access) |>
    tibble::as_tibble()

  # Must use subquery to extract information ONLY for the given KontaktID,
  # as joining the full table tLokalerKontakter would look for NULL among
  # all contacts in table, not only the specified one(s).
  str_sql_has_no_access <-
    glue::glue(
      "SELECT tLokaler.{area_field}, tLokaler.LokalID, tLokaler.Lokalkod, tLokaler.Lokalnamn, b.KontaktID, b.AnsvarID, b.Aktiv
FROM tLokaler LEFT JOIN (SELECT * FROM tLokalerKontakter WHERE KontaktID IN ({str_c(kontakt_id, collapse = ',')})) AS b ON tLokaler.LokalID = b.Lokalid
WHERE (((tLokaler.{area_field}) In ({str_c(area_id, collapse = ',')})) AND ((b.KontaktID) IS NULL))
ORDER BY tLokaler.{area_field}, tLokaler.Lokalkod;"
    )

  no_access <- DBI::dbGetQuery(con, str_sql_has_no_access) |>
    tibble::as_tibble()

  set_access <- no_access |>
    dplyr::select(LokalID, KontaktID, AnsvarID, Aktiv) |>
    dplyr::mutate(
      KontaktID = kontakt_id,
      # AnsvarID = 14, # Fältinventering + rapportering
      AnsvarID = 8, # För kännedom
      # Aktiv = -1 # om aktiv
      Aktiv = 0
    )

  out <- list(
    all_sites_contact = all_sites_contact,
    all_sites_set = all_sites,
    has_access = has_access,
    has_access_not_in_set = has_access_not_in_set,
    no_access = no_access,
    set_access = set_access
  )

  out
}

#' Title
#'
#' @param .data
#' @param wtse_sites
#'
#' @return
#' @export
#'
#' @examples
wtse_sites_get_access_map <- function(
    .data,
    wtse_sites = sites_sf
) {
  mapview::mapview(
    list(
      access = wtse_sites |>
        dplyr::select(-c(9:15)) |>
        dplyr::semi_join(
          .data$all_sites_contact,
          dplyr::join_by(LokalID)
        ),
      no_access = wtse_sites |>
        dplyr::select(-c(9:15)) |>
        dplyr::anti_join(
          .data$all_sites_contact,
          dplyr::join_by(LokalID)
        )
    ),
    col.regions = list("blue", "red")
  )
}

#' Title
#'
#' @param .data
#' @param dsn
#'
#' @return
#' @export
#'
#' @examples
wtse_sites_set_access <- function(.data, dsn = "Havsorn_Data") {
  if (nrow(.data$set_access) > 0) {

    on.exit(close(con))
    con <- RODBC::odbcConnect(dsn)

    sql_insert_site_role <- eagles::db_pquery(
      "tLokalerKontakter",
      c("LokalID", "KontaktID", "AnsvarID", "Aktiv"),
      type = "insert"
    )

    RODBCext::sqlExecute(
      con,
      sql_insert_site_role,
      .data$set_access |> as.data.frame()
    )
  } else {
    cat("No rows in input data!", "\n")
  }
}

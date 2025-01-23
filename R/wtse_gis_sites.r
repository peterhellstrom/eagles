# If spatial = TRUE an sf object is returned,
# To retrieve data in non-spatial form, use spatial = FALSE.
# Prior to R 4.2, encoding = 'windows-1252' was used with
# connection made with DBI-package, but this is not necessary
# since native UTF8 encoding support was introduced in R >= 4.2.
# However, odbc connection to MS Access and UTF-8 strings might
# introduce errors (right string truncation due to special characters).
# A previous version of this function therefore had extra functionality
# where it was possible to connect to an odbc source either with
# the RODBC or the DBI package. This functionality was removed in
# September 2023, in favour of the DBI package as the DBI errors were no longer
# present.

#' Title
#'
#' @param spatial
#' @param crs
#' @param add_coordinates
#' @param coordinate_cols
#' @param odbc_name
#' @param encoding
#' @param add_subsites
#' @param add_monitoring
#' @param drop_id_columns
#' @param ortnamn_path
#'
#' @return
#' @export
#'
#' @examples
wtse_sites <- function(
    spatial = FALSE,
    crs = NULL,
    add_coordinates = FALSE,
    coordinate_cols = c("x", "y"),
    odbc_name = "Havsorn_Data",
    encoding = "",
    add_subsites = TRUE,
    add_monitoring = TRUE,
    drop_id_columns = TRUE,
    ortnamn_path = "G:/Ortnamn/GSD-Ortnamn_2012.gpkg"
) {

  on.exit(DBI::dbDisconnect(con))
  con <- DBI::dbConnect(odbc::odbc(), odbc_name, encoding = encoding)
  con_tbls <- DBI::dbListTables(con)

  if (spatial | add_coordinates) {
    if (!any(file.exists(ortnamn_path), "tLokalerOrtnamn" %in% con_tbls)) {
      spatial <- FALSE
      add_coordinates <- FALSE
      message("'ortnamn' GeoPackage or local table tLokalerOrtnamn can not be found. Spatial data can not be returned!")
    }
  }

  sites_sql <-
    "SELECT tLokaler.*,
    luGeografiLivsmiljo.Livsmiljo,
    luGeografiLandsdel.Landsdel,
    luGeografiVattendistrikt.Vattendistrikt,
    luGeografiRegion.Region,
    luGeografiLan.LanBokstav AS Lan,
    luGeografiKommun.Kommun,
    luGeografiLandskap.LandskapBokstav AS Landskap,
    luGeografiDistrikt.Distrikt,
    luGeografiRapportomrade.Rapportomrade,
    luGeografiDelomrade.Delomrade
    FROM ((((((((((tLokaler
    LEFT JOIN luGeografiRegion ON tLokaler.RegionID = luGeografiRegion.RegionID)
    LEFT JOIN luGeografiLan ON tLokaler.LanID = luGeografiLan.LanID)
    LEFT JOIN luGeografiKommun ON tLokaler.KommunID = luGeografiKommun.KommunID)
    LEFT JOIN luGeografiLandskap ON tLokaler.LandskapID = luGeografiLandskap.LandskapID)
    LEFT JOIN luGeografiDistrikt ON tLokaler.DistriktID = luGeografiDistrikt.DistriktID)
    LEFT JOIN luGeografiRapportomrade ON tLokaler.RapportomradeID = luGeografiRapportomrade.RapportomradeID)
    LEFT JOIN luGeografiDelomrade ON tLokaler.DelomradeID = luGeografiDelomrade.DelomradeID)
    LEFT JOIN luGeografiLivsmiljo ON tLokaler.LivsmiljoID = luGeografiLivsmiljo.LivsmiljoID)
    LEFT JOIN luGeografiLandsdel ON tLokaler.LandsdelID = luGeografiLandsdel.LandsdelID)
    LEFT JOIN luGeografiVattendistrikt ON tLokaler.VattendistriktID = luGeografiVattendistrikt.VattendistriktID)
  ORDER BY tLokaler.LanID, tLokaler.Lokalkod"

  sites <- DBI::dbGetQuery(con, sites_sql) |>
    tibble::as_tibble()

  # Re-format sites table
  sites <- sites |>
    dplyr::relocate(
      Livsmiljo:Delomrade,
      .after = LokalID
    ) |>
    # Drop some non-essential columns (this step might change)
    dplyr::select(-Alias, -KommentarHemlig, -OrtnamnOsakert)

  if (drop_id_columns) {
    sites <- sites |>
      dplyr::select(-c(StationID:DelomradeID))
  }

  if (add_subsites) {
    sites_sub <- DBI::dbReadTable(con, "tLokalerUnder") |>
      tibble::as_tibble()

    # Sub-sites, summarize to one row per main site.
    sites_sub_sum <- sites_sub |>
      dplyr::arrange(LokalID, LokalerUnderID) |>
      dplyr::summarize(
        LokalUnder = stringr::str_c(LokalUnder, collapse = " | "),
        .by = LokalID
      )

    sites <- sites |>
      dplyr::left_join(
        sites_sub_sum,
        dplyr::join_by(LokalID)
      ) |>
      dplyr::relocate(
        LokalUnder,
        .after = Lokalnamn
      )
  }

  if (add_monitoring) {
    sites_monitoring <- DBI::dbReadTable(con, "tLokalerOvervakning") |>
      tibble::as_tibble()
    sites <- sites |>
      dplyr::left_join(
        sites_monitoring |> monitoring_summary(),
        dplyr::join_by(LokalID)
      )
  }

  if (spatial | add_coordinates) {

    if ("tLokalerOrtnamn" %in% con_tbls) {
      # message("Use spatial data in tLokalerOrtnamn")

      sites_coords <- DBI::dbReadTable(con, "tLokalerOrtnamn") |>
        tibble::as_tibble() |>
        sf::st_as_sf(
          coords = c("easting", "northing"),
          crs = 3006
        )

    } else {
      # message("Use spatial data in ortnamn GeoPackage")

      ortnamn_ids <- sites |>
        dplyr::filter(!is.na(Ortnamn_LOPNR)) |>
        dplyr::pull(Ortnamn_LOPNR) |>
        stringr::str_c(collapse = ", ")

      sites_coords <- sf::read_sf(
        ortnamn_path,
        query = glue::glue("SELECT DISTINCT lopnr, geom FROM ortnamn WHERE lopnr IN ({ortnamn_ids})")
      )
    }
    # sites_coords <- get_sites_coords(con)

    if (!is.null(crs)) {
      sites_coords <- sites_coords |>
        sf::st_transform(crs = crs)
    }
  }

  if (spatial) {

    sites <- sites |>
      dplyr::inner_join(
        sites_coords,
        dplyr::join_by(Ortnamn_LOPNR == lopnr)
      ) |>
      sf::st_sf()

    if (add_coordinates) {
      sites <- sites |>
        swecoords::sfc_as_cols(names = coordinate_cols)
    }

  } else if (!spatial) {

    if (add_coordinates) {

      sites <- sites |>
        dplyr::left_join(
          sites_coords |>
            swecoords::sfc_as_cols(names = coordinate_cols) |>
            sf::st_drop_geometry(),
          dplyr::join_by(Ortnamn_LOPNR == lopnr)
        )
    }
  }

  sites
}

# Examples
# lkd <- wtse_sites(spatial = FALSE)
# lkd |> View()
# lkd |> dplyr::count(Region)
# lkd |> dplyr::select(BoplatsOkand, date_created, date_modified)
# lkd_id <- wtse_sites(spatial = FALSE, drop_id_columns = FALSE, add_monitoring = FALSE)
# lkd_id |>
#   dplyr::count(RegionID, Region, LanID, Lan) |>
#   print(n = Inf)
# Argument order:
# spatial = TRUE, crs = NULL, add_coordinates = FALSE
# wtse_sites(TRUE, NULL, TRUE, add_monitoring = FALSE)
# wtse_sites(TRUE, NULL, FALSE, add_monitoring = FALSE)
# wtse_sites(TRUE, 4326, FALSE, add_monitoring = FALSE)
# wtse_sites(TRUE, 4326, TRUE, coordinate_cols = c("longitude", "latitude"), add_monitoring = FALSE)
# wtse_sites(FALSE, NULL, FALSE, add_monitoring = FALSE)
# wtse_sites(FALSE, 3847, FALSE, add_monitoring = FALSE)
# wtse_sites(FALSE, NULL, TRUE, add_monitoring = FALSE)
# wtse_sites(FALSE, NULL, TRUE, add_monitoring = FALSE, coordinate_cols = c("Easting", "Northing"))
# wtse_sites(FALSE, 3847, TRUE, coordinate_cols = c("Easting", "Northing"), add_monitoring = FALSE)
# wtse_sites(FALSE, 4326, TRUE, coordinate_cols = c("longitude", "latitude"), add_monitoring = FALSE)


#' Title
#'
#' @param .data
#' @param .lookup_table
#' @param .join_cols
#' @param .fn_join
#'
#' @return
#' @export
#'
#' @examples
named_join <- function(
    .data,
    .lookup_table,
    .join_cols,
    .fn_join = dplyr::left_join
) {
  .data |>
    .fn_join(
      .lookup_table,
      dplyr::join_by( {{ .join_cols }} )
    )
}

# a <- tribble(
#   ~id, ~state_abbrev,
#   1, "AL",
#   2, "AK"
# )
#
# b <- tribble(
#   ~id, ~state_name,
#   1, "Alabama",
#   2, "Alaska",
#   3, "Arizona"
# )
#
# named_join(a, b, id, .fn_join = dplyr::left_join)
# named_join(a, b, id, .fn_join = dplyr::right_join)
# named_join(a, b, id, .fn_join = dplyr::full_join)
# named_join(a, b, id, .fn_join = dplyr::inner_join)

get_sites_coords <- function(con) {
  # Add geographical data (in SWEREF99 TM)
  # Source data is "hard-coded" in the data connection.
  # db_ortnamn_path = "G:/Ortnamn/GSD-Ortnamn_2012.gpkg"
  # NOTE: The GeoPackage dataset is a linked table in the
  # wtse database backend (Havsorn_Data). MS Access can not
  # deal with spatial datatypes, so it is necessary to
  # reformat the WKT representation of the geometry column in
  # MS Access to simple features.
  # This could be avoided if it is/would be possible to
  # construct an SQL query combining data from tables in
  # more than one data source (e.g. a GeoPackage and an MS Access db).

  str_sql <-
    "SELECT DISTINCT ortnamn.lopnr, ortnamn.geom
     FROM tLokaler
     INNER JOIN ortnamn ON tLokaler.Ortnamn_LOPNR = ortnamn.lopnr
     ORDER BY ortnamn.lopnr;"

  x <- DBI::dbGetQuery(con, str_sql) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      geom = stringr::str_extract(geom, "(?<=X').*(?=')")
    )

  x$geom <- sf::st_as_sfc(
    structure(as.list(x$geom), class = "WKB"),
    EWKB = TRUE
  )

  x |>
    sf::st_sf()
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
wtse_sites_subregion <- function() {

  wtse_sites(
    add_monitoring = FALSE,
    drop_id_columns = FALSE,
    spatial = FALSE
  ) |>
    dplyr::select(
      LokalID, RegionID, KommunID
    ) |>
    dplyr::mutate(
      SubRegion = wtse_region_from_kommun(
        as.character(RegionID),
        KommunID
      ),
      SubRegionText = wtse_region_reformat(SubRegion),
      SubRegionText0 = dplyr::case_when(
        str_detect(SubRegionText, "Eg. Östersjön") ~ "Eg. Östersjön",
        str_detect(SubRegionText, "Botten") ~ "Bottniska viken",
        .default = SubRegionText
      )
    ) |>
    dplyr::select(
      LokalID,
      SubRegionText0,
      SubRegionText,
      SubRegion
    )

}

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
# September 2023, in favor of the DBI package as the DBI errors were no longer
# present.
#
#' @export
wtse_sites <- function(
    spatial = FALSE,
    crs = NULL,
    add_coordinates = FALSE,
    coordinate_cols = c("x", "y"),
    odbc_name = "Havsorn_Data",
    encoding = "",
    add_subsites = TRUE,
    add_monitoring = TRUE,
    drop_id_columns = TRUE) {

  # Import data tables
  # values = names of tables in odbc source.
  # names = names of tables when imported to R an environment.
  tbls <- c(
    sites = "tLokaler", sites_sub = "tLokalerUnder",
    sites_monitoring = "tLokalerOvervakning", county = "luGeografiLan",
    municipality = "luGeografiKommun", province = "luGeografiLandskap",
    district = "luGeografiDistrikt", report_area = "luGeografiRapportomrade",
    sub_area = "luGeografiDelomrade", region = "luGeografiRegion"
  )

  con <- DBI::dbConnect(odbc::odbc(), odbc_name, encoding = encoding)
  on.exit(DBI::dbDisconnect(con))

  # Assign variables in the current R environment (= within the function)
  # assign("lkd_db", RODBC::sqlFetch(con, "tLokaler") |> tibble::as_tibble(), envir = parent.frame(n = 2))
  # Men detta steg funkar inte i R 4.3.0, behöver sätta parent.frame(n = 3). Varför?

  purrr::map2(names(tbls), tbls, \(x, y) {
    base::assign(
      x, DBI::dbReadTable(con, y) |>
        tibble::as_tibble(),
      envir = base::parent.frame(n = 3)
      # envir = globalenv() # Debug case, assign data to Global environment
    )
  })

  # Re-format sites table, lookup text values/names based on ID-columns
  sites <- sites |>
    dplyr::arrange(LanID, Lokalkod) |>
    named_join(region |> dplyr::select(RegionID, Region), RegionID) |>
    named_join(county |> dplyr::select(LanID, Lan = LanBokstav), LanID) |>
    named_join(municipality |> dplyr::select(KommunID, Kommun), KommunID) |>
    named_join(province |> dplyr::select(LandskapID, Landskap = LandskapBokstav), LandskapID) |>
    named_join(district |> dplyr::select(DistriktID, Distrikt), DistriktID) |>
    named_join(report_area |> dplyr::select(RapportomradeID, Rapportomrade), RapportomradeID) |>
    named_join(sub_area |> dplyr::select(DelomradeID, Delomrade), DelomradeID) |>
    dplyr::relocate(Region:Delomrade, .after = LokalID) |>
    # Drop some non-essential columns (this step might change)
    dplyr::select(-Alias, -KommentarHemlig, -OrtnamnOsakert)

  if (drop_id_columns) {
    sites <- sites |>
      dplyr::select(-c(LanID:DelomradeID))
  }

  if (add_subsites) {
    # Sub-sites, summarize to one row per main site.
    sites_sub_sum <- sites_sub |>
      dplyr::arrange(LokalID, LokalerUnderID) |>
      dplyr::summarize(
        LokalUnder = stringr::str_c(LokalUnder, collapse = " | "),
        .by = LokalID
      )

    sites <- sites |>
      dplyr::left_join(sites_sub_sum, dplyr::join_by(LokalID)) |>
      dplyr::relocate(LokalUnder, .after = Lokalnamn)
  }

  if (add_monitoring) {
    sites <- sites |>
      dplyr::left_join(
        sites_monitoring |> monitoring_summary(),
        dplyr::join_by(LokalID)
      )
  }

  if (spatial | add_coordinates) {

    sites_coords <- get_sites_coords(con)

    if (!is.null(crs)) {
      sites_coords <- sites_coords |>
        sf::st_transform(crs = crs)
    }
  }

  if (spatial) {

    sites <- sites |>
      dplyr::inner_join(
        sites_coords,
        dplyr::join_by(Ortnamn_LOPNR == Lopnr)) |>
      sf::st_sf()

    if (add_coordinates) {
      sites <- sites |> sfc_as_cols(names = coordinate_cols)
    }

  } else if (!spatial) {

    if (add_coordinates) {

      sites <- sites |>
        dplyr::left_join(
          sites_coords |>
            sfc_as_cols(names = coordinate_cols) |>
            sf::st_drop_geometry(),
          dplyr::join_by(Ortnamn_LOPNR == Lopnr)
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

# Sum yearly monitoring posts per site
# This function was really slow in R 4.3.0!
# I hade re-written the function based on dplyr::case_when statements.
# But it was confirmed that code is much faster in R 4.1.3. It is the case_when
# statements that causes trouble.
monitoring_summary <- function(.data, .by_column = LokalID) {
  .data |>
    # dplyr::filter(CensusYear >= year(now()) - 6) |>
    dplyr::arrange(LokalID, CensusYear) |>
    dplyr::summarize(
      n_year_posts = dplyr::n(),
      missing_status = sum(is.na(OvervakningUtfallID), na.rm = TRUE),
      n_surveyed = sum(OvervakningUtfallID > 0, na.rm = TRUE),
      n_occupied = sum(OvervakningUtfallID > 0 & OvervakningUtfallID <= 44, na.rm = TRUE),
      n_not_occupied = sum(OvervakningUtfallID >= 51, na.rm = TRUE),
      n_occupied_nest = sum(OvervakningUtfallID > 0 & OvervakningUtfallID <= 41, na.rm = TRUE),
      n_productive = sum(OvervakningUtfallID >= 21 & OvervakningUtfallID <= 34, na.rm = TRUE),
      first_survey = dplyr::first(CensusYear[OvervakningUtfallID > 0 & !is.na(OvervakningUtfallID)], na_rm = TRUE),
      last_survey = dplyr::last(CensusYear[OvervakningUtfallID > 0 & !is.na(OvervakningUtfallID)], na_rm = TRUE),
      last_occupied = dplyr::last(CensusYear[OvervakningUtfallID > 0 & OvervakningUtfallID <= 44 & !is.na(OvervakningUtfallID)], na_rm = TRUE),
      last_occupied_nest = dplyr::last(CensusYear[OvervakningUtfallID > 0 & OvervakningUtfallID <= 41 & !is.na(OvervakningUtfallID)], na_rm = TRUE),
      last_productive = dplyr::last(CensusYear[OvervakningUtfallID >= 21 & OvervakningUtfallID <= 34 & !is.na(OvervakningUtfallID)], na_rm = TRUE),
      .by = {{ .by_column }}
    )
}

named_join <- function(.data, .lookup_table, .join_cols) {
  .data |>
    dplyr::left_join(
      .lookup_table,
      dplyr::join_by( {{ .join_cols }} )
    )
}

get_sites_coords <- function(con) {
  # Add geographical data (in SWEREF99 TM)
  # Source data is "hard-coded" in the data connection.
  # db_ortnamn_path = "E:/Maps/Ortnamn/GSD-Ortnamn_2012.gpkg"
  # NOTE: The GeoPackage dataset is a linked table in the
  # wtse database backend (Havsorn_Data). MS Access can not
  # deal with spatial datatypes, so it is necessary to
  # reformat the WKT representation of the geometry column in
  # MS Access to simple features.
  # This could be avoided if it is/would be possible to
  # construct an SQL query combining data from tables in
  # more than one data source (e.g. a GeoPackage and an MS Access db).

  str_sql <-
    "SELECT DISTINCT ortnamn.lopnr AS Lopnr, ortnamn.geom
		    FROM tLokaler
		    INNER JOIN ortnamn ON tLokaler.Ortnamn_LOPNR = ortnamn.lopnr
		    ORDER BY ortnamn.lopnr;"

  x <- DBI::dbGetQuery(
    con, str_sql
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      geom = stringr::str_extract(geom, "(?<=X').*(?=')")
    )

  x$geom <- sf::st_as_sfc(
    structure(as.list(x$geom), class = "WKB"), EWKB = TRUE
  )

  x |> sf::st_sf()
}

# Format site codes to common format
#' @export
wtse_sites_str <- function(.x, pattern = "([0-9]{1,3})") {
  .x |>
    # remove all spaces
    stringr::str_replace_all("[[:blank:]]", "") |>
    # Extract and pad numeric part
    stringr::str_replace_all(
      pattern,
      \(x) stringr::str_pad(x, width = 3, pad = "0")) |>
    # remove any trailing inland "i"
    stringr::str_replace("([i]{1}$)", "") |>
    stringr::str_to_upper() |>
    stringr::str_trim()
}

#' @export
wtse_sites_str_old <- function(.x) {
  .x |>
    stringr::str_replace_all("[[:blank:]]", "") |>
    stringr::str_sub(2) |>
    stringr::str_remove_all("^0+(?!$|([A-Za-z]{1}))") |>
    stringr::str_replace_all("\\/0+", "/") |>
    stringr::str_c(
      stringr::str_sub(.x, 1, 1), ...=_)
}

# x <- tibble::tibble(lokalkod = c("E2/4", " H7b   ", "C98i", "C98 i", "C9 8 i", "C3B2", "X4", "X004", "x4b", "X4b", "C3/15"))
# x |>
#   dplyr::mutate(lokalkod_test = wtse_sites_str(lokalkod) |> wtse_sites_str(.))
# OBS! Denna funktion gör inga förändringar för koder som C315 och C917!
#
# .x <- c("B1", "B11", "B11B", "B111", "E2/4", "C3/15", "C3B2", "B1 ", "B102i")
# wtse_sites_str(.x)
#
# Change "the other way", i.e. remove zeros
# .x <- c("B020", "B020B", "B001", "B011", "B011B", "B111", "E002/004", "C003/015", "C003B2", "B001 ", "B100", "B101", "B102i")
# wtse_sites_str_old(.x)
#
# Extract numeric part
# .x |> stringr::str_extract_all("[[:digit:]]+")
#
# Extract characters:
# 0*(\\d+)

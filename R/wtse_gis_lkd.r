# If spatial = TRUE converts data to sf object,
# and removes all points without spatial information.
# to retrieve data in non-spatial form, use spatial = FALSE
# Prior to R 4.2, encoding = 'windows-1252' was used with
# connection made with DBI-package, but the new native
# UTF8 encoding support in R >= 4.2, does not work with MS Access
# databases. RODBC connection is therefore used as standard connection.
#' @export
wtse_lkd <- function(
    odbc_name = "Havsorn_Data",
    spatial = TRUE,
    crs = NULL,
    add_coordinates = FALSE,
    names = c("x", "y"),
    db_package = c("RODBC", "DBI"),
    encoding = "",
    # encoding = "windows-1252",
    db_ortnamn_path = "E:/Maps/Ortnamn/GSD-Ortnamn_2012.gpkg") {

  db_package <- match.arg(db_package)

  # Importera tabeller.
  # x = namn på objekt som skapas i R, y = motsvarande
  # namn på tabeller i databasen.
  l <- list(
    x = list("lkd_db", "lkd_under_db",
             "lkd_db_overv",
             "lan", "kommun",
             "landskap", "distrikt",
             "rapportomr", "delomrade",
             "region"),
    y = list("tLokaler", "tLokalerUnder",
             "tLokalerOvervakning",
             "luGeografiLan", "luGeografiKommun",
             "luGeografiLandskap", "luGeografiDistrikt",
             "luGeografiRapportomrade", "luGeografiDelomrade",
             "luGeografiRegion"))

  # Ange vilken funktion som ska användas för att läsa in tabeller
  if (db_package == "DBI") {
    db_data_conn <- DBI::dbConnect(odbc::odbc(), odbc_name, encoding = encoding)
    db_f <- function (...) DBI::dbReadTable(...)
  } else if (db_package == "RODBC") {
    db_data_conn <- RODBC::odbcConnect(dsn = odbc_name)
    db_f <- function(...) RODBC::sqlFetch(...)
  }

  # "Läs in" data till variablerna x.
  # Exempel på vad som egentligen sker:
  # assign("lkd_db", RODBC::sqlFetch(db_data_conn, "tLokaler") |> tibble::as_tibble(), envir = parent.frame(n = 2))
  # Men detta steg funkar inte i R 4.3.0, behöver sätta parent.frame(n = 3). Varför?
  purrr::pmap(l, function(x, y) {
    base::assign(x, db_f(db_data_conn, y) |> tibble::as_tibble(),
           envir = base::parent.frame(n = 3)) })

  if (db_package == "RODBC") {
    lkd_db <- lkd_db |> dplyr::mutate(BoplatsOkand = base::as.logical(BoplatsOkand == 1))
  }

  # Underlokaler
  lkd_under_db <- lkd_under_db |>
    dplyr::arrange(LokalID, LokalerUnderID) |>
    dplyr::group_by(LokalID) |>
    dplyr::summarize(LokalUnder = stringr::str_c(LokalUnder, collapse = " | "))

  # Re-investigate here again, use named vectors or perhaps a map statement?
	lkd <- lkd_db |>
	  dplyr::arrange(LanID, Lokalkod) |>
	  dplyr::left_join(region |> dplyr::select(RegionID, Region), dplyr::join_by(RegionID)) |>
	  dplyr::left_join(lan |> dplyr::select(LanID, Lan = LanBokstav), dplyr::join_by(LanID)) |>
	  dplyr::left_join(kommun |> dplyr::select(KommunID, Kommun), dplyr::join_by(KommunID)) |>
	  dplyr::left_join(landskap |> dplyr::select(LandskapID, Landskap = LandskapBokstav), dplyr::join_by(LandskapID)) |>
	  dplyr::left_join(distrikt |> dplyr::select(DistriktID, Distrikt), dplyr::join_by(DistriktID)) |>
	  dplyr::left_join(rapportomr |> dplyr::select(RapportomradeID, Rapportomrade), dplyr::join_by(RapportomradeID)) |>
	  dplyr::left_join(delomrade |> dplyr::select(DelomradeID, Delomrade), dplyr::join_by(DelomradeID)) |>
	  dplyr::relocate(Region:Delomrade, .after = LokalID) |>
	  dplyr::select(
	    -c(LanID:DelomradeID),
	    -Alias, -KommentarHemlig, -OrtnamnOsakert)

	lkd <- lkd |>
	  dplyr::left_join(lkd_under_db, dplyr::join_by(LokalID)) |>
	  dplyr::relocate(LokalUnder, .after = Lokalnamn)

	# Summera årsposter och koppla till lkd
	# This step is really slow in R 4.3.0 - investigate why!
	# Confirmed that code is much faster in R 4.1.3. It is the case_when
	# statements that causes trouble.
	lkd_sum <- lkd_db_overv |>
	  # dplyr::filter(CensusYear >= year(now()) - 6) |>
	  dplyr::arrange(LokalID, CensusYear) |>
	  dplyr::group_by(LokalID) |>
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
	    last_productive = dplyr::last(CensusYear[OvervakningUtfallID >= 21 & OvervakningUtfallID <= 34 & !is.na(OvervakningUtfallID)], na_rm = TRUE))

	lkd <- lkd |>
	  dplyr::left_join(lkd_sum, dplyr::join_by(LokalID))

	if (spatial | add_coordinates) {

	  # Add geographical data, SWEREF 99 TM
	  lkd_koord_sql <-
	    "SELECT DISTINCT ortnamn.lopnr AS Lopnr, ortnamn.ykoord AS Easting, ortnamn.xkoord AS Northing
		    FROM tLokaler
		    INNER JOIN ortnamn ON tLokaler.Ortnamn_LOPNR = ortnamn.lopnr
		    ORDER BY ortnamn.lopnr;"

	  if (db_package == "DBI") {
	    db_f_coords <- function(...) DBI::dbGetQuery(...)
	  } else if (db_package == "RODBC") {
	    db_f_coords <- function(...) RODBC::sqlQuery(...)
	  }

	  lkd_koord <- db_f_coords(db_data_conn, lkd_koord_sql) |>
	    tibble::as_tibble()
	}

	if (spatial) {

	  lkd_sf <- lkd |>
	    dplyr::inner_join(lkd_koord, dplyr::join_by(Ortnamn_LOPNR == Lopnr)) |>
	    sf::st_as_sf(coords = c("Easting", "Northing"), crs = 3006)

	  if (!is.null(crs)) lkd_sf <- lkd_sf |> sf::st_transform(crs = crs)
	  if (add_coordinates) lkd_sf <- lkd_sf |> sfc_as_cols(names = names)

	  return(lkd_sf)
	} else if (!spatial) {
	  if (add_coordinates) {
	    if(!is.null(crs)) {

	      lkd_koord <- lkd_koord |>
	        sf::st_as_sf(coords = c("Easting", "Northing"), crs = 3006) |>
	        sf::st_transform(crs) |>
	        sfc_as_cols(names = names) |>
	        sf::st_drop_geometry() |>
	        tibble::as_tibble()

	      lkd <- lkd |>
	        dplyr::left_join(lkd_koord, dplyr::join_by (Ortnamn_LOPNR == Lopnr))

	    } else {

	      lkd <- lkd |>
	        dplyr::left_join(lkd_koord, dplyr::join_by(Ortnamn_LOPNR == Lopnr)) |>
	        dplyr::rename_with(~ names, c(Easting, Northing))
	    }
	  }
	  return(lkd)
	}

	if (db_package == "DBI") {
		on.exit(DBI::dbDisconnect(db_data_conn))
	} else if (db_package == "RODBC") {
	  on.exit(RODBC::odbcClose(db_data_conn))
	}

	return(lkd)
}

# Examples ----
# Problem with RODBC: doesn't close connection properly if used within function
# "In .Internal(gc(verbose, reset, full)) : closing unused RODBC handle"

# Differences RODBC / DBI -package
# RODBC does not read logical fields properly, must convert 0/1 to TRUE/FALSE
# Date/time: tzone attributes are inconsistent. DBI uses UTC (but is this correct?)
# lkd_dbi <- wtse_lkd(spatial = FALSE, db_package = "DBI", encoding = "")
# lkd_rodbc <- wtse_lkd(spatial = FALSE, db_package = "RODBC")
#
# lkd_rodbc <- lkd_rodbc |>
#   dplyr::mutate(BoplatsOkand = as.logical(BoplatsOkand == 1))
#
# all.equal(lkd_dbi, lkd_rodbc)
#
# chk_inds <- which(lkd_dbi$KommentarPublik != lkd_rodbc$KommentarPublik)
# lkd_dbi$KommentarPublik[chk_inds]
# lkd_rodbc$KommentarPublik[chk_inds]
#
# lkd_dbi |> dplyr::count(Region)
# lkd_rodbc |> dplyr::count(Region)
#
# lkd_dbi |> dplyr::select(BoplatsOkand, date_created, date_modified)
# lkd_rodbc |> dplyr::select(BoplatsOkand, date_created, date_modified)
#
# wtse_lkd(spatial = FALSE, add_coordinates = TRUE)
# wtse_lkd(spatial = TRUE, add_coordinates = TRUE)
# wtse_lkd(spatial = FALSE, crs = 3847, add_coordinates = TRUE, names = c("Easting", "Northing"))
# wtse_lkd(spatial = TRUE, crs = 4326, add_coordinates = FALSE)
# wtse_lkd(spatial = TRUE, crs = 4326, add_coordinates = TRUE, names = c("lat", "long"))

# Format site codes to common format
#' @export
wtse_lkd_str <- function(.x, pattern = "([0-9]{1,3})") {
	.x |>
  # remove all spaces
	stringr::str_replace_all("[[:blank:]]", "") |>
	# Extract and pad numeric part
  stringr::str_replace_all(
    pattern,
    \(x) stringr::str_pad(x, width = 3, pad = "0")) |>
	# remove any trailing inland "i"
	stringr::str_replace("([i]{1}$)", "") |>
	# convert all characters to upper
	stringr::str_to_upper() |>
	# trim whitespace
	stringr::str_trim()
}

#' @export
wtse_lkd_str_old <- function(.x) {
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
#   dplyr::mutate(lokalkod_test = wtse_lkd_str(lokalkod) |> wtse_lkd_str(.))
# OBS! Denna funktion gör inga förändringar för koder som C315 och C917!
#
# .x <- c("B1", "B11", "B11B", "B111", "E2/4", "C3/15", "C3B2", "B1 ", "B102i")
# wtse_lkd_str(.x)
#
# Change "the other way", i.e. remove zeros
# .x <- c("B020", "B020B", "B001", "B011", "B011B", "B111", "E002/004", "C003/015", "C003B2", "B001 ", "B100", "B101", "B102i")
# wtse_lkd_str_old(.x)
#
# Extract numeric part
# .x |> stringr::str_extract_all("[[:digit:]]+")
#
# Extract characters:
# 0*(\\d+)

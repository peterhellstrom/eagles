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
    encoding = 'windows-1252',
    db_ortnamn_path = "E:/Maps/Ortnamn/GSD-Ortnamn_2012.gpkg") {
	# requires: tidyverse, DBI, sf

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
  # assign("lkd_db", RODBC::sqlFetch(db_data_conn, "tLokaler") %>% as_tibble(), envir = parent.frame(n = 2))
  # Men detta steg funkar inte i R 4.3.0, behöver sätta parent.frame(n = 3). Varför?
  pmap(l, function(x, y) {
    assign(x, db_f(db_data_conn, y) %>% as_tibble(),
           envir = parent.frame(n = 3)) })

  if (db_package == "RODBC") {
    lkd_db <- lkd_db %>% mutate(BoplatsOkand = as.logical(BoplatsOkand == 1))
  }

  # Underlokaler
  lkd_under_db <- lkd_under_db %>%
    arrange(LokalID, LokalerUnderID) %>%
    group_by(LokalID) %>%
    summarize(LokalUnder = str_c(LokalUnder, collapse = " | "))

  # Re-investigate here again, use named vectors or perhaps a map statement?
	lkd <- lkd_db %>%
	  arrange(LanID, Lokalkod) %>%
	  left_join(region %>% select(RegionID, Region), by = "RegionID") %>%
	  left_join(lan %>% select(LanID, Lan = LanBokstav), by = "LanID") %>%
	  left_join(kommun %>% select(KommunID, Kommun), by = "KommunID") %>%
	  left_join(landskap %>% select(LandskapID, Landskap = LandskapBokstav), by = "LandskapID") %>%
	  left_join(distrikt %>% select(DistriktID, Distrikt), by = "DistriktID") %>%
	  left_join(rapportomr %>% select(RapportomradeID, Rapportomrade), by = "RapportomradeID") %>%
	  left_join(delomrade %>% select(DelomradeID, Delomrade), by = "DelomradeID") %>%
	  relocate(Region:Delomrade, .after = LokalID) %>%
	  select(
	    -c(LanID:DelomradeID),
	    -Alias, -KommentarHemlig, -OrtnamnOsakert)

	lkd <- lkd %>%
	  left_join(lkd_under_db, by = "LokalID") %>%
	  relocate(LokalUnder, .after = Lokalnamn)

	# Summera årsposter och koppla till lkd
	# This step is really slow in R 4.3.0 - investigate why!
	# Confirmed that code is much faster in R 4.1.3. It is the case_when
	# statements that causes trouble.
	lkd_sum <- lkd_db_overv %>%
	  #filter(CensusYear >= year(now()) - 6) %>%
	  arrange(LokalID, CensusYear) %>%
	  group_by(LokalID) %>%
	  summarize(
	    n_year_posts = n(),
	    missing_status = sum(is.na(OvervakningUtfallID), na.rm = TRUE),
	    n_surveyed = sum(OvervakningUtfallID > 0, na.rm = TRUE),
	    n_occupied = sum(OvervakningUtfallID > 0 & OvervakningUtfallID <= 44, na.rm = TRUE),
	    n_not_occupied = sum(OvervakningUtfallID >= 51, na.rm = TRUE),
	    n_occupied_nest = sum(OvervakningUtfallID > 0 & OvervakningUtfallID <= 41, na.rm = TRUE),
	    n_productive = sum(OvervakningUtfallID >= 21 & OvervakningUtfallID <= 34, na.rm = TRUE),
	    first_survey = first(CensusYear[OvervakningUtfallID > 0 & !is.na(OvervakningUtfallID)], na_rm = TRUE),
	    last_survey = last(CensusYear[OvervakningUtfallID > 0 & !is.na(OvervakningUtfallID)], na_rm = TRUE),
	    last_occupied = last(CensusYear[OvervakningUtfallID > 0 & OvervakningUtfallID <= 44 & !is.na(OvervakningUtfallID)], na_rm = TRUE),
	    last_occupied_nest = last(CensusYear[OvervakningUtfallID > 0 & OvervakningUtfallID <= 41 & !is.na(OvervakningUtfallID)], na_rm = TRUE),
	    last_productive = last(CensusYear[OvervakningUtfallID >= 21 & OvervakningUtfallID <= 34 & !is.na(OvervakningUtfallID)], na_rm = TRUE))

	lkd <- lkd %>%
	  left_join(lkd_sum, by = "LokalID")

	if (spatial | add_coordinates) {

	  # Add geographical data, SWEREF 99 TM
	  lkd_koord_sql <-
	    "SELECT DISTINCT ortnamn.lopnr AS Lopnr, ortnamn.ykoord AS Easting, ortnamn.xkoord AS Northing
		    FROM tLokaler
		    INNER JOIN ortnamn ON tLokaler.Ortnamn_LOPNR = ortnamn.lopnr
		    ORDER BY ortnamn.lopnr;"

	  if (db_package == "DBI") {
	    db_f_coords <- DBI::dbGetQuery
	  } else if (db_package == "RODBC") {
	    db_f_coords <- RODBC::sqlQuery
	  }

	  lkd_koord <- db_f_coords(db_data_conn, lkd_koord_sql) %>%
	    as_tibble()
	}

	if (spatial) {

	  lkd_sf <- lkd %>%
	    inner_join(lkd_koord, by = c("Ortnamn_LOPNR" = "Lopnr")) %>%
	    sf::st_as_sf(coords = c("Easting", "Northing"), crs = 3006)

	  if (!is.null(crs)) lkd_sf <- lkd_sf %>% st_transform(crs = crs)
	  if (add_coordinates) lkd_sf <- lkd_sf %>% sfc_as_cols(names = names)

	  return(lkd_sf)
	} else if (!spatial) {
	  if (add_coordinates) {
	    if(!is.null(crs)) {

	      lkd_koord <- lkd_koord %>%
	        sf::st_as_sf(coords = c("Easting", "Northing"), crs = 3006) %>%
	        st_transform(crs) %>%
	        sfc_as_cols(names = names) %>%
	        st_drop_geometry() %>%
	        as_tibble()

	      lkd <- lkd %>%
	        left_join(lkd_koord, by = c("Ortnamn_LOPNR" = "Lopnr"))

	    } else {

	      lkd <- lkd %>%
	        left_join(lkd_koord, by = c("Ortnamn_LOPNR" = "Lopnr")) %>%
	        rename_with(~ names, c(Easting, Northing))
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

# system.time(lkd_dbi <- wtse_lkd(spatial = FALSE, db_package = "DBI"))
# system.time(lkd_rodbc <- wtse_lkd(spatial = FALSE, db_package = "RODBC"))
#
# lkd_rodbc <- lkd_rodbc %>%
#   mutate(BoplatsOkand = as.logical(BoplatsOkand == 1))
#
# all.equal(lkd_dbi, lkd_rodbc)
#
# lkd_dbi %>% count(Region)
# lkd_rodbc %>% count(Region)
#
# lkd_dbi %>% select(BoplatsOkand, date_created, date_modified)
# lkd_rodbc %>% select(BoplatsOkand, date_created, date_modified)

# wtse_lkd(spatial = FALSE, add_coordinates = TRUE)
# wtse_lkd(spatial = TRUE, add_coordinates = TRUE)
# wtse_lkd(spatial = FALSE, crs = 3847, add_coordinates = TRUE, names = c("Easting", "Northing"))
# wtse_lkd(spatial = TRUE, crs = 4326, add_coordinates = FALSE)
# wtse_lkd(spatial = TRUE, crs = 4326, add_coordinates = TRUE, names = c("lat", "long"))

# Format site codes to common format
#' @export
wtse_lkd_str <- function(.x) {
	# remove all spaces
	stringr::str_replace_all(.x, "[[:blank:]]", "") %>%
	# Extract and pad numeric part
	stringr::str_replace(., "([0-9]{1,3})",
		stringr::str_extract(., "([0-9]{1,3})") %>%
			stringr::str_pad(., width = 3, pad = "0")) %>%
	# remove any trailing inland "i"
	stringr::str_replace(., "([i]{1}$)", "") %>%
	# convert all characters to upper
	stringr::str_to_upper() %>%
	# trim whitespace
	stringr::str_trim(.)
}

#' @export
wtse_lkd_str_old <- function(.x) {
  .x %>%
    stringr::str_replace_all(., "[[:blank:]]", "") %>%
    stringr::str_sub(., 2) %>%
    str_remove_all(., "^0+(?!$|([A-Za-z]{1}))") %>%
    str_replace_all(., "\\/0+", "/") %>%
    str_c(stringr::str_sub(.x, 1, 1), .)
}

# Examples ----
# library(tidyverse)
# x <- tibble(lokalkod = c("E2/4", " H7b   ", "C98i", "C98 i", "C9 8 i", "C3B2", "X4", "X004", "x4b", "X4b", "C3/15"))
# x %>%
#   mutate(lokalkod_test = wtse_lkd_str(lokalkod) %>% wtse_lkd_str(.))
# # OBS! Denna funktion gör inga förändringar för koder som C315 och C917!
#
# y <- c("B1", "B11", "B11B", "B111", "E2/4", "C3/15", "C3B2", "B1 ")
# wtse_lkd_str(y)
#
# # Change "the other way", i.e. remove zeros
# z <- c("B020", "B020B", "B001", "B011", "B011B", "B111", "E002/004", "C003/015", "C003B2", "B001 ", "B100", "B101", "B102i")
# wtse_lkd_str_old(z)
#
# # Extract numeric part
# #z %>% str_extract_all(., "[[:digit:]]+")
#
# # Extract characters:
# # 0*(\\d+)

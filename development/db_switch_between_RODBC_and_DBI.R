# Switch between DBI and RODBC when importing data.
# Basic example function!
# Not complete, since we need to pass extra arguments to functions somehow.
# For RODBC, it is for instance necessary to add the argument as.is = TRUE

# Date/time: tzone attributes are inconsistent. DBI uses UTC (but is this correct?)
# DBI and RODBC also treat logical columns differently

db_odbc_switch <- function(
    db_package = c("DBI", "RODBC"),
    odbc_name = "Havsorn_Data",
    table = "tLokaler",
    str_sql = "SELECT LokalID, LanID, Lokalkod, Lokalnamn FROM tLokaler ORDER BY LokalID",
    encoding = "",
    ...) {

  db_package <- match.arg(db_package)

  if (db_package == "DBI") {
    con <- DBI::dbConnect(odbc::odbc(), odbc_name, encoding = encoding)
    f_get_table <- function (...) DBI::dbReadTable(...)
    f_get_query <- function(...) DBI::dbGetQuery(...)
    on.exit(DBI::dbDisconnect(con))
  } else if (db_package == "RODBC") {
    con <- RODBC::odbcConnect(dsn = odbc_name)
    f_get_table <- function(...) RODBC::sqlFetch(...)
    f_get_query <- function(...) RODBC::sqlQuery(...)
    on.exit(RODBC::odbcClose(con))
  }

  list(
    table = f_get_table(con, table) |>
      tibble::as_tibble(),
    query = f_get_query(con, str_sql) |>
      tibble::as_tibble()
  )

}

db_odbc_switch("DBI")
db_odbc_switch("RODBC")

tmp_db <- function(
    sql_query,
    path = "W:/projects/data/raptorbase/RaptorBase_Data.accdb") {

  con <- RODBC::odbcConnectAccess2007(path)
  out <- RODBC::sqlQuery(con, sql_query, as.is = FALSE) |>
    tibble::as_tibble()
  on.exit(close(con))
  out
}

s <- c("SELECT * FROM tCoordinates", "SELECT * FROM tMonitoringSeason")
d <- map(s, \(x) tmp_db(x))


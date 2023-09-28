library(tidyverse)
library(eagles)

db_debug_connection <- function(access_file, package, ...) {

  con <- db_connect_to_access(access_file, package = package, ...)
  on.exit(close_fn(con))

  if (package == "DBI") {
    data_fn = DBI::dbListTables
    close_fn = DBI::dbDisconnect
  } else if (package == "RODBC") {
    data_fn = RODBC::sqlTables
    close_fn = RODBC::odbcClose
  }

  data_fn(con)
}

# db_debug_connection("W:\\projects\\data\\Havsorn_Data.accdb", "DBI")

test_secret <- pmap(
  list(access_file = "W:\\projects\\data\\Havsorn_Data.accdb",
       package = rep(c("DBI", "RODBC"), times = 2),
       pwd = rep(c("", Sys.getenv("db_wtse")), each = 2)
  ),
  db_debug_connection)

test_open <- pmap(
  list(access_file = "W:\\projects\\data\\raptorbase\\RaptorBase_Data.accdb",
       package = c("DBI", "RODBC")
  ),
  db_debug_connection)

glimpse(test_secret)
glimpse(test_open)

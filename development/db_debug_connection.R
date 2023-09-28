library(tidyverse)
library(eagles)

# Connect to open and password-protected Access db with DBI and RODBC ----

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

# Import data; use file path or existing connection ----

data_dir <- "W:/projects/data/ringing/data"
data_file <- "RingDb0392.mdb"
package <- "DBI"

## Use a file path as input ----
ringdb <- file.path(data_dir, data_file)

system.time(
  ringon <- db_import_access(
    ringdb,
    "SELECT * FROM Ringon",
    package = "DBI"
  )
)

## Use an existing database connection ----
ringdb <- db_connect_to_access(
  file.path(data_dir, data_file),
  package = package
)

system.time(
  ringon <- db_import_access(
    ringdb,
    "SELECT * FROM Ringon",
    package = package
  )
)

## Close connections ----
if (package == "RODBC") {
  RODBC::odbcClose(ringdb)
} else if(package == "DBI") {
  DBI::dbDisconnect(ringdb)
}

# Frequent error message?:
# "closing unused RODBC handle "

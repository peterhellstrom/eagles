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

# Define paths to test databases ----
db_open <- "W:\\projects\\data\\raptorbase\\RaptorBase_Data.accdb"
db_secret <- "W:\\projects\\data\\Havsorn_Data.accdb"

# 2) Test db_connect_to_access() ----
con_open_dbi <- db_connect_to_access(db_open, package = "DBI")
con_open_dbi
DBI::dbDisconnect(con_open_dbi)

con_open_rodbc <- db_connect_to_access(db_open, package = "RODBC")
con_open_rodbc
RODBC::odbcClose(con_open_rodbc)

con_secret_dbi <- db_connect_to_access(db_secret, package = "DBI")
con_secret_dbi
DBI::dbDisconnect(con_secret_dbi)

con_secret_rodbc <- db_connect_to_access(db_secret, package = "RODBC")
con_secret_rodbc
RODBC::odbcClose(con_secret_rodbc)

con_secret_dbi_pwd <- db_connect_to_access(db_secret, package = "DBI", pwd = Sys.getenv("db_wtse"))
con_secret_dbi_pwd
DBI::dbDisconnect(con_secret_dbi_pwd)

# 3) Test wrapper function db_debug_connection() ----

## Single runs ----
db_debug_connection(db_secret, "DBI")
db_debug_connection(db_secret, "RODBC")

## All combinations
test_secret <- pmap(
  list(
    access_file = db_secret,
    package = rep(c("DBI", "RODBC"), times = 2),
    pwd = rep(c("", Sys.getenv("db_wtse")), each = 2)
  ),
  db_debug_connection)

test_open <- pmap(
  list(
    access_file = db_open,
    package = c("DBI", "RODBC")
  ),
  db_debug_connection)

glimpse(test_secret)
glimpse(test_open)

# 4) Import data; use file path or existing connection ----

data_dir <- "W:/projects/data/ringing/data"
data_file <- "RingDb0392.mdb"
package <- "DBI"
# package <- "RODBC"

## Use a file path as input ----
ringdb <- file.path(data_dir, data_file)

system.time(
  ringon <- db_import_access(
    ringdb,
    "SELECT * FROM Ringon",
    package = package
  )
)

## Use an existing database connection ----
# This should be faster than using a string path as input
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

#' @export
# (Non-exported) function from RODBC-package, RODBC:::full.path()
db_full_path <- function(filename) {
  fn <- gsub("\\", "/", filename, fixed = TRUE)
  is.abs <- length(grep("^[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]:|/", fn)) > 0L
  gsub("/", "\\",
       if(!is.abs) file.path(getwd(), filename)
       else filename, fixed = TRUE
  )
}

#' @export
# Adapted from RODBC::odbcConnectAccess2007()
db_connect_to_access_string <- function(
    access_file, uid = "", pwd = "", ...) {

  con <- if (missing(access_file)) {
    "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq="
  } else {
    paste(
      "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=",
      db_full_path(access_file), ";Uid=", uid, ";Pwd=", pwd, ";",
      sep = ""
    )
  }
  con
}

#access_connect_string("W:/projects/data/RaptorBase/RaptorBase.accdb")

# Connect to MS Access database
# Connect with packages DBI or RODBC
# RODBC - asks for pwd, uid is admin
# DBI - does not ask for password by default.
# Two options available in this function, depending if the function is run
# from RStudio or not.
# Dependencies: RODBC, DBI, getPass
# But works in RStudio when pwd parameter is supplied as below

# Koppla upp via ODBC mot lösenordsskyddad (Access)-databas
# Andra R-alternativ än DSN-uppkoppling:
# RStudio - använd funktionen rstudioapi::askForPassword
# RODBC - om "connection string" inte innehåller lösenordet så visas automatiskt dialogruta
# DBI - använd getPass::getPass()

#' @export
db_connect_to_access <- function(
    access_file,
    uid = "",
    pwd = "",
    encoding = "",
    package = c("DBI", "RODBC"),
    ...)  {

  package <- match.arg(package)

  if (!file.exists(access_file)) {
    stop("Database file does not exist:", access_file)
  }

  # Assemble connection strings:
  # 1) Start with basic connection string without password
  #    (or user supplied password).
  # 2) Check if a connection attempt is successful
  # 3) If step 2 returns FALSE, prompt for password.
  # 4) Re-build connection string with user input password.
  # 5) Make final connection attempt
  db_connect_string <- db_connect_to_access_string(
    access_file, uid = uid, pwd = pwd, ...
  )

  if (package == "DBI") {
    con_chk <- con_chk <- DBI::dbCanConnect(
      odbc::odbc(),
      .connection_string = db_connect_string,
      encoding = encoding)

    if (!con_chk) {
      db_connect_string <- db_connect_to_access_string(
        access_file, uid = uid, pwd = db_pwd_ask(), ...
      )
    }
    con <- DBI::dbConnect(
      odbc::odbc(),
      .connection_string = db_connect_string,
      encoding = encoding
    )

  } else if (package == "RODBC") {
    con_chk <- suppressWarnings(
      RODBC::odbcDriverConnect(db_connect_string))

    if (con_chk == -1) {
      db_connect_string <- db_connect_to_access_string(
        access_file, uid = uid, pwd = db_pwd_ask(), ...
      )
    }

    con <- RODBC::odbcDriverConnect(
      db_connect_string
    )
  }
  con
}

db_pwd_ask <- function() {
  if (Sys.getenv("RSTUDIO") == "") {
    pwd <- getPass::getPass()
  } else {
    pwd <- rstudioapi::askForPassword(
      "Database password"
    )
  }
  pwd
}

#' @export
db_import_access <- function(
    access_file, sql_expr, ...,
    package = c("DBI", "RODBC")
) {

  package <- match.arg(package)

  if(package == "RODBC") {
    con <- RODBC::odbcConnectAccess2007(access_file)
    on.exit(close(con))
    RODBC::sqlQuery(
      con, sql_expr,
      stringsAsFactors = FALSE, as.is = TRUE, ...
    )
  } else if(package == "DBI") {
    con <- DBI::dbConnect(access_file)
    on.exit(DBI::dbDisconnect(con))
    DBI::dbGetQuery(con, sql_expr, ...)
  }
}

#' @export
# Get data with identical structure from multiple Access database files
# fls = character vector with file paths, sql_expr = string with SQL expression
db_import_access_n <- function(
    fls,
    sql_expr,
    fn = db_import_access,
    ...) {
  purrr::map(
    fls |> rlang::set_names(basename(fls)),
    \(x) fn(x, sql_expr, ...)
  ) |>
    purrr::list_rbind(names_to = "source") |>
    tibble::as_tibble() |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.character), \(x) dplyr::na_if(x, "")
      )
    )
}

# Parameterized SQL-queries with RODBCext
#' @export
db_pquery <- function(
    tbl,
    flds,
    type = c("insert", "select", "update", "delete"),
    where_fld = NULL) {

  switch(type,
	insert = paste("INSERT INTO ", tbl, " (", paste(flds, collapse = ", "),
		") VALUES (", stringr::str_sub(paste(rep("?", length(flds)), collapse = ", "), 1, -1), ")", sep = ""),
	select = paste("SELECT ", paste(flds, collapse = ", "), " FROM ", tbl, sep = ""),
	update = gsub(pattern = paste0(where_fld, " = \\?, "), replacement = "", x = paste("UPDATE ", tbl, " SET ",
		paste(flds, collapse = " = ?, "), " = ? WHERE ", where_fld, " = ?;", sep = ""))
	)
}

# db_pquery("tbl", flds = c("a", "b"), type = "insert")
# db_pquery("tbl", flds = c("a", "b"), type = "select")

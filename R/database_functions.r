#' Title
#'
#' @param filename
#'
#' @returns
#' @export
#'
#' @examples
db_full_path <- function(filename) {
  # (Non-exported) function from RODBC-package, RODBC:::full.path()
  fn <- gsub("\\", "/", filename, fixed = TRUE)
  is.abs <- length(grep("^[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]:|/", fn)) > 0L
  gsub("/", "\\",
       if(!is.abs) file.path(getwd(), filename)
       else filename, fixed = TRUE
  )
}

#' Title
#'
#' @param access_file
#' @param uid
#' @param pwd
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
db_connect_to_access_string <- function(
    # Adapted from RODBC::odbcConnectAccess2007()
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

#' Title
#'
#' @param access_file
#' @param uid
#' @param pwd
#' @param encoding
#' @param package
#' @param check_connection
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
db_connect_to_access <- function(
    access_file,
    uid = "",
    pwd = "",
    encoding = "",
    package = c("DBI", "RODBC"),
    check_connection = TRUE,
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
    if (check_connection) {
      con_chk <- DBI::dbCanConnect(
        odbc::odbc(),
        .connection_string = db_connect_string,
        encoding = encoding)

      if (!con_chk) {
        db_connect_string <- db_connect_to_access_string(
          access_file, uid = uid, pwd = db_pwd_ask(), ...
        )
      }
    }

    con <- DBI::dbConnect(
      odbc::odbc(),
      .connection_string = db_connect_string,
      encoding = encoding
    )

  } else if (package == "RODBC") {
    con <- suppressWarnings(
      RODBC::odbcDriverConnect(db_connect_string))

    if (con == -1 | !check_connection) {
      db_connect_string <- db_connect_to_access_string(
        access_file, uid = uid, pwd = db_pwd_ask(), ...
      )
      con <- RODBC::odbcDriverConnect(
        db_connect_string
      )
    }
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

#' Title
#'
#' @param con
#' @param sql_expr
#' @param ...
#' @param package
#' @param tibble
#' @param na_value
#'
#' @returns
#' @export
#'
#' @examples
db_import_access <- function(
    con,
    sql_expr,
    ...,
    package = c("DBI", "RODBC"),
    tibble = TRUE,
    na_value = c("", " ", "  ")
) {

  package <- match.arg(package)

  if (class(con) == "character") {
    con <- db_connect_to_access(con, ..., package = package)
    input <- "file_path"
  } else {
    input <- "connection"
  }

  if(package == "RODBC") {
    if (input == "file_path") on.exit(close(con))
    res <- RODBC::sqlQuery(
      con, sql_expr,
      stringsAsFactors = FALSE, as.is = TRUE
    )
  } else if(package == "DBI") {
    if (input == "file_path") on.exit(DBI::dbDisconnect(con))
    res <- DBI::dbGetQuery(con, sql_expr)
  }

  if (tibble) {
    res <- res |>
      tibble::as_tibble()
  }

  if (!is.null(na_value)) {
    res <- res |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::where(is.character),
          \(x) dplyr::if_else(x %in% na_value, NA_character_, x)
          # \(x) dplyr::na_if(x, na_value)
        )
      )
  }
  res
}

# Get data with identical structure from multiple Access database files
# fls = character vector with file paths, sql_expr = string with SQL expression

#' Title
#'
#' @param fls
#' @param sql_expr
#' @param ...
#' @param source_column
#' @param source_names
#' @param na_value
#'
#' @returns
#' @export
#'
#' @examples
db_import_access_n <- function(
    fls,
    sql_expr,
    ...,
    source_column = "source",
    source_names = basename(fls),
    na_value = c("", " ", "  ")) {
  purrr::map(
    fls |> rlang::set_names(source_names),
    \(x) db_import_access(x, sql_expr, ..., tibble = FALSE, na_value = NULL)
  ) |>
    purrr::list_rbind(names_to = source_column) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.character),
        # \(x) dplyr::na_if(x, "")
        \(x) dplyr::if_else(x %in% na_value, NA_character_, x)
      )
    )
}

# Parameterized SQL-queries with RODBCext

#' Title
#'
#' @param tbl
#' @param flds
#' @param type
#' @param where_fld
#'
#' @returns
#' @export
#'
#' @examples
db_pquery <- function(
    tbl,
    flds,
    type = c("insert", "select", "update", "delete"),
    where_fld = NULL
) {

  switch(
    type,
    insert = paste(
      "INSERT INTO ", tbl, " (", paste(flds, collapse = ", "),
      ") VALUES (", stringr::str_sub(paste(rep("?", length(flds)), collapse = ", "), 1, -1), ")",
      sep = ""
    ),
    select = paste("SELECT ", paste(flds, collapse = ", "), " FROM ", tbl, sep = ""),
    update = gsub(
      pattern = paste0(where_fld, " = \\?, "), replacement = "",
      x = paste(
        "UPDATE ", tbl, " SET ",
        paste(flds, collapse = " = ?, "), " = ? WHERE ", where_fld, " = ?;", sep = ""
      )
    )
  )
}

# db_pquery("tbl", flds = c("a", "b"), type = "insert")
# db_pquery("tbl", flds = c("a", "b"), type = "select")

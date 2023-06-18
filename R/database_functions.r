#' @export
# Functions from RODBC-package
full.path <- function(filename) {
	fn <- gsub("\\", "/", filename, fixed = TRUE)
	is.abs <- length(grep("^[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]:|/", fn)) > 0L
	gsub("/", "\\",
		if(!is.abs) file.path(getwd(), filename)
		else filename, fixed = TRUE)
}

#' @export
# Adapted from RODBC::odbcConnectAccess2007()
db_connect_to_access_string <- function (access.file, uid = "", pwd = "", ...) {
    con <- if (missing(access.file))
        "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq="
    else paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=",
        full.path(access.file), ";Uid=", uid, ";Pwd=", pwd, ";",
        sep = "")
	con
    #RODBC::odbcDriverConnect(con, ...)
}

#access_connect_string("W:/PROJEKT/databaser/RaptorBase/RaptorBase.accdb")

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
db_connect_to_access <- function(db_file_path,
	uid = "", pwd = "",
	encoding = "",
	package = c("RODBC", "DBI"), ...)  {

  #if (Sys.getenv("R_ARCH") == "/x64") {
  #  stop("The MS Access database connection can only be opened with the 32-bit version of R.\nYou are running R in 64-bit mode now!")
  #} else {
    if (!file.exists(db_file_path)) {
      stop("DB file does not exist at ", db_file_path)
    }
    # Assemble connection strings
	if (Sys.getenv("RSTUDIO") == "") {
	  require(getPass)
	  pwd <- getPass::getPass()
	} else {
	  pwd <- rstudioapi::askForPassword("Database password")
	}

    db_connect_string <- db_connect_to_access_string(db_file_path, uid = uid, pwd = pwd, ...)

    if (package == "DBI") {
	  require(DBI)
      myconn <- DBI::dbConnect(odbc::odbc(), .connection_string = db_connect_string, encoding = encoding)
    } else if (package == "RODBC") {
	  require(RODBC)
      myconn <- RODBC::odbcDriverConnect(db_connect_string)
    }
    return(myconn)
  }
#}

#' @export
# Get data with identical structure from multiple Access database files
# fls = character vector with file names, sql_expr = string with SQL expression
# Uses library(RODBC) and library(tidyverse)
db_get_access_data <- function(fls, sql_expr) {
	out <- vector("list", length(fls))
	for (i in 1:length(fls)) {
		db_tmp <- RODBC::odbcConnectAccess2007(fls[i])
		out[[i]] <- RODBC::sqlQuery(db_tmp, sql_expr, stringsAsFactors = FALSE, as.is = TRUE)
		close(db_tmp)
	}
	out <- do.call("rbind", out)
	as_tibble(na_if(out, ""))
}

# Parameterized SQL-queries with RODBCext
#' @export
db_pquery <- function(tbl, flds, type = c("insert", "select", "update", "delete"), where_fld = NULL) {
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

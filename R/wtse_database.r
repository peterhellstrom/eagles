#' @export
move_new_jpg <- function(jpg_dir, db_dir_base, k_id, v_id, seq_start,
                         overwrite = FALSE, execute = TRUE, sep = ".",
						 pattern = ".jpg") {

	setwd(jpg_dir)
	x <- list.files(pattern = pattern)
	x_new <- paste0(
	  "k", str_pad(k_id, width = 2, pad = 0),
	  sep, "v", str_pad(v_id, width = 4, pad = 0),
	  sep, "b", str_pad(seq(seq_start, seq_start + length(x) - 1), width = 4, pad = 0), ".jpg")

	if (execute) file.rename(x, x_new)

	db_dir <- paste0(
	  "k", str_pad(k_id, width = 2, pad = 0),
	  "/v", str_pad(v_id, width = 4, pad = 0))

	db_dir_path <- file.path(db_dir_base, db_dir)

	if (execute) {
		if (!dir.exists(db_dir_path)) dir.create(db_dir_path)
		if (dir.exists(db_dir_path)) {
			file.copy(x_new,
				file.path(db_dir_path, x_new),
				overwrite = overwrite,
				copy.mode = TRUE,
				copy.date = TRUE)
		}
		#unlink(jpg_dir, recursive = TRUE)
	}

	invisible(list(x = x, x_new = x_new, db_dir = db_dir, db_dir_path = db_dir_path))
}

# Helper functions
#' @export
get_dir_path <- function(k_id, v_id) {
	db_dir <- paste0(
	  "k", str_pad(k_id, width = 2, pad = 0),
	  "/v", str_pad(v_id, width = 4, pad = 0))
	db_dir
}

#' @export
set_file_name <- function(k_id, v_id, b_id, sep = ".", pattern = ".jpg") {
	paste0(
		"k", str_pad(k_id, width = 2, pad = 0),
		sep, "v", str_pad(v_id, width = 4, pad = 0),
		sep, "b", str_pad(b_id, width = 4, pad = 0), pattern)
}

#' @export
get_seq_start <- function(db_dir_base, db_dir_path, pattern = "*.jpg", start = 12, stop = 15) {
	get_path <- file.path(db_dir_base, db_dir_path)
	if (dir.exists(get_path)) {
		x <- list.files(path = get_path, pattern = pattern)
		max(as.numeric(substr(x = x, start = start, stop = stop))) + 1
	} else {
		cat("Specified directory does not exist!", "\n")
	}
}

#' @export
insert_arkiv <- function(k_id, v_id,
                         seq_start = 1,
                         observator_text,
                         typ_data, typ_handling,
                         tidsperiod,
                         dsn = "Havsorn_Data",
                         db_dir_base,
                         pattern = ".jpg",
                         execute = FALSE) {

  # List existing files
  x <- list.files(file.path(db_dir_base, get_dir_path(k_id, v_id)), pattern = pattern)

  x <- x %>%
    as_tibble() %>%
    rename(Filnamn = value)

  # Construct data
  sql_insert_data <- x %>%
    mutate(VolymID = v_id,
           Lopnr = seq(seq_start, length.out = nrow(.), by = 1),
           KontaktText = observator_text,
           TypDataID = typ_data,
           TypHandlingID = typ_handling,
           TidsPeriod = tidsperiod) %>%
    select(VolymID, Lopnr, Filnamn, KontaktText, TypDataID, TypHandlingID, TidsPeriod)

    sql_insert_list <- sql_insert_data %>%
      as.list()

  # Check if data already exists - only add non-existing file names!
  con_rodbc <- RODBC::odbcConnect(dsn = dsn)
  fls_exist <- RODBC::sqlQuery(con_rodbc,
                               paste0("SELECT * FROM tArkivDok WHERE Filnamn IN (",
                                      str_c(paste0("'",pull(sql_insert_data, Filnamn), "'"),
                                            collapse = ", "), ")"))

  sql_insert_data <- sql_insert_data %>%
    as_tibble() %>%
    anti_join(fls_exist, by = "Filnamn") %>%
    #mutate(Lopnr = seq_start:(seq_start + (nrow(.) - 1))) %>%
    as.list()

  if (execute) {

    sql_insert_str <-
      "INSERT INTO tArkivDok (VolymID, Lopnr, Filnamn, KontaktText, TypDataID, TypHandlingID, TidsPeriod) VALUES (?, ?, ?, ?, ?, ?, ?)"

    RODBCext::sqlExecute(con_rodbc, sql_insert_str, data = sql_insert_data)

    # Check if data were inserted to db
    #sql_str_chk <- paste0("SELECT * FROM tArkivDok WHERE VolymID = ", v_id)
    #tmp <- RODBCext::sqlExecute(con_rodbc, sql_str_chk, fetch = TRUE, as.is = TRUE) %>%
    #  as_tibble() %>%
    #  select(VolymID, Lopnr, Filnamn, KontaktText, TypDataID, TypHandlingID, TidsPeriod)


    # with DBI-package, does not work with Access databases
    #sql_insert <- DBI::dbSendQuery(con, sql_str)
    #dbBind(sql_insert, sql_insert_data)
    #dbClearResult(sql_insert)
    #dbDisconnect(con)
  } else {
      return(sql_insert_data)
  }

  odbcClose(con_rodbc)
  close(con_rodbc)
}

# Create bat-file function ----
# Behöver ändra teckenkodning i bat-filen, om filnamnet innehåller "special characters" som åäöÅÄÖ
# https://stackoverflow.com/questions/43046559/why-are-danish-characters-not-displayed-as-in-text-editor-on-executing-batch-fil
# I exemplet ovan använder jag ISO-8859-1
# https://docs.microsoft.com/en-us/windows/win32/intl/code-page-identifiers

# Q: Is it possible to use relative links with IrfanView thumbnails?
# This example uses absolute links
# But it appears that relative paths can work, if they are expressed relative to i_view64.exe,
# which can be moved. Put the exe-file in the picture maps!

#' @export
thumbs_bat <- function(files, output_file, output_dir = getwd(),
                       sub_dir = TRUE,
                       relative = FALSE,
                       i_view_path = "C:/Program Files/IrfanView/i_view64.exe") {

  # Write file list
  if (!dir.exists(output_dir)) dir.create(output_dir)

  if (sub_dir) {
    filelist_file <- file.path(output_dir, "filelist", paste0(output_file, ".txt"))
    chk_dir <- file.path(output_dir, "filelist")
    if (!dir.exists(chk_dir)) dir.create(chk_dir)
  } else {
    filelist_file <- file.path(output_dir, paste0(output_file, ".txt"))
  }

  # Write contents of filelist
  write.table(files,
              filelist_file,
              row.names = FALSE,
              col.names = FALSE)

  if (relative) {
    filelist_file <- str_replace(filelist_file, paste0(output_dir, "/"), "")
    i_view_path <- basename(i_view_path)
  }

  bat_content <- paste0('"', i_view_path, '" /thumbs /filelist="', filelist_file, '"')

  bat_file <- file.path(output_dir, paste0(output_file, ".bat"))

  # Connect/create bat-file
  file_conn <- file(bat_file)
  # Write data to bat_file
  writeLines(c("chcp 28591", bat_content), file_conn)
  close(file_conn)
}

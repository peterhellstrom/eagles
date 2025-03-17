#' Title
#'
#' @param k_id
#' @param v_id
#'
#' @returns
#' @export
#'
#' @examples
get_dir_path <- function(k_id, v_id) {
  sprintf('k%02d/v%04d', k_id, v_id)
}

#' Title
#'
#' @param k_id
#' @param v_id
#' @param b_id
#' @param sep
#' @param file_extension
#'
#' @returns
#' @export
#'
#' @examples
set_file_name <- function(
    k_id,
    v_id,
    b_id,
    sep = ".",
    file_extension = "jpg"
) {
  sprintf(
    'k%02d%sv%04d%sb%04d.%s',
    k_id, sep, v_id, sep, b_id, file_extension
  )
}

#' Title
#'
#' @param db_dir_base
#' @param db_dir_path
#' @param pattern
#' @param start
#' @param end
#'
#' @returns
#' @export
#'
#' @examples
get_seq_start <- function(
    db_dir_base,
    db_dir_path,
    pattern = "*.jpg",
    start = 12,
    end = 15
) {

  get_path <- file.path(db_dir_base, db_dir_path)

  if (dir.exists(get_path)) {

    x <- list.files(
      path = get_path,
      pattern = pattern
    )

    stringr::str_sub(x, start = start, end = end) |>
      readr::parse_number() |>
      max() + 1

  } else {
    cat("Specified directory does not exist!", "\n")
  }
}

# db_dir_base <- "E:/NRM/NA/PAN/PAN_Delad/DataBase/Arkivdok"
# k_id <- c(3, 3, 1)
# v_id <- c(274, 253, 271)
# b_id <- c(1, 1, 3)
#
# get_dir_path(k_id, v_id)
# set_file_name(k_id, v_id, b_id)
# set_file_name(k_id, v_id, b_id, sep = "_", file_extension = "tif")
#
# f <- file.path(
#   db_dir_base,
#   get_dir_path(k_id, v_id),
#   set_file_name(k_id, v_id, b_id)
# )
#
# f
# file.exists(f)
#
# map_int(
#   get_dir_path(k_id, v_id),
#   \(x) get_seq_start(db_dir_base, x)
# )

#' Title
#'
#' @param input_dir
#' @param db_dir_base
#' @param k_id
#' @param v_id
#' @param seq_start
#' @param overwrite
#' @param execute
#' @param sep
#' @param file_extension
#'
#' @returns
#' @export
#'
#' @examples
db_copy_new_files <- function(
    input_dir, db_dir_base,
    k_id, v_id,
    seq_start,
    overwrite = FALSE,
    execute = TRUE,
    sep = ".",
    file_extension = ".jpg"
) {

  # List files in input directory
  x <- list.files(path = input_dir, pattern = pattern, full.names = TRUE)
  # Generate sequence for new file names
  b_id <- seq(seq_start, by = 1, length.out = length(x))
  # Create new file names
  x_new <- set_file_name(
    k_id = k_id, v_id = v_id, b_id = b_id,
    sep = sep, file_extension = file_extension
  )
  # Rename existing files in input directory
  x_new <- file.path(input_dir, x_new)

  if (execute) {
    file.rename(x, x_new)
  }

  # Create output directory
  db_dir <- get_dir_path(k_id = k_id, v_id = v_id)
  db_dir_path <- file.path(db_dir_base, db_dir)

  if (execute) {
    if (!dir.exists(db_dir_path)) {
      dir.create(db_dir_path)
    }
    if (dir.exists(db_dir_path)) {
      file.copy(
        x_new,
        file.path(db_dir_path, basename(x_new)),
        overwrite = overwrite,
        copy.mode = TRUE,
        copy.date = TRUE
      )
    }
  }

  invisible(
    list(
      x = x,
      x_new = x_new,
      db_dir = db_dir,
      db_dir_path = db_dir_path
    )
  )
}

#' Title
#'
#' @param k_id
#' @param v_id
#' @param seq_start
#' @param observer_id
#' @param observer_text
#' @param data_type_id
#' @param document_type_id
#' @param time_text
#' @param dsn
#' @param db_dir_base
#' @param pattern
#' @param execute
#'
#' @returns
#' @export
#'
#' @examples
db_insert_archive <- function(
    k_id, v_id,
    seq_start = 1,
    observer_id,
    observer_text,
    data_type_id,
    document_type_id,
    time_text,
    dsn = "Havsorn_Data",
    db_dir_base,
    pattern = ".jpg",
    execute = FALSE) {

  # List existing files
  x <- tibble::tibble(
    Filnamn =
      list.files(
        file.path(db_dir_base, get_dir_path(k_id, v_id)),
        pattern = pattern)
  )

  # Construct data
  sql_insert_data <- x |>
    dplyr::mutate(
      VolymID       = v_id,
      Lopnr         = (seq_start - 1) + dplyr::row_number(),
      KontaktID     = observer_id,
      KontaktText   = observer_text,
      TypDataID     = data_type_id,
      TypHandlingID = document_type_id,
      TidsPeriod    = time_text
    ) |>
    dplyr::select(
      VolymID, Lopnr, Filnamn, KontaktID, KontaktText,
      TypDataID, TypHandlingID, TidsPeriod
    )

  # Check if data already exists - only add non-existing file names!
  con <- RODBC::odbcConnect(dsn = dsn)

  fls_exist <- RODBC::sqlQuery(
    con,
    stringr::str_c(
      "SELECT * FROM tArkivDok WHERE Filnamn IN (",
      stringr::str_c(
        stringr::str_c("'", dplyr::pull(sql_insert_data, Filnamn), "'"),
        collapse = ", "
      )
      , ")"
    )
  )

  sql_insert_data <- sql_insert_data |>
    tibble::as_tibble() |>
    dplyr::anti_join(
      fls_exist, dplyr::join_by(Filnamn)
    )
  if (execute) {

    sql_insert_str <-
      "INSERT INTO tArkivDok (
      VolymID, Lopnr, Filnamn, KontaktID, KontaktText,
      TypDataID, TypHandlingID, TidsPeriod)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?)"

    RODBCext::sqlExecute(
      con,
      sql_insert_str,
      data = as.list(sql_insert_data)
    )

    # Check if data were inserted to db
    # sql_str_chk <- paste0("SELECT * FROM tArkivDok WHERE VolymID = ", v_id)
    # tmp <- RODBCext::sqlExecute(con_rodbc, sql_str_chk, fetch = TRUE, as.is = TRUE) |>
    #  tibble::as_tibble() |>
    #  dplyr::select(VolymID, Lopnr, Filnamn, KontaktText, TypDataID, TypHandlingID, TidsPeriod)

  } else {
    return(sql_insert_data)
  }

  RODBC::odbcClose(con)
}

# Create bat-file function
# Behöver ändra teckenkodning i bat-filen, om filnamnet innehåller "special characters" som åäöÅÄÖ
# https://stackoverflow.com/questions/43046559/why-are-danish-characters-not-displayed-as-in-text-editor-on-executing-batch-fil
# I exemplet ovan använder jag ISO-8859-1
# https://docs.microsoft.com/en-us/windows/win32/intl/code-page-identifiers

# Q: Is it possible to use relative links with IrfanView thumbnails?
# This example uses absolute links
# But it appears that relative paths can work,
# if they are expressed relative to i_view64.exe,
# which can be moved. Put the exe-file in the picture maps!

#' Title
#'
#' @param files
#' @param output_file
#' @param output_dir
#' @param sub_dir
#' @param relative
#' @param file_encoding
#' @param i_view_path
#'
#' @returns
#' @export
#'
#' @examples
thumbs_bat <- function(
    files,
    output_file,
    output_dir = getwd(),
    sub_dir = TRUE,
    relative = FALSE,
    file_encoding = "latin1",
    i_view_path = "C:/Program Files/IrfanView/i_view64.exe") {

  # Write file list
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }

  if (sub_dir) {
    filelist_file <- file.path(
      output_dir, "filelist", stringr::str_c(output_file, ".txt")
    )
    chk_dir <- file.path(output_dir, "filelist")
    if (!dir.exists(chk_dir)) {
      dir.create(chk_dir)
    }
  } else {
    filelist_file <- file.path(
      output_dir, stringr::str_c(output_file, ".txt")
    )
  }

  # Write contents of filelist
  write.table(
    files,
    filelist_file,
    row.names = FALSE,
    col.names = FALSE,
    fileEncoding = file_encoding
  )

  if (relative) {
    filelist_file <- stringr::str_replace(
      filelist_file, stringr::str_c(output_dir, "/"), ""
    )
    i_view_path <- basename(i_view_path)
  }

  bat_content <- stringr::str_c(
    '"', i_view_path, '" /thumbs /filelist="', filelist_file, '"'
  )

  bat_file <- file.path(
    output_dir, stringr::str_c(output_file, ".bat")
  )

  # Connect/create bat-file
  file_conn <- file(bat_file)
  # Write data to bat_file
  writeLines(c("chcp 28591", bat_content), file_conn)
  close(file_conn)
}

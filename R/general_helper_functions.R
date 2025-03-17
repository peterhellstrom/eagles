# https://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x
# https://stackoverflow.com/questions/43627679/round-any-equivalent-for-dplyr/46489816#46489816

#' Title
#'
#' @param .x
#' @param .accuracy
#' @param f
#'
#' @returns
#' @export
#'
#' @examples
round_any <- function(.x, .accuracy, f = round) {
  f(.x / .accuracy) * .accuracy
}

#' Round a numeric value upwards
#'
#' This functions rounds a numeric value upwards
#' @param .x Numeric value
#' @returns Numeric
#' @export
round_up <- function(.x) {
  out <- 10^ceiling(log10(.x))
  return(out)
}

#' Title
#'
#' @param x
#' @param round_to
#' @param dir
#'
#' @returns
#' @export
#'
#' @examples
round_choose <- function(x, round_to, dir = c("up", "down")) {
  if(dir == "up") {
    x + (round_to - x %% round_to)
  } else {
    if(dir == "down") {
      x - (x %% round_to)
    }
  }
}

#' Title
#'
#' @param vec
#' @param fun
#'
#' @returns
#' @export
#'
#' @examples
my_min_max <- function(vec, fun = max) {
  ifelse(length(vec[!is.na(vec)]) == 0, NA_real_, fun(vec, na.rm = TRUE))
}
# my_min_max(c(NA, NA))
# my_min_max(c(1, NA, NA, 3), max)
# my_min_max(c(1, NA, NA, 3), min)


#' Title
#'
#' @param .x
#'
#' @returns
#' @export
#'
#' @examples
get_digits <- function(.x) {
  floor(log10(.x)) + 1
}

# get_member can be replaced by stringr::str_split_i()

#' Title
#'
#' @param .x
#' @param sep
#' @param pos
#'
#' @returns
#' @export
#'
#' @examples
get_member <- function(.x, sep = ",", pos = 1) {
  # sapply(strsplit(.x, sep), '[', pos)
}

# chec fs::fs_bytes() as an alternative!
# # https://stackoverflow.com/questions/63543853/how-can-we-get-the-formated-file-size-in-kb-mb-gb-tb-in-r
# https://stackoverflow.com/questions/10910688/converting-kilobytes-megabytes-etc-to-bytes-in-r

#' Title
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
convert_bytes <- function(x) {
  ptn <- "(\\d*(.\\d+)*)(.*)"
  num  <- as.numeric(sub(ptn, "\\1", x))
  unit <- sub(ptn, "\\3", x)
  unit[unit == ""] <- "1"

  mult <- c("1" = 1, "K" = 1024, "M" = 1024^2, "G" = 1024^3)
  num * unname(mult[unit])
}

#' Title
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
asc <- function(x) {
  strtoi(charToRaw(x), 16L)
}

# Main difference with base::cut is that this function
# generates the desired labels
# Uses cut to generate bins, must use option right = FALSE!
# Q: Does this work as intended if the vector x
# has gaps (like missing one or two "years" in a sequence)?

#' Title
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
lbl_breaks <- function(x) {
  paste(x[-length(x)], x[-1] - 1, sep = "-")
}

#' Title
#'
#' @param x
#' @param breaks
#' @param by
#'
#' @returns
#' @export
#'
#' @examples
cut2 <- function(x, breaks = NULL, by = 5) {

  if (is.null(breaks)) {
    breaks <- seq(from = min(x), to = max(x), by = by)
  }

  base::cut(
    x = x, breaks = breaks, labels = lbl_breaks(breaks),
    right = FALSE
  )

  # Start developing another approach, usinge the unexposed breaks function
  # br <- ggplot2:::breaks(x, "width", nbins = NULL, binwidth = by)
  # y <- cut(x, br, include.lowest = TRUE, right = FALSE)
  # lbls <- str_c(head(br, -1), "-", na.omit(lead(br) - 1))

}

# x <- 1961:2023
# cut(x, seq(from = 1961, to = 2026, by = 5), right = FALSE)
# cut_interval(x, length = 5, right = FALSE)
# cut2(x, seq(from = 1961, to = 2026, by = 5))

# add_timestamp <- function(.x, sep = "_") {
#   str_c(.x, format(Sys.time(), "%Y%m%d_%H%M%S"), sep = sep)
# }

# add_timestamp mimics utils::timestamp
# utils::timestamp(stamp = format(Sys.time(), "%Y%m%d_%H%M%S"),
# prefix = "", suffix = "", quiet = TRUE)

#' Title
#'
#' @param .x
#' @param suffix
#' @param sep
#'
#' @returns
#' @export
#'
#' @examples
add_timestamp <- function(.x, suffix = NULL, sep = "_") {
  out <- stringr::str_c(
    .x,
    format(Sys.time(), glue::glue("%Y%m%d{sep}%H%M%S")),
    sep = sep
  )
  if (!is.null(suffix)) {
    out <- stringr::str_c(out, suffix)
  }
  out
}

#' Title
#'
#' @param .out
#' @param .data
#'
#' @returns
#' @export
#'
#' @examples
sink_cat <- function(.out, .data) {
  sink(.out)
  cat(.data)
  sink()
}

# Hjälpfunktion för att testa om ett värde finns i en vektor

#' Title
#'
#' @param x
#' @param match_value
#'
#' @returns
#' @export
#'
#' @examples
has_field_name <- function(x, match_value) {
  !all(is.na(match(x, match_value)))
}
#has_field_name(LETTERS, "W") # returnerar TRUE
#has_field_name(LETTERS[1:5], "W") # returnerar FALSE

#' Title
#'
#' @param i
#'
#' @returns
#' @export
#'
#' @examples
extend <- function(alphabet) function(i) {
  base10toA <- function(n, A) {
    stopifnot(n >= 0L)
    N <- length(A)
    j <- n %/% N
    if (j == 0L) A[n + 1L] else paste0(Recall(j - 1L, A), A[n %% N + 1L])
  }
  vapply(i-1L, base10toA, character(1L), alphabet)
}

#' Title
#'
#' @param .files
#' @param .columns
#'
#' @returns
#' @export
#'
#' @examples
pdf_page_count <- function(.files, .columns = c("pages", "created")) {
  purrr::map_dfr(
    .files,
    \(x) pdftools::pdf_info(x)[.columns],
    .id = "input_file")
}


# Konvertera logiska fält från text [Ja/Nej] till TRUE/FALSE

#' Title
#'
#' @param .x
#' @param cols
#' @param yes_value
#' @param no_value
#'
#' @returns
#' @export
#'
#' @examples
convert_to_logical <- function(.x, cols, yes_value = "Ja", no_value = "Nej") {
  .x |>
    dplyr::mutate(
      dplyr::across( {{ cols }} , \(x) dplyr::case_when(
        x %in% yes_value ~ TRUE,
        x %in% no_value ~ FALSE,
        TRUE ~ NA)
      )
    ) |>
    dplyr::mutate(
      dplyr::across( {{ cols }} , as.logical)
    )
}

# Ersätt specificerat värde med kolumnnamnet för variabeln

# Tänkt användning: ersätt "Ja" med kolumnnamnet och sätt Nej till NA
# Två varianter av funktionen, den första använder ifelse och där finns
# även möjligheten att ange vilka värden som motsvarar Ja och Nej

#' Title
#'
#' @param .x
#' @param cols
#' @param yes_value
#' @param no_value
#'
#' @returns
#' @export
#'
#' @examples
colname_to_value <- function(.x, cols, yes_value = TRUE, no_value = NA) {
  .x |>
    dplyr::mutate(
      dplyr::across( {{ cols }} , as.character),
      dplyr::across(
        {{ cols }},
        \(x) dplyr::if_else(
          x %in% yes_value, dplyr::cur_column(), no_value
        )
      )
    )
}

# x <- tribble(
#   ~x, ~y,
#   "yes", TRUE,
#   "no", FALSE
# )
#
# colname_to_value(x, x:y, yes_value = c("yes", TRUE), no_value = NA)
# convert_to_logical(x, x, yes_value = "yes", no_value = "no")


# den andra varianten använder case_when och förutsätter att datavärden
# i kolumnerna som anges med argumentet cols är TRUE/FALSE (dvs. logical).

#' Title
#'
#' @param .x
#' @param cols
#'
#' @returns
#' @export
#'
#' @examples
colname_to_value2 <- function(.x, cols) {
  cols <- enquo(cols)
  .x %>%
    dplyr::mutate(
      dplyr::across(
        !!cols,
        ~ dplyr::case_when
        (. == TRUE ~ dplyr::cur_column(),
          TRUE ~ NA_character_)
      )
    ) %>%
    dplyr::mutate(
      dplyr::across(!!cols, ~ as.character(.))
    )
}

#' Title
#'
#' @param .x
#' @param .data
#' @param .vars
#' @param .fn
#'
#' @returns
#' @export
#'
#' @examples
sum_by_grp <- function(.x, .data, .vars, .fn = sum) {
  .x %>%
    purrr::map(
      ~ .data %>%
        dplyr::group_by_at(.x) %>%
        dplyr::summarize_at(vars({{.vars}}), .fn, na.rm = TRUE)
    ) %>%
    dplyr::bind_rows()
}

#' Title
#'
#' @param .x
#' @param .var
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
add_wt <- function(.x, .var, ...) {
  .x |>
    dplyr::select( {{.var }}, ...) |>
    dplyr::distinct() |>
    dplyr::add_count( {{.var}} ) |>
    dplyr::mutate(wt = 1/n) |>
    dplyr::arrange( {{.var}} )
}

#' Title
#'
#' @param wd
#' @param folder
#' @param files
#' @param files_url
#' @param file_type
#' @param files_unzip
#' @param delete_zip
#' @param junkpaths
#' @param overwrite
#' @param add_date
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
download_zip <- function(
    wd, folder,
    files, files_url,
    file_type = "zip", files_unzip = TRUE,
    delete_zip = TRUE,
    junkpaths = TRUE, overwrite = TRUE,
    add_date = FALSE, ...) {

  # Create path to a directory
  if (add_date) {
    dl_wd <- paste(wd, folder, "_", Sys.Date(), sep = "")
  } else {
    dl_wd <- paste(wd, folder, sep = "")
  }

  # Check if directory exists, create if not!
  if (file.exists(dl_wd) == FALSE) {
    dir.create(dl_wd)
  }

  cat(paste("File(s) will be downloaded to ", dl_wd), "\n")

  # Download files
  dl_files <- paste(dl_wd, "/", files, ".", file_type, sep = "")

  for (i in 1:length(files)) {
    cat(paste("Downloading ", files[i], ".", file_type, sep = ""), "\n")
    try(download.file(url = files_url[i], destfile = dl_files[i], quiet = TRUE, ...))
  }

  # create list of all zip-files in the working directory
  files_zip <- list.files(
    dl_wd,
    pattern = paste(".", file_type, sep = ""),
    full.names = TRUE
  )

  if (files_unzip) {
    # unzip each downloaded archive
    for (i in 1:length(files_zip)) {
      try(unzip(
        files_zip[i],
        junkpaths = junkpaths,
        overwrite = overwrite,
        exdir = dl_wd)
      )
    }
  }

  if (delete_zip == TRUE) {
    file.remove(files_zip)
  }
}

#' Title
#'
#' @param url
#'
#' @returns
#' @export
#'
#' @examples
url_file_exist <- function(url){
  HTTP_STATUS_OK <- 200
  hd <- httr::HEAD(url)
  status <- hd$all_headers[[1]]$status
  list(
    exists = status == HTTP_STATUS_OK,
    status = status
  )
}

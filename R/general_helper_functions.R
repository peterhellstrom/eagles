# https://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x
# https://stackoverflow.com/questions/43627679/round-any-equivalent-for-dplyr/46489816#46489816

#' @export
round_any <- function(.x, .accuracy, f = round) {
  f(.x / .accuracy) * .accuracy
}

#' Round a numeric value upwards
#'
#' This functions rounds a numeric value upwards
#' @param .x Numeric value
#' @return Numeric
#' @export
round_up <- function(.x) {
  out <- 10^ceiling(log10(.x))
  return(out)
}

#' @export
round_choose <- function(x, round_to, dir = c("up", "down")) {
  if(dir == "up") {
    x + (round_to - x %% round_to)
  } else {
    if(dir == "down") {
      x - (x %% round_to)
    }
  }
}

#' @export
my_min_max <- function(vec, fun = max) {
  ifelse(length(vec[!is.na(vec)]) == 0, NA_real_, fun(vec, na.rm = TRUE))
}
# my_min_max(c(NA, NA))
# my_min_max(c(1, NA, NA, 3), max)
# my_min_max(c(1, NA, NA, 3), min)

#' @export
get_digits <- function(.x) {
  floor(log10(.x)) + 1
}

#' @export
get_member <- function(.x, sep = ",", pos = 1) {
  sapply(strsplit(.x, sep), '[', pos)
}

#' @export
convert_bytes <- function(x) {
  ptn <- "(\\d*(.\\d+)*)(.*)"
  num  <- as.numeric(sub(ptn, "\\1", x))
  unit <- sub(ptn, "\\3", x)
  unit[unit == ""] <- "1"

  mult <- c("1" = 1, "K" = 1024, "M" = 1024^2, "G" = 1024^3)
  num * unname(mult[unit])
}

#' @export
asc <- function(x) {
  strtoi(charToRaw(x), 16L)
}

# Main difference with base::cut is that this function
# generates the desired labels
# Uses cut to generate bins, must use option right = FALSE!
# Q: Does this work as intended if the vector x
# has gaps (like missing one or two "years" in a sequence)?
#' @export
lbl_breaks <- function(x) paste(x[-length(x)], x[-1] - 1, sep = "-")

#' @export
cut2 <- function(x, breaks = NULL, by = 5) {

  if (is.null(breaks)) {
    breaks <- seq(from = min(x), to = max(x), by = by)
  }

  base::cut(x = x, breaks = breaks, labels = lbl_breaks(breaks),
            right = FALSE)

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

#' @export
add_timestamp <- function(.x, sep = "_") {
  stringr::str_c(.x, format(Sys.time(), glue::glue("%Y%m%d{sep}%H%M%S")), sep = sep)
}

#' @export
sink_cat <- function(.out, .data) {
  sink(.out)
  cat(.data)
  sink()
}

# Hjälpfunktion för att testa om ett värde finns i en vektor
#' @export
has_field_name <- function(x, match_value) {
  !all(is.na(match(x, match_value)))
}
#has_field_name(LETTERS, "W") # returnerar TRUE
#has_field_name(LETTERS[1:5], "W") # returnerar FALSE

#' @export
extend <- function(alphabet) function(i) {
  base10toA <- function(n, A) {
    stopifnot(n >= 0L)
    N <- length(A)
    j <- n %/% N
    if (j == 0L) A[n + 1L] else paste0(Recall(j - 1L, A), A[n %% N + 1L])
  }
  vapply(i-1L, base10toA, character(1L), alphabet)
}

#' @export
pdf_page_count <- function(.files, .columns = c("pages", "created")) {
  map_dfr(
    .files,
    ~ pdftools::pdf_info(.x)[.columns],
    .id = "input_file")
}


# Konvertera logiska fält från text [Ja/Nej] till TRUE/FALSE ----
#' @export
convert_to_logical <- function(.x, cols, yes_value = "Ja", no_value = "Nej") {
  cols <- enquo(cols)
  .x %>%
    mutate(across(!!cols, ~ case_when(
      . == yes_value ~ TRUE,
      . == no_value ~ FALSE,
      TRUE ~ NA))) %>%
    mutate(across(!!cols, ~ as.logical(.)))
}

# Ersätt specificerat värde med kolumnnamnet för variabeln ----

# Tänkt användning: ersätt "Ja" med kolumnnamnet och sätt Nej till NA
# Två varianter av funktionen, den första använder ifelse och där finns
# även möjligheten att ange vilka värden som motsvarar Ja och Nej
#' @export
colname_to_value <- function(.x, cols, yes_value = TRUE, no_value = NA) {
  cols <- enquo(cols)
  .x %>%
    dplyr::mutate(dplyr::across(!!cols, function(x) base::ifelse(x == yes_value, dplyr::cur_column(), no_value))) %>%
    dplyr::mutate(dplyr::across(!!cols, ~ as.character(.)))
}
# den andra varianten använder case_when och förutsätter att datavärden
# i kolumnerna som anges meda argumentet cols är TRUE/FALSE (dvs. logical)
# behöver alltså användas tillsammans med funktionen kungsorn logical för att
# fungera som tänkt.
#' @export
colname_to_value2 <- function(.x, cols) {
  cols <- enquo(cols)
  .x %>%
    dplyr::mutate(
      dplyr::across(!!cols,
                    ~ dplyr::case_when
                    (. == TRUE ~ dplyr::cur_column(),
                      TRUE ~ NA_character_))) %>%
    dplyr::mutate(dplyr::across(!!cols, ~ as.character(.)))
}

## Summera grupp ----
#' @export
sum_by_grp <- function(.x, .data, .vars, .fn = sum) {
  .x %>%
    map(~ .data %>%
          group_by_at(.x) %>%
          summarize_at(vars({{.vars}}), .fn, na.rm = TRUE)) %>%
    bind_rows()
}

#' @export
add_wt <- function(.x, .var, ...) {
  {{.x}} %>%
    select({{.var}}, ...) %>%
    distinct() %>%
    add_count({{.var}}) %>%
    mutate(wt = 1/n) %>%
    arrange({{.var}})
}

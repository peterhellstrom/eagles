# USEFUL THREADS on how-to calculate totals and subtotals
# https://stackoverflow.com/questions/31164350/dplyr-summarize-with-subtotals
# A new function dplyr-based in development stage (2020-12-02) found here, could be useful
# https://github.com/jrf1111/TCCD/blob/dev/R/with_subtotals.R
# https://jozef.io/r912-datatable-grouping-sets/
# https://cran.r-project.org/web/packages/janitor/vignettes/tabyls.html

# To do:
# 1) Alternative to rowSums in wtse_vars
# 2) OBS! Antal ungar i beräkningen i wtse_vars inkluderar inte >0

# To investigate:
# 1) Do calculations based on spatial joins, e.g. landskap, rapportområden?
# 2) Summarize at a higher level, i.e. also include ej inventerat? borttappade?
# 3) Adjust function to allow individually for step transform -> cube -> sum within wtse_sum?
# 4) NOTE: code fails if any of the grouping variables contains NA? Check this further
#    e.g. Region, Delområde for BD 2020

# WORKFLOW
# 1) Transform data (wtse_transform) from long to wide format
# 2) Calculate totals and sub-totals for all hierarchical levels
#    (use cube function in data.table)
# 3) Calculate derived variables (wtse_vars)
# wtse_sum() is a wrapper function that performs all steps above.

# Create an empty tibble with the necessary columns
# in the correct order.
# This is necessary as all "columns" might not exist in
# the analyzed data set, i.e. some breeding outcomes might
# not exist in the selected data sets, which would cause
# the function wtse_vars() to fail. The empty tibble
# is used with bind_columns together with the tibble containing the actual data.


#' Title
#'
#' @param dsn
#' @param table
#' @param column
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
wtse_columns <- function (
    dsn = "Havsorn",
    table = "luOvervakningUtfall",
    column = "OvervakningUtfallR",
    ...) {

  con <- DBI::dbConnect(odbc::odbc(), dsn, ...)

  dplyr::tbl(con, table) |>
    dplyr::arrange(SortOrder) |>
    dplyr::pull(column) |>
    purrr::map_dfr(
      \(x) tibble::tibble( {{ x }} := logical())
    ) |>
    dplyr::mutate(
      dplyr::across(tidyselect::everything(), as.integer)
    )
}

# Make this function more general!
# use the the right_join + complete method to ensure that all (factor) levels
# of UtfallR (or other response variable) actually are included in the data


#' Title
#'
#' @param .data
#' @param ...
#' @param .response
#' @param .columns
#' @param .drop
#'
#' @return
#' @export
#'
#' @examples
wtse_transform <- function(
    .data,
    ...,
    .response = UtfallR,
    .columns = NULL,
    .drop = FALSE) {

  # ... are the user-specified grouping variables,
  # e.g. Region, CensusYear
  # UtfallR is a column in the data set
  # two options, to use count or tally
  # tally might be a better option to generate counts for groups

  if (missing(.columns)) {
    .columns <- wtse_columns()
  }

  .data_pivot <- .data |>
    dplyr::count(..., {{ .response }}, .drop = .drop) |>
    tidyr::pivot_wider(
      values_from = n,
      names_from = UtfallR
    )

  .columns |>
    dplyr::bind_rows(.data_pivot) |>
    dplyr::relocate(...) |>
    {\(.) {base::replace(., base::is.na(.), 0)}}()
}

# OBS! Variabeln "Kända revir" bör läggas till här!
# Två varianter: Räkna årsposter, eller från lokal-tabellens Första år
# OBS! Även ej inventerade revir bör inkluderas


#' Title
#'
#' @param .data
#' @param clean_names
#'
#' @return
#' @export
#'
#' @examples
wtse_vars <- function(
    .data,
    clean_names = FALSE
) {
  .x <- .data |>
    dplyr::mutate(
      inventerade_revir     = rowSums(dplyr::across(ej_h:annan_art)),
      besatta_revir         = rowSums(dplyr::across(ej_h:ad_i_revir)),
      ej_besatta_revir      = rowSums(dplyr::across(observation:annan_art)),
      antal_par             = rowSums(dplyr::across(ej_h:par_ej_hack)),
      bebott_bo             = rowSums(dplyr::across(ej_h:h_okant_res)),
      produktiva_par        = rowSums(dplyr::across(lh_1:lh_min_3)),
      improduktiva_par      = rowSums(dplyr::across(ej_h:missl)),
      n_utfall              = produktiva_par + improduktiva_par,
      andel_produktiva_par  = produktiva_par / (produktiva_par + improduktiva_par),
      n_kullstorlek         = lh_1 + lh_2 + lh_3,
      kullstorlek           = (lh_1 + 2*lh_2 + 3*lh_3) / n_kullstorlek,
      produktivitet         = andel_produktiva_par * kullstorlek,
      antal_ungar           = lh_1 + 2*lh_2 + 3*lh_3 + lh_min_0 + lh_min_1 + 2*lh_min_2 + 3*lh_min_3
    )

  if (clean_names) {
    .x <- .x |>
      janitor::clean_names()
  }

  .x
}

# Sum yearly monitoring posts per site
# This function was really slow in R 4.3.0!
# I had re-written the function based on dplyr::case_when statements.
# But it was confirmed that code is much faster in R 4.1.3. It is the case_when
# statements that causes trouble.

#' Title
#'
#' @param .data
#' @param .by_column
#'
#' @return
#' @export
#'
#' @examples
monitoring_summary <- function(
    .data,
    .by_column = LokalID
) {
  .data |>
    # dplyr::filter(CensusYear >= year(now()) - 6) |>
    dplyr::summarize(
      n_year_posts = dplyr::n(),
      missing_status = sum(is.na(OvervakningUtfallID), na.rm = TRUE),
      n_surveyed = sum(OvervakningUtfallID > 0, na.rm = TRUE),
      n_not_surveyed = sum(OvervakningUtfallID < 0, na.rm = TRUE),
      n_occupied = sum(OvervakningUtfallID > 0 & OvervakningUtfallID <= 44, na.rm = TRUE),
      n_not_occupied = sum(OvervakningUtfallID >= 51, na.rm = TRUE),
      n_occupied_nest = sum(OvervakningUtfallID > 0 & OvervakningUtfallID <= 41, na.rm = TRUE),
      n_productive = sum(OvervakningUtfallID >= 21 & OvervakningUtfallID <= 34, na.rm = TRUE),
      first_survey = my_min_max(CensusYear[OvervakningUtfallID > 0 & !is.na(OvervakningUtfallID)], fun = min),
      last_survey = my_min_max(CensusYear[OvervakningUtfallID > 0 & !is.na(OvervakningUtfallID)], fun = max),
      last_occupied = my_min_max(CensusYear[OvervakningUtfallID > 0 & OvervakningUtfallID <= 44 & !is.na(OvervakningUtfallID)], fun = max),
      last_occupied_nest = my_min_max(CensusYear[OvervakningUtfallID > 0 & OvervakningUtfallID <= 41 & !is.na(OvervakningUtfallID)], fun = max),
      last_productive = my_min_max(CensusYear[OvervakningUtfallID >= 21 & OvervakningUtfallID <= 34 & !is.na(OvervakningUtfallID)], fun = max),
      # first_survey = dplyr::first(CensusYear[OvervakningUtfallID > 0 & !is.na(OvervakningUtfallID)], na_rm = TRUE),
      # last_survey = dplyr::last(CensusYear[OvervakningUtfallID > 0 & !is.na(OvervakningUtfallID)], na_rm = TRUE),
      # last_occupied = dplyr::last(CensusYear[OvervakningUtfallID > 0 & OvervakningUtfallID <= 44 & !is.na(OvervakningUtfallID)], na_rm = TRUE),
      # last_occupied_nest = dplyr::last(CensusYear[OvervakningUtfallID > 0 & OvervakningUtfallID <= 41 & !is.na(OvervakningUtfallID)], na_rm = TRUE),
      # last_productive = dplyr::last(CensusYear[OvervakningUtfallID >= 21 & OvervakningUtfallID <= 34 & !is.na(OvervakningUtfallID)], na_rm = TRUE),
      .by = {{ .by_column }}
    )
}

#' Title
#'
#' @param .data
#' @param ...
#' @param .columns
#' @param .remove
#'
#' @return
#' @export
#'
#' @examples
wtse_sum <- function(
    .data,
    ...,
    .columns = NULL,
    .remove = FALSE
) {

  if (missing(.columns)) {
    .columns <- wtse_columns()
  }

  .x <- .data |>
    wtse_transform(
      ...,
      .columns = .columns
    )

  .x <- .x |>
    data.table::data.table() |>
    data.table::cube(
      base::lapply(.SD, sum),
      by = base::sapply(
        dplyr::vars(...), rlang::quo_text
      )
    ) |>
    tibble::as_tibble() |>
    wtse_vars()

  .x <- .x |>
    dplyr::select(
      ...,
      `inventerade_revir`:`antal_ungar`,
      `ej_h`:`uppgift_saknas`
    )

  if (.remove) {
    .x <- .x |>
      select(
        -c(`ej_h`:`uppgift_saknas`)
      )
  }

  .x
}

# Brief test examples (NOT RUN):
#
# x <- tibble(
#   Region = c("AB", "AB", "AB", "AB", "C", "C", "C", "C"),
#   UtfallR = c("ej_h", "lh_1", "lh_2", "ej_h", "ad_i_revir", "h_okant_res", "missl", "lh_2"),
#   CensusYear = 2021
# )
#
# .columns <- wtse_columns()
#
# x$UtfallR %in% names(.columns)
#
# x %>%
#   wtse_transform(Region, CensusYear)
#
# x %>%
#   wtse_transform(Region, CensusYear) %>%
#   wtse_vars()
#
# x %>%
#   wtse_sum(Region, CensusYear)
#
# x %>%
#   wtse_sum(Region, CensusYear, .remove = TRUE)
#
# # Does NOT work with grouped data - check why.
# x %>%
#   group_by(Region, CensusYear) %>%
#   wtse_sum()
#
# # Example
#
# # 1) Pivot
# x_t <- x |>
#   count(Region, CensusYear, UtfallR) |>
#   pivot_wider(names_from = UtfallR, values_from = n)
#
# 2) "Fill" with missing columns
# Bind rows using empty tibble to include all columns
# .columns |>
#   bind_rows(x_t) |>
#   relocate(Region, CensusYear) |>
#   replace(., is.na(.), 0)
#
# Alternative, Use factor levels and complete() to include all columns
# x |>
#   mutate(UtfallR = factor(UtfallR, levels = names(.columns))) |>
#   count(Region, CensusYear, UtfallR) |>
#   group_by(Region, CensusYear) |>
#   complete(UtfallR = UtfallR) |>
#   pivot_wider(names_from = UtfallR, values_from = n) |>
#   replace(., is.na(.), 0)
#
#
# snakecase::to_any_case(names(.columns))

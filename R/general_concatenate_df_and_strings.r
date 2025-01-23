#' Title
#'
#' @param .x
#' @param sep
#' @param .field
#' @param .prefix
#' @param remove
#'
#' @return
#' @export
#'
#' @examples
df_c <- function(
    .x,
    sep = "|",
    .field = data,
    .prefix = NULL,
    remove = TRUE
) {

  if (!is.null(.prefix)) {
    .prefix <- stringr::str_c(.prefix, sep)
  }

  .x |>
    dplyr::mutate(
      # dplyr::across(
      #   tidyselect::everything(), \(x) dplyr::na_if(x, "")
      # ),
      dplyr::across(
        dplyr::where(is.character), \(x) dplyr::na_if(x, "")
      ),
      dplyr::across(
        tidyselect::everything(), \(x) tidyr::replace_na(as.character(x), "")
      )
    ) |>
    tidyr::unite(
      {{ .field }}, tidyselect::everything(), sep = sep, remove = remove
    ) |>
    dplyr::mutate(
      {{ .field }} := stringr::str_c(.prefix, {{ .field }}, sep)
    )
}

# Sammanfoga strängar, endast unika värden ----
# sort may not be good...mis-aligns items of columns are non-independent!
# str_c propagates missing values, so any occurence of a missing value returns
# another missing value.
# This is not desired here, so we remove all NAs!
# But what if length of string is 0? Must return NA, not empty string ""

# ToDo: use stringr::str_unique() in this function

#' Title
#'
#' @param .x
#' @param collapse
#' @param sort
#' @param unique
#' @param na.rm
#'
#' @return
#' @export
#'
#' @examples
str_c_unique <- function(
    .x, collapse = "; ", sort = FALSE,
    unique = TRUE, na.rm = TRUE) {

  if (sort) .x <- sort(.x)
  if (unique) .x <- unique(.x)
  if (na.rm) .x <- .x[!is.na(.x)]
  if (all(is.na(.x))) {
    out <- NA
  } else {
    out <- stringr::str_c(.x, collapse = collapse)
  }
  out
}

# Examples:
# .x <- c(NA, 4, 3)
# str_c_unique(.x, sort = TRUE)
# str_c_unique(.x, na.rm = FALSE)
# paste0 includes NA, str_c propagates NA
# paste0(.x, collapse = ",")
# str_c(.x, collapse = ",")

# z <- tibble::tibble(
#   grp = rep(letters[1:2], each = 2),
#   val = c(3, NA, 4, 51),
#   val_NA = rep(NA, 4))
#
# z |>
#   dplyr::group_by(grp) |>
#   dplyr::summarize(
#     test = stringr::str_c_unique(val),
#     test_NA = str_c_unique(val_NA))

# But...on the other hand - is a specific function
# really necessary? See alternatives below.
# Better to combine str_c with str_unique and na.omit?
# str_c works best with mutate(), str_flatten is the
# better function to use in combination with summarize()

# z |>
#   dplyr::group_by(grp) |>
#   dplyr::summarize(
#     test = stringr::str_c(val |> na.omit(), collapse = "; "),
#     test_NA = stringr::str_c(val_NA, collapse = "; "))

# x <- tibble::tibble(
#   grp_1 = c("a", "a", "a", "b", "b", "b", "c"),
#   grp_2 = c("x", "x", "y", "z", "z", NA, NA),
#   id = 1:7)
#
# x |>
#   dplyr::group_by(grp_1) |>
#   dplyr::summarize(
#     grp_2_keep = stringr::str_c(grp_2 |> na.omit(), collapse = ", "),
#     grp_2_unique = stringr::str_c(grp_2 |> na.omit() |> stringr::str_unique(), collapse = ", "),
#     id = stringr::str_c(id, collapse = ", ")) |>
#   dplyr::mutate(dplyr::across(tidyselect::everything(), \(x) na_if(x, "")))

#' Title
#'
#' @param .x
#'
#' @returns
#' @export
#'
#' @examples
group_levels <- function(.x) {
  purrr::map(
    base::seq_along(.x), \(i) .x[1:i]
  ) |>
    rlang::set_names(
      base::seq_along(.x)
    ) |>
    base::rev()
}

#' Title
#'
#' @param data
#' @param group_cols
#' @param summarize_cols
#' @param fn
#' @param na_rm
#'
#' @returns
#' @export
#'
#' @examples
group_sum <- function(
    data,
    group_cols,
    summarize_cols,
    fn = sum,
    na_rm = TRUE
) {

  data |>
    dplyr::summarize(
      dplyr::across(
        {{ summarize_cols }},
        \(x) fn(x, na.rm = na_rm)
      ),
      # .by = tidyselect::all_of(group_cols)
      .by = {{ group_cols }}
    )
}

#' Title
#'
#' @param data
#' @param group_sets
#' @param sum_cols
#' @param ...
#' @param names_to
#'
#' @returns
#' @export
#'
#' @examples
group_sum_sets <- function(
    data,
    # sum_levels,
    group_sets,
    sum_cols,
    ...,
    names_to = "group_set"
) {

  purrr::map(
    group_sets,
    \(x) group_sum(
      data,
      # group_cols = tidyselect::all_of( {{ x }} ),
      group_cols = ( {{ x }} ),
      summarize_cols = {{ sum_cols }},
      ...
    )
  ) |>
    purrr::list_rbind(
      names_to = names_to
    ) |>
    dplyr::select(
      all_of(names_to),
      any_of(names(data))
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
  .x |>
    purrr::map(
      \(x) .data |>
        dplyr::group_by_at(x) |>
        dplyr::summarize_at(vars( {{ .vars }} ), .fn, na.rm = TRUE)
    ) |>
    dplyr::bind_rows()
}

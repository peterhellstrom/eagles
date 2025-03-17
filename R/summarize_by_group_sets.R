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
    seq_along(.x), \(i) .x[1:i]
  ) |>
    rlang::set_names(
      seq_along(.x)
    ) |>
    rev()
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
    na_rm = TRUE) {

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
    names_to = "group_set") {

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
    select(
      all_of(names_to),
      any_of(names(data))
    )
}

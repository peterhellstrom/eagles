#' Title
#'
#' @param .x
#'
#' @return
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
#' @return
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
#' @return
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

#' Title
#'
#' @param x
#' @param layer
#' @param .cols
#' @param crs
#' @param .new_cols
#'
#' @return
#' @export
#'
#' @examples
import_gpx <- function(
    x,
    layer = "waypoints",
    .cols = c(name, cmt, geometry),
    crs = 3847,
    .new_cols = c("Easting90", "Northing90")
) {
  sf::read_sf(x, layer = layer) |>
    dplyr::select({{ .cols }}) |>
    sf::st_transform(crs) |>
    eagles::sfc_as_cols(names = .new_cols) |>
    dplyr::relocate(geometry, .after = last_col())
}

#' Title
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
get_nearest <- function(x, y) {

  near_ind <- sf::st_nearest_feature(x, y)

  dplyr::bind_cols(
    x |> st_drop_geometry(),
    y[near_ind,] |> st_drop_geometry(),
    tibble::tibble(
      distance = sf::st_distance(
        x,
        y[near_ind,],
        by_element = TRUE
      )
    )
  ) |>
    dplyr::mutate(
      identical = dplyr::case_when(
        distance < units::as_units(1, "m") ~ TRUE,
        TRUE ~ FALSE
      )
    )
}

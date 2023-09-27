#' Title
#'
#' @param .data
#' @param .grid_size
#' @param .fn
#' @param .prefix
#' @param .suffix
#' @param .keep
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
add_index_1 <- function(
    .data,
    .grid_size,
    .fn,
    .prefix = "grid",
    .suffix = "",
    .keep = "all",
    ...) {

  .data |>
    {(\(.) dplyr::mutate(
      .,
      "{.prefix}_{stringr::str_replace(.grid_size / 1000, ',|\\\\.', '_')}{.suffix}" := .fn(., .grid_size = .grid_size, ...),
      .keep = .keep
    )
    )}()

  # dplyr::mutate(
  #   .data,
  #   "{.prefix}_{stringr::str_replace(.grid_size / 1000, ',|\\\\.', '_')}{.suffix}" := sweref99_index(.data, .grid_size = .grid_size),
  #   .keep = .keep
  # )
}

#' Title
#'
#' @param .data
#' @param .grid_size
#' @param .keep
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
add_index <- function(
    .data,
    .grid_size,
    .keep = "none",
    ...) {

  dplyr::bind_cols(
    .data,
    purrr::map(
      .grid_size,
      \(.x) .data |>
        add_index_1(.x, .keep = .keep, ...)
    ) |>
      purrr::list_cbind()
  )
}

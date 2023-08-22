#' @export
wtse_lkd_grid <- function(
    .x,
    .grid,
    type = c("LINESTRING", "POINT"),
    position = c("centroid", "lower-left")) {

  type <- match.arg(type)
  position <- match.arg(position)

  if (type == "LINESTRING") {

    # Extract index grid position
    .grid_pts <- .grid |>
      sf::st_centroid()

    # Create index variable, which index grid cell is closest to territory?
    # Use this index variable together with slice in next command
    .grid_index <- .x |>
      sf::st_nearest_feature(.grid_pts)

    if (position == "lower-left") {
      .grid_pts <- extract_lower_left_2(.grid)
    }

    # Convert to lines, from actual coordinates to centroid of grid cell
    lkd_grid_lines <- .x |>
      # st_nearest points returns a LINESTRING geometry set
      sf::st_nearest_points(
        grid_pts |>
          dplyr::slice(grid_index), pairwise = TRUE) |>
      sf::st_as_sf() |>
      dplyr::rename(geometry = x)

    # Add attributes & calculate length
    lkd_grid_lines <- lkd_grid_lines |>
      dplyr::bind_cols(
        .x |>
          sf::st_drop_geometry() |>
          tibble::as_tibble() |>
          dplyr::select(Region:Lokalkod)) |>
      dplyr::mutate(length = st_length(.))

    lkd_grid_lines

  } else if (type == "POINT") {
    lkd_grid_pts <- .grid |>
      sf::st_join(.x) |>
      dplyr::filter(!is.na(LokalID))

    if (position == "centroid") {
      lkd_grid_pts <- lkd_grid_pts |>
        # convert from polygon to point
        sf::st_centroid() |>
        dplyr::select(BK, Region:Lokalkod)
    } else if (position == "lower-left") {
      lkd_grid_pts <- lkd_grid_pts |>
        sf::st_cast("POINT") |>
        dplyr::group_by(BK) |>
        dplyr::slice_head() |>
        dplyr::ungroup() |>
        dplyr::select(BK, Region:Lokalkod)
    }

    lkd_grid_pts
  }
}

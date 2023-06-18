#' @export
wtse_lkd_grid <- function(.x, grid_ref,
                          type = c("LINESTRING", "POINT"),
                          position = c("centroid", "lower-left")) {

  type <- match.arg(type)
  position <- match.arg(position)

  if (type == "LINESTRING") {

    # Extract index grid position ----
    grid_ref_pts <- grid_ref %>%
      st_centroid() %>%
      arrange(BK)

    # Create index variable, which index grid cell is closest to territory?
    # Use this index variable together with slice in next command
    grid_index <- .x %>%
      st_nearest_feature(grid_ref_pts)

    if (position == "lower-left") {
      grid_ref_pts <- grid_ref %>%
        st_cast("POINT") %>%
        group_by(BK) %>%
        slice_head() %>%
        ungroup() %>%
        arrange(BK)
    }

    # Convert to lines, from actual coordinates to centroid of grid cell ----
    lkd_grid_lines <- .x %>%
      # st_nearest points returns a LINESTRING geometry set
      st_nearest_points(
        grid_ref_pts %>% slice(grid_index), pairwise = TRUE) %>%
      st_as_sf() %>%
      rename(geometry = x)

    # Add attributes & calculate length
    lkd_grid_lines <- lkd_grid_lines %>%
      bind_cols(
        .x %>%
          st_drop_geometry() %>%
          as_tibble() %>%
          select(Region:Lokalkod)) %>%
      mutate(length = st_length(.))

    lkd_grid_lines

  } else if (type == "POINT") {
    lkd_grid_pts <- grid_ref %>%
      st_join(.x) %>%
      filter(!is.na(LokalID))

    if (position == "centroid") {
      lkd_grid_pts <- lkd_grid_pts %>%
        # convert from polygon to point
        st_centroid() %>%
        select(BK, Region:Lokalkod)
    } else if (position == "lower-left") {
      lkd_grid_pts <- lkd_grid_pts %>%
        st_cast("POINT") %>%
        group_by(BK) %>%
        slice_head() %>%
        ungroup() %>%
        select(BK, Region:Lokalkod)
    }

    lkd_grid_pts
  }
}

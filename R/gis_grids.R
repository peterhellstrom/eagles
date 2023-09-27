# Q: Option to generate row_number and col_number?
# Har något sådant i add_grid_neighbours().

# grid_polygons generates a grid
# requires: sf
#' @export
grid_polygons <- function(
    x_range, y_range,
    delta_x, delta_y,
    crs,
    direction = "clockwise") {

	n_x <- (max(x_range) - min(x_range)) / delta_x
	n_y <- (max(y_range) - min(y_range)) / delta_y

	grid_corners <- base::expand.grid(
		x = base::seq(min(x_range), by = delta_x, length.out = n_x),
		y = base::seq(min(y_range), by = delta_y, length.out = n_y)
	)

	m <- purrr::map2(
	  grid_corners$x,
	  grid_corners$y,
	  \(x,y) {
	    grid_cell(
	      x, y,
	      delta_x = delta_x, delta_y = delta_y,
	      direction = direction)
	  }
	)

	sf::st_sfc(m, crs = crs)
}

# Examples:
# system.time(xy1 <- grid_polygons(c(1200000, 1900000), c(6100000, 7700000), 5000, 5000, 3847))
# system.time(xy2 <- st_make_grid(st_bbox(xy1), n = c(140, 320)))
#
# Different ways of generating a grid with st_make_grid:
# st_bbox(xy1)
# xy21 <- st_make_grid(st_bbox(xy1), n = c(140, 320))
# xy22 <- st_make_grid(st_bbox(xy1), cellsize = 5000)
# xy23 <- st_make_grid(offset = c(1200000, 6100000), n = c(140, 320), cellsize = 5000, crs = 3847)
#
# RT90 2.5 gon V (EPSG:3021)
# g_3021 <- map(c(50000, 25000, 5000), ~ grid_polygons(
#   x_range = c(1200000, 1900000), y_range = c(6100000, 7700000),
#   delta_x = .x, delta_y = .x,
#   epsg = 3847))
#
# lengths(g_3021)

#' @export
grid_cell <- function(
    x_min, y_min,
    delta_x, delta_y,
    direction = c("clockwise", "counter-clockwise")) {

  direction <- base::match.arg(direction)

  x_max <- x_min + delta_x
  y_max <- y_min + delta_y

  if (direction == "clockwise") {
    xy_vec <-
      c(x_min, y_min,
        x_min, y_max,
        x_max, y_max,
        x_max, y_min,
        x_min, y_min)
  } else {
    xy_vec <-
      c(x_min, y_min,
        x_max, y_min,
        x_max, y_max,
        x_min, y_max,
        x_min, y_min)
  }

  sf::st_polygon(
    base::list(
      base::matrix(xy_vec, nrow = 5, ncol = 2, byrow = TRUE)
    )
  )
}
# grid_cell(1300000, 6100000, 50000, 50000, direction = "clockwise")
# grid_cell(1300000, 6100000, 50000, 50000, direction = "counter-clockwise")

#' @export
grid_parms <- function(
    delta_x, delta_y,
    min_x = 200000, max_x = 1000000,
    min_y = 6100000, max_y = 7700000) {

  n_x <- base::ceiling((max_x - min_x) / delta_x)
  x <- min_x + (0:n_x * delta_x)

  n_y <- base::ceiling((max_y - min_y) / delta_y)
  y <- min_y + (0:n_y * delta_y)

  base::list(
    delta_x = delta_x, n_x = n_x, x = x,
    delta_y = delta_y, n_y = n_y, y = y
  )
}

# .x = numeric coordinate in e.g. EPSG:3006 or EPSG:3847, EPSG:3021
# g = grid size in meters, rounds to nearest g
# Is this a "round down"?
#' @export
round_coords <- function(.x, g, centroid = FALSE) {
  .x <- .x - (.x %% g)
  if (centroid) .x <- .x + g / 2
  .x
}

# round_coords(c(734422, 6633780), g = 5000, centroid = FALSE) %>% setNames(c("N", "E"))
# map_dfr(c(5, 10, 25, 50)*1000,
#         ~ round_coords(c(734422, 6633780), g = .x, centroid = FALSE) %>%
#           setNames(c("Northing", "Easting")))

#' @export
st_bbox_round <- function(.x, .size) {
  if (class(.x)[1] == "sf") {
    .x <- sf::st_bbox(.x)
  }
  if (class(.x)[1] == "bbox") {
    .x[1:4] <- c(
      round_choose(.x$xmin, .size, "down"),
      round_choose(.x$ymin, .size, "down"),
      round_choose(.x$xmax, .size, "up"),
      round_choose(.x$ymax, .size, "up"))
  }
  .x
}

#' @export
extract_lower_left <- function(.data, .grp = L2) {
  .data |>
    sf::st_coordinates() |>
    tibble::as_tibble() |>
    dplyr::group_by( {{ .grp }} ) |>
    dplyr::arrange(X, Y) |>
    dplyr::slice_head() |>
    dplyr::ungroup() |>
    dplyr::rename_with(tolower)
}

#' @export
extract_lower_left_2 <- function(.data, .grp = ruta) {
  .data |>
    sf::st_cast("POINT") |>
    dplyr::group_by( {{ .grp }} ) |>
    dplyr::slice_head() |>
    dplyr::ungroup()
}

#' @export
grid_filter <- function(
    data,
    filter_col,
    filter_value,
    grid_size,
    .fn = eagles::sweref99_index) {
  sf::st_make_grid(
    data |>
      dplyr::filter( {{ filter_col}} == filter_value) |>
      sf::st_bbox() |>
      eagles::st_bbox_round(.size = grid_size),
    cellsize = grid_size) |>
    sf::st_sf(geometry = _) |>
    {(\(.) mutate(
      .,
      ruta = .fn(st_centroid(x = .), .grid_size = grid_size))
    )}() |>
    eagles::st_filter(
      sf::st_union(
        data |>
          dplyr::filter( {{ filter_col }} == filter_value)
      )
    )
}

#' @export
gdaltindex <- function(file_name, file_list) {

  file_conn <- base::file(glue::glue("{file_name}.txt"))
  base::writeLines(file_list, file_conn)
  base::close(file_conn)

  base::system(
    command = "cmd.exe",
    input = glue::glue("gdaltindex -f GPKG {file_name}.gpkg --optfile {file_name}.txt"),
    show.output.on.console = TRUE)

  base::unlink(glue::glue("{file_name}.txt"))
}

#' @export
layout_grid_size <- function(
    map_frame_x = 37.9,
    map_frame_y = 27.2,
    map_scale = 50000,
    overlap = 1.1,
    round = FALSE) {

  layout_scale <- map_scale / overlap

  layout_scale <- dplyr::case_when(
    round == TRUE ~ base::round(layout_scale, 0),
    TRUE ~ layout_scale)

  overlap <- map_scale / layout_scale

  delta_x <- map_frame_x / (100 * overlap / map_scale)
  delta_y <- map_frame_y / (100 * overlap / map_scale)

  # Should output be data.frame instead of a vector, c()?
  c(
    map_scale = map_scale,
    layout_scale = layout_scale,
    overlap = overlap,
    delta_x = delta_x,
    delta_y = delta_y
  )
}

find_page_number <- function(.data, row_offset, col_offset, page_variable) {
  base::with(.data, {
    inds <- base::match(
      base::paste(grid_row + row_offset, grid_column + col_offset),
      base::paste(grid_row, grid_column)
    )
    .data[inds, page_variable]
  })
}

#' @export
add_grid_neighbours <- function(
    map_grid,
    direction = c("s-n", "n-s"),
    page_variable = "PageNumber",
    sep = "_",
    add_neighbours = TRUE) {

  direction <- base::match.arg(direction)

  # Add bounding box for each grid cell,
  # then calculate extent in x and y-direction
  map_grid <- map_grid |>
    mutate(
      purrr::map_dfr(
        base::seq_len(base::nrow(map_grid)),
        \(x) sf::st_bbox(map_grid[x,])[1:4]
      ),
      dx = xmax - xmin,
      dy = ymax - ymin
    )

  # Calculate grid rows and columns
  if (direction == "s-n") {
    map_grid <- map_grid |>
      dplyr::mutate(
        grid_row = base::round( (((ymin - min(ymin)) / dy) + 1), 0),
        grid_column = base::round( (((xmin - min(xmin)) / dx) + 1), 0)
      )
  } else if (direction == "n-s") {
    map_grid <- map_grid |>
      dplyr::mutate(
        grid_row = base::round( (((max(ymax) - ymax) / dy) + 1), 0),
        grid_column = base::round( (((xmin - min(xmin)) / dx) + 1), 0)) |>
      dplyr::arrange(grid_row, grid_column)
  }

  # Add row number variable
  map_grid <- map_grid |>
    dplyr::mutate(
      {{ page_variable }} := dplyr::row_number()
    )

  if (add_neighbours) {

    .d <- map_grid |>
      sf::st_drop_geometry() |>
      as.data.frame()

    if (direction == "s-n") {
      row_offset <- 1
      col_offset <- 1
    } else if (direction == "n-s") {
      row_offset <- -1
      col_offset <- 1
    }

    map_grid <- map_grid |>
      {\(.) mutate(
        .,
        "{page_variable}{sep}NW" := find_page_number(.d, +row_offset, -col_offset, page_variable),
        "{page_variable}{sep}N"  := find_page_number(.d, +row_offset, 0          , page_variable),
        "{page_variable}{sep}NE" := find_page_number(.d, +row_offset, +col_offset, page_variable),
        "{page_variable}{sep}W"  := find_page_number(.d, 0          , -col_offset, page_variable),
        "{page_variable}{sep}E"  := find_page_number(.d, 0          , +col_offset, page_variable),
        "{page_variable}{sep}SW" := find_page_number(.d, -row_offset, -col_offset, page_variable),
        "{page_variable}{sep}S"  := find_page_number(.d, -row_offset, 0          , page_variable),
        "{page_variable}{sep}SE" := find_page_number(.d, -row_offset, +col_offset, page_variable)
      )
      }()

    map_grid <- map_grid |>
      relocate(geometry, .after = last_col())
    # attr(map_grid, "sf_column")

    # Convert numeric values to text, and replace NA with "empty string"
    # When and why is this? Dynamic text in ArcGIS or QGIS?

    # map_grid <- map_grid |>
    #   dplyr::mutate(
    #     dplyr::across(
    #       tidyselect::starts_with(page_variable), \(x) as.character(x) |> replace_na("")
    #     )
    #   )
  }

  map_grid
}

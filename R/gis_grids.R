# Q: Option to generate row_number and col_number?
# Har något sådant i add_grid_neighbours().

# grid_polygons generates a grid
# requires: sf
#' @export
grid_polygons <- function(x_range, y_range, delta_x, delta_y, crs, order = "clockwise") {

	n_x <- (max(x_range) - min(x_range)) / delta_x
	n_y <- (max(y_range) - min(y_range)) / delta_y

	grid_corners <- expand.grid(
		x = seq(min(x_range), by = delta_x, length.out = n_x),
		y = seq(min(y_range), by = delta_y, length.out = n_y))

	m <- map2(
	  grid_corners$x, grid_corners$y,
	  ~ grid_cell(.x, .y, delta_x = delta_x, delta_y = delta_y, order = order))

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

  direction <- match.arg(direction)

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
    list(
      matrix(xy_vec, nrow = 5, ncol = 2, byrow = TRUE)))
}
# grid_cell(1300000, 6100000, 50000, 50000, direction = "clockwise")
# grid_cell(1300000, 6100000, 50000, 50000, direction = "counter-clockwise")

#' @export
grid_parms <- function(
    delta_x, delta_y,
    min_x = 200000, max_x = 1000000,
    min_y = 6100000, max_y = 7700000) {

  n_x <- ceiling((max_x - min_x) / delta_x)
  x <- min_x + (0:n_x * delta_x)

  n_y <- ceiling((max_y - min_y) / delta_y)
  y <- min_y + (0:n_y * delta_y)

  list(delta_x = delta_x, n_x = n_x, x = x,
       delta_y = delta_y, n_y = n_y, y = y)
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
    .x <- st_bbox(.x)
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
    st_coordinates() |>
    as_tibble() |>
    group_by({{.grp}}) |>
    arrange(X, Y) |>
    slice_head() |>
    ungroup() |>
    rename_with(tolower)
}

#' @export
extract_lower_left_2 <- function(.data, .grp = ruta) {
  .data |>
    st_cast("POINT") |>
    group_by({{ .grp }}) |>
    slice_head() |>
    ungroup()
}

#' @export
gdaltindex <- function(file_name, file_list) {

  file_conn <- file(glue("{file_name}.txt"))
  writeLines(file_list, file_conn)
  close(file_conn)

  system(
    command = "cmd.exe",
    input = glue("gdaltindex -f GPKG {file_name}.gpkg --optfile {file_name}.txt"),
    show.output.on.console = TRUE)

  unlink(glue("{file_name}.txt"))
}

#' @export
layout_grid_size <- function(
    map_frame_x = 37.9,
    map_frame_y = 27.2,
    map_scale = 50000,
    overlap = 1.1,
    round = FALSE) {

  layout_scale <- map_scale / overlap

  layout_scale <- case_when(
    round == TRUE ~ round(layout_scale, 0),
    TRUE ~ layout_scale)

  overlap <- map_scale / layout_scale

  delta_x <- map_frame_x / (100 * overlap / map_scale)
  delta_y <- map_frame_y / (100 * overlap / map_scale)

  c(
    map_scale = map_scale,
    layout_scale = layout_scale,
    overlap = overlap,
    delta_x = delta_x,
    delta_y = delta_y)
}

#' @export
add_grid_neighbours <- function(
    map_grid,
    direction = c("s-n", "n-s"),
    page_variable = "PageNumber",
    sep = "_",
    add_neighbours = TRUE) {

  direction <- match.arg(direction)

  map_grid <- map_grid %>%
    bind_cols(
      map_dfr(seq_len(nrow(map_grid)), ~ st_bbox(map_grid[.x,])[1:4]))

  map_grid <- map_grid %>%
    mutate(
      dx = xmax - xmin,
      dy = ymax - ymin)

  if (direction == "s-n") {
    map_grid <- map_grid %>%
      mutate(
        grid_row = round( (((ymin - min(ymin)) / dy) + 1), 0),
        grid_column = round( (((xmin - min(xmin)) / dx) + 1), 0))
  } else if (direction == "n-s") {
    map_grid <- map_grid %>%
      mutate(
        grid_row = round( (((max(ymax) - ymax) / dy) + 1), 0),
        grid_column = round( (((xmin - min(xmin)) / dx) + 1), 0)) %>%
      arrange(grid_row, grid_column)

  }

  map_grid <- map_grid %>%
    mutate({{page_variable}} := row_number())

  if (add_neighbours) {
    # Keep only attributes, as a data frame.
    # Do NOT convert to tibble, since subsetting a tibble and data frame works differently.
    # The syntax used here, df[test_expression, extract_variable] is not equal to
    # df[test_expression, ]$extract_variable if df is a tibble.
    .d <- map_grid %>%
      st_drop_geometry()

    if (direction == "s-n") {
      row_offset <- 1
      col_offset <- 1 }
    else if (direction == "n-s") {
      row_offset <- -1
      col_offset <- 1
    }

    .m <- lapply(seq_len(nrow(.d)), function(i) {
      .p <- with(.d, {
        list(
          NW = .d[grid_row == grid_row[i] + row_offset & grid_column == grid_column[i] - col_offset, page_variable],
          N  = .d[grid_row == grid_row[i] + row_offset & grid_column == grid_column[i], page_variable],
          NE = .d[grid_row == grid_row[i] + row_offset & grid_column == grid_column[i] + col_offset, page_variable],
          W  = .d[grid_row == grid_row[i] & grid_column == grid_column[i] - col_offset, page_variable],
          E  = .d[grid_row == grid_row[i] & grid_column == grid_column[i] + col_offset, page_variable],
          SW = .d[grid_row == grid_row[i] - row_offset & grid_column == grid_column[i] - col_offset, page_variable],
          S  = .d[grid_row == grid_row[i] - row_offset & grid_column == grid_column[i], page_variable],
          SE = .d[grid_row == grid_row[i] - row_offset & grid_column == grid_column[i] + col_offset, page_variable])
      })
      .p[which(lengths(.p) == 0)] <- NA
      .p
    })

    map_grid <- map_grid %>%
      bind_cols(
        map_dfr(.m, bind_rows) %>%
          rename_with(.cols = everything(), function(x) {str_c(page_variable, x, sep = sep)}))

    # Convert numeric values to text, and replace NA with "empty string"
    # When and why is this step necessary?

    # map_grid <- map_grid %>%
    #   mutate(across(str_c(page_variable, "NW", sep = sep):str_c(page_variable, "SE", sep = sep), ~ as.character(.x))) %>%
    #   mutate(across(str_c(page_variable, "NW", sep = sep):str_c(page_variable, "SE", sep = sep), ~ replace_na(.x, "")))
  }
  map_grid
}

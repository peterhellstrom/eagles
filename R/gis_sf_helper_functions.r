# Source: https://github.com/r-spatial/sf/issues/231
#' @export
sfc_as_cols <- function(x, geometry, names = c("x","y"), drop_geometry = FALSE) {
  if (missing(geometry)) {
    geometry <- sf::st_geometry(x)
  } else {
    geometry <- rlang::eval_tidy(enquo(geometry), x)
  }
  stopifnot(inherits(x,"sf") && inherits(geometry,"sfc_POINT"))
  ret <- sf::st_coordinates(geometry)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  x <- dplyr::bind_cols(x,ret)
  if (drop_geometry) {
    x <- x |>
      sf::st_drop_geometry() |>
      dplyr::as_tibble()
  }
  x
}

#' @export
st_add_geom_column <- function(.x, coords, crs = 3006) {
  .x |>
    st_as_sf(coords = {{coords}}, crs = crs) |>
    st_geometry()
}

#' @export
st_add_geom_column_round <- function(.x, .g, .g_names, centroid = TRUE, crs = 3006) {

  for (i in seq_along(.g)) {
    .x_coords <- st_coordinates(.x) %>%
      as_tibble()

    .x_coords_round <- .x_coords %>%
      mutate(across(X:Y, ~ round_coords(., .g[[i]], centroid)))

    .x_col <- st_as_sf(.x_coords_round, coords = c("X", "Y"), crs = crs) %>%
      st_geometry()

    .x[[.g_names[i]]] <- .x_col
  }
  .x
}

#' @export
st_extract_pt_coords <- function (.x) {
  # Extract coordinates if input is an sf-object
  if (class(.x)[1] == "sf") {
    # Check if input geometry type is point, if not convert to point geometry type by
    # extracting centroid. This may not be the appropriate behavior?
    if (!inherits(st_geometry(.x), "sfc_POINT")) {
      .x <- st_centroid(.x)
    }
    crs <- sf::st_crs(.x)$epsg
    .xy <- sf::st_coordinates(.x)
    .y <- .xy[,"Y"]
    .x <- .xy[,"X"]
    list(.x = .x, .y = .y)
  }
}

# Todo: check function in rmapshaper, e.g. ms_erase and ms_dissolve
# Note check the erase function rmapshaper::ms_erase
#' @export
st_erase <- function(x, y) st_difference(x, st_union(st_combine(y)))

#' @export
st_filter = function(.x, .y, .pred = st_intersects) {
  # this is equal to .x[.y, op = st_intersects]
  # check that dplyr is loaded, then
  filter(.x, lengths(.pred(.x, .y)) > 0)
}

#' @export
list_layers <- function(x) {
  tibble::tibble(
    name = x$name,
    geomtype = unlist(x$geomtype),
    driver = x$driver,
    features = x$features,
    fields = x$fields)
}

#' @export
# Note: st_layers does not include feature dataset for FileGDBs
st_layers_tibble <- function(.x) {
  sf::st_layers(.x) |> list_layers()
}

#' @export
gpkg_contents <- function(.x) {

  x <- st_layers_tibble(.x)

  con <- DBI::dbConnect(RSQLite::SQLite(), .x)

  x_contents <- DBI::dbGetQuery(con, 'SELECT * FROM gpkg_contents') |>
    tibble::as_tibble() |>
    dplyr::mutate(last_change = readr::parse_datetime(last_change))

  DBI::dbDisconnect(con)

  x <- x |>
    dplyr::left_join(x_contents, by = c("name" = "table_name")) |>
    dplyr::arrange(name)
  x

}

#' @export
copy_layer <- function(.from_dsn, .from_layer, .to_dsn, .to_layer = .from_layer, ...) {
  x <- st_read(.from_dsn, .from_layer)
  st_write(x, .to_dsn, .to_layer, ...)
}

#' @export
copy_layer_arc <- function(.from_dsn, .from_layer, .to_dsn, .to_layer = .from_layer,
                           .feature_dataset = NULL, overwrite = TRUE, validate = TRUE) {

  x <- st_read(.from_dsn, .from_layer)
  if (is.null(.feature_dataset) ) {
    p <- file.path(.to_dsn, .to_layer) }
  else {
    p <- file.path(.to_dsn, .feature_dataset, .to_layer)
  }

  arcgisbinding::arc.write(p, x, overwrite = overwrite, validate = validate)
  # arc.write(
  #   p,
  #   x,
  #   shape_info = list(type = 'Polygon', hasZ = FALSE, WKID = 3006),
  #   overwrite = TRUE, validate = TRUE)
}

# Edit epsg (SRID) to show correctly, st_write can not write the epsg (SRID) because this is apparently done in GDAL
# and there's not a 1:1 relationship between epsg (SRID) and the proj4string. The proj4string is written correctly to the gpkg,
# but SRID is not.
# Check two tables; gpkg_spatial_ref_sys and gpkg_geometry_columns, the SRSID must exist in gpkg_spatial_ref_sys before gpkg_geometry_columns can be updated
# use sqlite-builtin function to set correct SRID and proj4string:
# Generates error message:
# "UNIQUE constraint failed: gpkg_spatial_ref_sys.srs_id"
# but executes as expected anyway....
# Check EPSG (SRID) & proj4string: both should now be correct, epsg (SRID) should not be NA
# Also try load in QGIS: check BOTH Layer properties from the Browser and Database > DB Manager
# Try to load in ArcGIS

# See more info:
# https://github.com/r-spatial/sf/issues/786

# Check if value already exists?

#' @export
set_epsg_gpkg <- function(dsn, layer, epsg = 3006, delete_srid = NULL) {
	gdalUtils::ogrinfo(dsn, dialect = "sqlite", sql = glue::glue("SELECT gpkgInsertEpsgSRID({epsg})"))
	gdalUtils::ogrinfo(dsn, dialect = "sqlite", sql = glue::glue("UPDATE gpkg_geometry_columns SET srs_id = {epsg} WHERE table_name LIKE '{layer}%';"))
	gdalUtils::ogrinfo(dsn, dialect = "sqlite", sql = glue::glue("UPDATE gpkg_contents SET srs_id = {epsg} WHERE table_name LIKE '{layer}%';"))
	if (!is.null(delete_srid)) {
		# Delete SRSID posts created "by" st_write [or rather GDAL]
		gdalUtils::ogrinfo(dsn, dialect = "sqlite", sql = glue::glue("DELETE FROM gpkg_spatial_ref_sys WHERE srs_id LIKE {delete_srid}%;"))
	}
}

#' @export
stdh_cast_substring <- function(x, to = "MULTILINESTRING") {
  ggg <- st_geometry(x)

  if (!unique(st_geometry_type(ggg)) %in% c("POLYGON", "LINESTRING")) {
    stop("Input should be  LINESTRING or POLYGON")
  }
  for (k in 1:length(st_geometry(ggg))) {
    sub <- ggg[k]
    geom <- lapply(
      1:(length(st_coordinates(sub)[, 1]) - 1),
      function(i)
        rbind(
          as.numeric(st_coordinates(sub)[i, 1:2]),
          as.numeric(st_coordinates(sub)[i + 1, 1:2])
        )
    ) %>%
      st_multilinestring() %>%
      st_sfc()

    if (k == 1) {
      endgeom <- geom
    }
    else {
      endgeom <- rbind(endgeom, geom)
    }
  }
  endgeom <- endgeom %>% st_sfc(crs = st_crs(x))
  if (class(x)[1] == "sf") {
    endgeom <- st_set_geometry(x, endgeom)
  }

  if (to == "LINESTRING") {
    endgeom <- endgeom %>% st_cast("LINESTRING")
  }
  return(endgeom)
}

#' @export
split_holes <- function(.data, .crs = 3006, .area_unit = "km^2") {
  # Extract coordinates
  out <- st_coordinates(.data) %>%
    as_tibble() %>%
    group_by(L1)

  if (n_groups(out) > 0) {

    out <- out %>%
      # Split into separate lists
      group_split() %>%
      # Convert to sf-object, polygons
      map(~ select(., X, Y) %>%
            as.matrix() %>%
            list(.) %>%
            st_polygon() %>%
            st_sfc(crs = .crs) %>%
            st_sf())

    out <- out %>%
      # Bind all polygons together
      bind_rows() %>%
      # Calculate area of each polygon
      mutate(Area = st_area(.)) %>%
      # Set units
      mutate(Area = units::set_units(Area, .area_unit, mode = "standard")) %>%
      # Sort in descending order based on area
      arrange(desc(Area))

    out
  }
}

# convert gpx-data
# output:
# waypoints: data.frame, extensions are included
# tracks: nested list (data frame within lists)
# routes: list with data frames

# It is worth mentioning that the perhaps easiest [and fastest way] way to convert
# gpx to shp "as-is", is from the command line in OSGeo4W, for instance:
# ogr2ogr -f "ESRI Shapefile" Flygtracks_var_tracks.shp Flygtracks_var.gpx tracks
# ogr2ogr -f "GPKG" Flygdata.gpkg Flygtracks_var.gpx tracks -nln "tracks"
# ogr2ogr -f "GPKG" -update Flygdata.gpkg Flygtracks_var.gpx track_points -nln "track_points"

# ToDo: write description what this function actually does in comparison to st_read

#' @export
gpx_to_sf <- function(
  x,
  data_type = c("route_points", "track_points", "waypoints"),
	bearing = FALSE,
  convert_time = TRUE,
  current_time_zone = TRUE,
	lines = FALSE
) {

	data_type <- match.arg(data_type)

	# Calculate bearing
	if (bearing) {
	  x <- x |>
	    tidyr::nest() |>
	    dplyr::mutate(
	      b = purrr::map(data, bearing_df)
	    ) |>
	    tidyr::unnest(cols = c(data, b)) |>
	    sf::st_sf()
	}

	# Convert time
	if (convert_time) {
		if ("time" %in% names(x)) {
			x <- x |>
				#dplyr::ungroup() %>%
				mutate(
				  time = lubridate::ymd_hms(
				    time,
				    tz = Sys.timezone(location = current_time_zone)
				  )
				)
				#dplyr::group_by(id_field)
		}
	}

	# deal with waypoints layer?
	if (lines) {
		if (data_type == "track_points") {
		  id_field <- "track_fid"
			x <- x |>
				sf::summarize(
					n_points = dplyr::n(),
					start_time = base::min(time),
					end_time = base::max(time),
					do_union = FALSE
				) |>
				dplyr::mutate(
				  sort_field = dplyr::row_number()
				) |>
				sf::st_cast("LINESTRING") |>
				dplyr::mutate(
					track_length = st_length(.),
					track_year = lubridate::year(start_time),
				) |>
				dplyr::select(
				  all_of(id_field),
				  sort_field, track_year,
				  start_time, end_time,
				  n_points, track_length
				)

		} else if (data_type == "route_points") {
		  id_field <- "route_fid"
			x <- x |>
				sf::summarize(
				  n_points = dplyr::n(),
				  do_union = FALSE
				) |>
				dplyr::mutate(
				  sort_field = row_number()
				) |>
				sf::st_cast("LINESTRING") |>
				dplyr::mutate(route_length = st_length(.)) |>
				dplyr::select(
				  all_of(id_field),
				  sort_field, n_points, route_length
				)
		}
	}

	x
}

#' @export
gps_route_points <- function(.x, crs = 3006) {
  gpx_to_sf(
    sf::read_sf(dsn = .x, layer = "route_points") |>
      dplyr::group_by(route_fid),
    lines = FALSE,
    bearing = TRUE
  ) |>
    dplyr::left_join(
      sf::read_sf(dsn = .x, layer = "routes") |>
        dplyr::mutate(route_fid = dplyr::row_number() - 1) |>
        sf::st_drop_geometry() |>
        dplyr::select(route_fid, route_name = name),
      dplyr::join_by(route_fid)
    ) |>
    dplyr::select(
      route_name, route_fid, time,
      name:desc, sym, type,
      bearing:c_dist, geometry
    ) |>
    sf::st_transform(crs = crs)
}

#' @export
gps_route_lines <- function(.x, crs = 3006) {
  gpx_to_sf(
    sf::read_sf(dsn = .x, layer = "route_points") |>
      dplyr::group_by(route_fid),
    data_type = "route_points",
    lines = TRUE
  ) |>
    dplyr::left_join(
      sf::read_sf(dsn = .x, layer = "routes") |>
        dplyr::mutate(route_fid = dplyr::row_number() - 1) |>
        sf::st_drop_geometry() |>
        dplyr::select(route_fid, name),
      dplyr::join_by(route_fid)
    ) |>
    dplyr::rename(route_name = name) |>
    dplyr::relocate(
      route_name,
      .before = route_fid
    ) |>
    sf::st_transform(crs = crs)
}

#' @export
gps_track_lines <- function(.x, crs = 3006, convert_time = FALSE) {
  gpx_to_sf(
    sf::read_sf(dsn = .x, layer = "track_points") |>
      group_by(track_fid),
    data_type = "track_points",
    lines = TRUE,
    convert_time = convert_time
  ) |>
    dplyr::left_join(
      sf::read_sf(dsn = .x, layer = "tracks") |>
        dplyr::mutate(track_fid = dplyr::row_number() - 1) |>
        sf::st_drop_geometry() |>
        dplyr::select(track_fid, name),
      dplyr::join_by(track_fid)
    ) |>
    dplyr::rename(track_name = name) |>
    dplyr::relocate(track_name, .before = track_fid) |>
    sf::st_transform(crs = crs)
}

# x is a data frame with gpx-data
# This function is used internally by gpx_to_sf, but could also
# be used outside that function. Out-commented sections of the code
# should be developed for "independent" use!

#' @export
bearing_df <- function(x) {

  if (dplyr::is_grouped_df(x)) {
    return(dplyr::do(x, bearing_df(.)))
  }

	xy <- st_coordinates(x)
	colnames(xy) <- c("lon", "lat")

	xy_b <- geosphere::bearing(xy) # geographic

	xy_m <- oce::magneticField(
	  lon = xy[,"lon"],
	  lat = xy[,"lat"],
	  Sys.Date())$declination

	xy_b_m <- xy_b - xy_m

	xy_angle <- dplyr::if_else(xy_b < 0, 360 - abs(xy_b), xy_b)
	xy_angle_m <- dplyr::if_else(xy_b_m < 0, 360 - abs(xy_b_m), xy_b_m)
	xy_dist <- c(geosphere::distHaversine(xy), NA)
	xy_cum_dist <- base::cumsum(xy_dist)

	xy_data <- data.frame(
		bearing = xy_b,
		angle = xy_angle,
		bearing_m = xy_b_m,
		angle_m = xy_angle_m,
		dist = xy_dist,
		c_dist = xy_cum_dist
	)

	# Remove last values of each route, i.e. do not bind values not belonging to the same route!
	# brks <- as.numeric(cumsum(sapply(x[[data_type]], nrow)))
	# Another alternativ option is:
	# brks <- which(tail(xy_data$id_field, -1) != head(xy_data$id_field, -1))
	# xy_data[brks, tail(names(xy_data), 3)] <- NA
	xy_data
}

# x %>% bearing_df()
# x %>% nest() %>% mutate(b = map(data, bearing_df)) %>% unnest(cols = c(data, b)) %>% st_sf()
# x %>% ungroup() %>% mutate(z = bearing_df(.))
# x %>% mutate(z = bearing_df(.))

# Other options for geodetic calculations:
# Packages: geosphere, geodist, lwgeom
# sf has a very slow st_distance function, that calculates a "dense" matrix
# lwgeom is developed together with (?) sf, but functions does not appear to be "pipeable"

# Some examples
# https://gis.stackexchange.com/questions/289608/calculating-distances-between-consecutive-points-using-r
# ?sf::st_distance
# geodist::geodist(st_coordinates(x), sequential = TRUE, measure = "haversine")
# geosphere::distHaversine(st_coordinates(x))
# lwgeom::st_geod_distance(x, x)
# lwgeom::st_geod_azimuth(x)

# Export route to simple text file
# use fields name, angle, and dist to generate a string
# NOTE: this function generates a single string, does
# not group into individual routes!
#' @export
rte_to_txt <- function(x, file_name) {
	rte_text <- with(x, paste0(name, " >[", round(angle), "°, ", round(dist/1000, 1), " km]"))
	sink(paste0(file_name, ".txt", sep = ""))
	cat(paste(unlist(t(rte_text)), collapse="> "), "\n")
	sink()
}

# x is a character string in format: "YYYY-MM-DDTHH:MM:SSZ", default format in gpx-files
# gpx-time is given in standard "Zulu"-time
# This function converts the time string to class POSIXlt
#' @export
gpx_time_to_time <- function(x, tz = Sys.timezone(location = TRUE)) {
	d <- gsub(x = gsub(x = x, pattern = "T", replacement = " "), pattern = "Z", replacement = "")
	d <- as.POSIXct(strptime(d, format = "%Y-%m-%d %H:%M:%S"), tz = "UTC")
	d <- as.POSIXlt(d, tz = tz)
	d
}

# UTC = Coordinated Universal Time
# gpx_time_to_time("2018-04-18T06:40:57Z")

#' @export
st_read_gpx <- function(
    dsn,
    layer = "waypoints",
    crs = 3006,
    keep_only_necessary = TRUE) {

  .x <- sf::read_sf(dsn, layer = layer)

  if (!is.null(crs)) {
    .x <- sf::st_transform(.x, crs)
  }

  if (keep_only_necessary) {
    .x <- .x |>
      dplyr::select(name, sym, type, cmt, desc, ele, time)
  }
  .x
}

#' @export
trk_preview <- function(.x) {
  leaflet(
    data = sf::st_transform(.x, crs = 4326)
  ) |>
    leaflet::addTiles() |>
    leaflet::addPolylines()
}

#' @export
rte_arrows <- function(x_lines, x_points) {
  dplyr::bind_cols(
    stdh_cast_substring( {{ x_lines }}, "LINESTRING") |>
      dplyr::select(geometry),
    {{ x_points }} |>
      dplyr::slice(1:(n()-1)) |>
      sf::st_drop_geometry()
  ) |>
    sf::st_centroid()
}

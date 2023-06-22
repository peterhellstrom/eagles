# General steps, seven-parameter transformation of projected coordinates
# 1) grid_to_geodetic
# 2) geodetic_to_cartesian
# 3) cartesian_proj
# 4) cartesian_to_geodetic
# 5) geodetic_to_grid

# INCOMPLETE, keep for future reference
# USE sf::st_transform instead, or direct transformation for SWEREF99 operations

# 2) Geodetic coordinates to cartesian coordinates
# h <- ellipsoid height
geodetic_to_cartesian <- function(latitude, longitude, h, epsg) {
	parameters <- as.list(lm_set_parameters(epsg))
	with(parameters, {
		latitude_rad <- deg_to_rad(latitude)
		longitude_rad <- deg_to_rad(longitude)
		
		e2 <- f * (2 - f)
		Nprim <- a / sqrt(1 - e2 * sin(latitude_rad) * sin(latitude_rad))
		
		x <- (Nprim + h) * cos(latitude_rad) * cos(longitude_rad)
		y <- (Nprim + h) * cos(latitude_rad) * sin(longitude_rad)
		z <- (Nprim * (1 - e2) + h) * sin(latitude_rad)
		data.frame(x = x, y = y, z = z)
	})
}

# Source?
# Differs slightly from GDAL
rotation_parameters <- function(transform_to) {	
	#enheter: c(rep("m", 3), rep("rad", 3), "mm/km")
	label_params <- c("delta_x", "delta_y", "delta_z", "omega_x", "omega_y", "omega_z", "delta")
	if (transform_to == "rt90") {
		# Transformation from SWEREF 99 and WGS 84 to RT 90
		params <- c(-414.0978567149, -41.3381489658, -603.0627177516, -0.8550434314, 2.1413465185, -7.0227209516, 0)
	} else if (transform_to == "sweref99") {
		# Transformation from RT90 to SWEREF 99
		params <- c(414.1055246174, 41.3265500042, 603.0582474221, 0.8551163377, -2.1413174055, 7.0227298286, 0)
	}
	names(params) <- label_params
	params
}

# 3) General formula, transformation parameters can be tricky though...
cartesian_proj <- function(x, y, z, parameters, rot_rad = 2 * pi / (3600 * 360)) {
	with(as.list(parameters), {
		omega_x <- omega_x * rot_rad
		omega_y <- omega_y * rot_rad
		omega_z <- omega_z * rot_rad

		r_z <- matrix(c(cos(omega_z), sin(omega_z), 0, -sin(omega_z), cos(omega_z), 0, 0, 0, 1), byrow = TRUE, nrow = 3, ncol = 3)
		r_y <- matrix(c(cos(omega_y), 0, -sin(omega_y), 0, 1, 0, sin(omega_y), 0, cos(omega_y)), byrow = TRUE, nrow = 3, ncol = 3)
		r_x <- matrix(c(1, 0, 0, 0, cos(omega_x), sin(omega_x), 0, -sin(omega_x), cos(omega_x)), byrow = TRUE, nrow = 3, ncol = 3)
		r <- r_z %*% r_y %*% r_x

		out <- c(delta_x, delta_y, delta_z) + (1 + delta) * r %*% c(x, y, z)
		out <- as.data.frame(t(out))
		names(out) <- c("x", "y", "z")
		out
	})
}

# 4) Cartesian coordinates to geodetic coordinates
cartesian_to_geodetic <- function(x, y, z, epsg) {
	parameters <- as.list(lm_set_parameters(epsg))
	with(parameters, {
		longitude_rad <- atan(y / x)
		longitude <- rad_to_deg(longitude_rad)
		
		e2 <- f * (2 - f)
		p <- sqrt(x^2 + y^2)
		tantheta <- z / (p * sqrt(1 - e2))
		theta <- atan(tantheta)
		latitude_rad <- atan((z + ((a * e2/sqrt(1 - e2)) * sin(theta)^3)) / (p - a * e2 * cos(theta)^3))
		latitude <- rad_to_deg(latitude_rad)
		Nprim <- a/sqrt(1 - e2 * sin(latitude_rad) * sin(latitude_rad))
		h <- (p / cos(latitude_rad)) - Nprim
		data.frame(latitude = latitude, longitude = longitude, height = h)
	})
}
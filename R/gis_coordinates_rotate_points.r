# Rotate coordinates
# Last updated February 24th 2012

# Useful information about coordinate rotation
# http://en.wikipedia.org/wiki/Rotation_matrix

# About the functions:
# Rotation is usually performed counter-clockwise
# The center of rotations is always the origin

# Rotation of one coordinate pair: use rotate_pt
# Rotation of several coordinate pairs: use rotate_pts

# x, y are coordinates in ...
# x.origo, y.origo coordinates of origin in (metric) projected coordinate system e.g. RT90.
# angle in degrees. Positive = counter-clockwise rotation, negative = clock-wise rotation
# The user can select two output formats:
# "short" returns only the rotated coordinates, "long" returns a list with angle, rotation matrix, unrotated and rotated coordinates.

#' @export
rotate_pt <- function(x, y, angle, x.origo = NULL, y.origo = NULL, output = "short") {

	# Convert the rotation angle from degrees to radians
	# If the angle is positive = counter-clockwise rotation
	# If the angle is negative = clockwise rotation
	theta <- angle*pi / 180
	# Set up the rotation matrix
	R <- matrix(c(
		cos(theta), -sin(theta),
		sin(theta), cos(theta)),
		ncol = 2, nrow = 2, byrow = TRUE)
	# Create a column matrix with two rows, and store the original x and y coordinates
	xy <- matrix(c(x,y), nrow = 2, ncol = 1)
	# Multiplication of rotation matrix and original position vector
	xy.prim <- R %*% xy
	# Store the rotated coordinates
	x.prim <- xy.prim[1,1]
	y.prim <- xy.prim[2,1]
	# If the user has supplied the x- AND y-coordinates, add them to the rotated coordinates.
	# If not, do nothing.
	if (!is.null(x.origo) & !is.null(y.origo)) {
		x.prim <- x.prim + x.origo
		y.prim <- y.prim + y.origo
	}

	# Create output (if == "long", send all output to a list; if =="short", send only the rotated coordinates)
	if (output == "long") {
		out <- list(angle = angle, theta = theta, R = R, x = x, y = y, x.prim = x.prim, y.prim = y.prim)
	}
	if (output == "short") {
		out <- c(x.prim, y.prim)
	}

	out
}

# Rotate many points with the same origo/coordinate system and angle.
# This functions calls rotate_pt, and reapeats the rotation procedure.
#' @export
rotate_pts <- function(x, y, angle, x.origo = NULL, y.origo = NULL, output = "short") {

	if (length(x) != length(y)) stop("x & y of unequal length")

	n <- length(x)
	if (length(angle) == 1) angle <- rep(angle,n)
	if (length(x.origo) == 1) x.origo <- rep(x.origo,n)
	if (length(y.origo) == 1) y.origo <- rep(y.origo,n)

	out <- sapply(1:n, function(i) rotatePt(x = x[i], y[i], angle = angle[i], x.origo = x.origo[i], y.origo = y.origo[i], output = output))
	out <- t(out)
	colnames(out) <- c("x", "y")
	out
}
